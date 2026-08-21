% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive Abrogation of Gender-Specific Qur'anic Verses
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies
 *
 * SUMMARY:
 *   This constraint story models the 'progressive abrogation' reading of the
 *   Qur'anic gender verses. This reading asserts that later, universal
 *   egalitarian principles in the Qur'an (e.g., 49:13 on universal human
 *   dignity) supersede earlier, gender-specific rules (e.g., 4:11, 2:282,
 *   4:34) via the principle of naskh (abrogation). This interpretation leads
 *   to a complete normative reversal, aiming for full legal parity for women
 *   and challenging traditional authority structures. The high extractiveness
 *   and suppression metrics reflect the nature of the *earlier
 *   gender-specific rules* as identified by this reading, which the principle
 *   of abrogation seeks to overcome.
 *
 * KEY AGENTS:
 *   - women_seeking_parity: Primary beneficiary (powerless/trapped) — bear extraction from traditional rules, gain from abrogation.
 *   - progressive_islamic_scholars: Secondary beneficiary (organized/identity_locked) — champion the reading, face high exit costs.
 *   - traditional_islamic_authorities: Primary payer/agenda_setter (institutional/identity_locked) — their authority is challenged, resist abrogation.
 *   - communities_bound_to_literal_reading: Secondary payer (organized/identity_locked) — perceive epistemic violence, resist change.
 *   - secular_gender_equality_advocates: Observer (organized/analytical) — support the reading from an external perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.85).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.9).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.85).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, scaffold).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive Abrogation of Gender-Specific Qur'anic Verses").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "islamic_jurisprudence/legal_hermeneutics/gender_studies").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).
narrative_ontology:has_sunset_clause(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '12c0a618-5ee1-4992-b3c3-05469f8f2adb').
narrative_ontology:cs_kernel_codification('12c0a618-5ee1-4992-b3c3-05469f8f2adb', fixed_text).
narrative_ontology:cs_authority_grounding('12c0a618-5ee1-4992-b3c3-05469f8f2adb', lineage).
narrative_ontology:cs_interpretation_layer_present('12c0a618-5ee1-4992-b3c3-05469f8f2adb').
narrative_ontology:cs_reading_relation('12c0a618-5ee1-4992-b3c3-05469f8f2adb', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('12c0a618-5ee1-4992-b3c3-05469f8f2adb', quranic_gender_verses__contextual_egalitarian, influences).
narrative_ontology:cs_axiom('12c0a618-5ee1-4992-b3c3-05469f8f2adb', foundational, quranic_egalitarianism_supersedes_particulars).
narrative_ontology:cs_axiom_status(quranic_egalitarianism_supersedes_particulars, holdable).
narrative_ontology:cs_axiom_grounding('12c0a618-5ee1-4992-b3c3-05469f8f2adb', quranic_egalitarianism_supersedes_particulars, deontological).
narrative_ontology:cs_axiom('12c0a618-5ee1-4992-b3c3-05469f8f2adb', foundational, naskh_applies_to_normative_trajectory).
narrative_ontology:cs_axiom_status(naskh_applies_to_normative_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('12c0a618-5ee1-4992-b3c3-05469f8f2adb', naskh_applies_to_normative_trajectory, conventional).
narrative_ontology:cs_reference_frame('12c0a618-5ee1-4992-b3c3-05469f8f2adb', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('12c0a618-5ee1-4992-b3c3-05469f8f2adb', contemporary_islamic_thought, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('12c0a618-5ee1-4992-b3c3-05469f8f2adb', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, women_seeking_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditional_islamic_authorities).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_reading).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stand to gain full legal and social parity if this reading is widely adopted and implemented. Currently bear the brunt of gender-specific rules and traditional interpretations.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, women_seeking_parity, beneficiary,
    powerless, generational, trapped, global).

% Champion this reading, providing intellectual and jurisprudential arguments for its adoption. Face significant professional and social costs, including ostracization and loss of institutional standing, for challenging traditional interpretations.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, progressive_islamic_scholars, beneficiary,
    organized, biographical, identity_locked, global).

% Their authority and institutional structures are largely predicated on literal, hierarchical interpretations of gender-specific verses. The progressive abrogation reading directly challenges and delegitimizes their established positions, forcing them to either adapt or resist.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditional_islamic_authorities, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, traditional_islamic_authorities, agenda_setter).

% Their social and religious identity is deeply intertwined with traditional, literal interpretations of gender-specific verses. This reading is perceived as an 'epistemic violence' that threatens their worldview and community cohesion.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_bound_to_literal_reading, payer,
    organized, generational, identity_locked, local).

% Observe and sometimes support the progressive abrogation reading as a means to achieve gender equality within Islamic frameworks, often from outside traditional religious institutions.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, secular_gender_equality_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, women_seeking_parity).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hermeneutical framework to reconcile seemingly contradictory Qur'anic verses, allowing for a consistent, egalitarian ethical framework that aligns with universal human dignity.
% TRANSFER_FUNCTION: Transfers normative authority from earlier, gender-specific verses to later, universal principles, effectively transferring rights and status from men to women, and delegitimizing traditional patriarchal interpretations.
% ABSENT_VOICES: Those who cannot conceive of an Islamic framework without traditional gender hierarchies, or those who fear the social disruption and perceived theological compromise of such a radical reinterpretation, are often marginalized or excluded from the discourse where this reading is advanced.
% DISAPPEARANCE_RATIONALE: If the principle of progressive abrogation vanished, the entire framework for Islamic gender ethics would be thrown into disarray. Without this interpretive tool, the tension between early and late Qur'anic verses would either revert to literal, hierarchical interpretations or push many towards secularism, fundamentally reorganizing Islamic legal and social thought.
% FOUNDING_PROBLEM: The apparent contradiction between early Qur'anic verses with gender-specific rules (e.g., regarding inheritance, testimony, male guardianship) and later verses emphasizing universal human dignity and equality (e.g., 49:13), leading to ethical dilemmas and perceived injustice in modern contexts.
% FOUNDING_PROBLEM_CORROBORATION: Progressive Muslim feminists, human rights organizations, and some interfaith dialogue initiatives corroborate the problem, highlighting the ethical and social challenges posed by traditional interpretations. Scholarly conferences and publications also attest to the ongoing nature of this hermeneutical challenge.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `base_extractiveness` is high (0.85, slowly decreasing) because this reading identifies the *superseded gender-specific rules* as inherently highly extractive from women. The `suppression` is very high (0.90, increasing) reflecting the deep entrenchment of traditional interpretations within legal systems and social norms, requiring immense effort to challenge and change. `Theater_ratio` is low (0.10, slowly increasing) as the traditional rules are genuinely enforced, though some institutions may begin to perform adherence to egalitarian principles without full structural change. `Accessibility_collapse` is high for women under traditional interpretations, while `resistance` is high from both those advocating for this reading and those opposing it. The `claimed_type` is 'scaffold' because this reading provides a transitional framework to move from a hierarchical to an egalitarian normative order, with a conceptual 'sunset' when full parity is achieved.
 *
 * PERSPECTIVAL GAP:
 *   The 'progressive abrogation' reading is experienced as a liberating force by women and progressive scholars, aiming to dismantle existing extraction. However, from the perspective of traditional authorities and communities, it is perceived as an attack on divine law and cultural identity, leading to a sense of loss and delegitimization. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Women seeking parity are full beneficiaries of this reading's application, as it aims to remove extraction from them. Progressive scholars are also beneficiaries, though they bear significant costs for advocating it. Traditional authorities and communities are targets, as their established power and identity structures are directly challenged and delegitimized by this reading. Secular advocates are observers, analyzing its impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not about a mandate that has atrophied, but rather about a hermeneutical principle used to *resolve* a perceived mandate (the earlier gender-specific rules) that is seen as having outlived its ethical justification. The classification as 'scaffold' prevents mislabeling it as a 'snare' (which it is not, as the principle itself is liberatory) or a 'piton' (as it is actively contested and transformative, not inert). The high extractiveness refers to the *target* of the abrogation, not the abrogation principle itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_reversal_feasibility,
    'Is a ''complete normative reversal'' via progressive abrogation practically achievable within diverse Islamic legal systems and communities, or will it remain an aspirational scholarly position?',
    'Empirical observation of legal reforms, fatwas, and social changes in Muslim-majority and minority contexts over several decades.',
    'If unachievable, the reading''s effective impact on extraction remains low, despite its theoretical potential. If widely adopted, it would lead to a significant reduction in extraction for women.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_reversal_feasibility, empirical, 'Feasibility of implementing progressive abrogation in practice.').

omega_variable(
    epistemic_violence_legitimacy,
    'Is the ''epistemic violence'' perceived by communities bound to literal readings a legitimate cost of normative progress, or an unacceptable imposition that undermines religious identity?',
    'Conceptual analysis of religious freedom, communal identity, and the ethics of theological reform, potentially informed by sociological studies of religious change.',
    'If deemed an unacceptable imposition, the ''payer'' role for these communities is amplified, and the overall social cost of this reading is higher. If deemed a necessary cost, the ''payer'' role is mitigated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_violence_legitimacy, preference, 'Ethical evaluation of the impact on traditional communities.').

omega_variable(
    scholarly_exit_cost_sustainability,
    'Are the ''high exit costs'' for scholars adopting this reading sustainable, or will they eventually deter further progressive scholarship, leading to a decline in the reading''s influence?',
    'Longitudinal study of academic careers, funding for progressive Islamic studies, and the formation of new institutional spaces for such scholarship.',
    'If unsustainable, the reading''s capacity to challenge traditional authority will diminish, potentially increasing the persistence of the superseded rules'' extraction. If new support structures emerge, its influence could grow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarly_exit_cost_sustainability, empirical, 'Sustainability of scholarly engagement with progressive abrogation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quranic_gender_verses__progressive_abrogation, theater_ratio, 20, 0.12).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.15).
narrative_ontology:measurement(qura_tr_t60, quranic_gender_verses__progressive_abrogation, theater_ratio, 60, 0.18).
narrative_ontology:measurement(qura_tr_t80, quranic_gender_verses__progressive_abrogation, theater_ratio, 80, 0.2).
narrative_ontology:measurement(qura_tr_t100, quranic_gender_verses__progressive_abrogation, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(qura_be_t20, quranic_gender_verses__progressive_abrogation, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(qura_be_t60, quranic_gender_verses__progressive_abrogation, base_extractiveness, 60, 0.82).
narrative_ontology:measurement(qura_be_t80, quranic_gender_verses__progressive_abrogation, base_extractiveness, 80, 0.81).
narrative_ontology:measurement(qura_be_t100, quranic_gender_verses__progressive_abrogation, base_extractiveness, 100, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(qura_su_t20, quranic_gender_verses__progressive_abrogation, suppression_requirement, 20, 0.91).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.92).
narrative_ontology:measurement(qura_su_t60, quranic_gender_verses__progressive_abrogation, suppression_requirement, 60, 0.93).
narrative_ontology:measurement(qura_su_t80, quranic_gender_verses__progressive_abrogation, suppression_requirement, 80, 0.94).
narrative_ontology:measurement(qura_su_t100, quranic_gender_verses__progressive_abrogation, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
