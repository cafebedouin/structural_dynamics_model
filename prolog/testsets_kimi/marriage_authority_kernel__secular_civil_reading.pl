% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Secular Civil Marriage Authority (Special Marriage Act 1954)
 *   domain: comparative_law/constitutional_pluralism
 *
 * SUMMARY:
 *   This constraint instantiates the secular_civil_reading of the contested
 *   marriage_authority_kernel in Indian law. It treats marriage and family
 *   law authority as derived from the Special Marriage Act 1954, a secular
 *   statute administered by civil courts and grounded in constitutional
 *   individual rights. The reading enables inter-religious marriage and
 *   claims the highest gender-equity quotient among the kernel's readings,
 *   but it simultaneously imposes social costs on individuals who exit
 *   community personal law. It coexists with four sibling readingsâHindu
 *   codified, Muslim Shariat, Christian canonical, and Parsi
 *   communalâwithin a plural legal order. Structurally, the constraint
 *   coordinates secular marriage registration while extracting authority from
 *   religious personal law boards and exposing interfaith couples to
 *   community ostracism.
 *
 * KEY AGENTS:
 *   - secular_state_authority: Agenda setter (institutional/mobile) â enacts SMA and asserts civil court supremacy
 *   - interfaith_couples: Dual-positioned beneficiary/payer (moderate/constrained) â gain legal recognition but bear social costs of community exit
 *   - women_seekers_equitable_rights: Beneficiary (moderate/constrained) â access gender-equitable adjudication unavailable under many personal laws
 *   - religious_personal_law_boards: Payer (organized/constrained) â lose exclusive authority over marriage for community members who opt out
 *   - constitutional_scholars: Observer (analytical/analytical) â analyze the tension between secular constitutionalism and legal pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.48).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.45).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Secular Civil Marriage Authority (Special Marriage Act 1954)").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'a0343102-5ce3-4842-9667-6eccb3607c6d').
narrative_ontology:cs_kernel_codification('a0343102-5ce3-4842-9667-6eccb3607c6d', formalized).
narrative_ontology:cs_authority_grounding('a0343102-5ce3-4842-9667-6eccb3607c6d', lineage).
narrative_ontology:cs_interpretation_layer_present('a0343102-5ce3-4842-9667-6eccb3607c6d').
narrative_ontology:cs_reading_relation('a0343102-5ce3-4842-9667-6eccb3607c6d', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0343102-5ce3-4842-9667-6eccb3607c6d', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0343102-5ce3-4842-9667-6eccb3607c6d', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0343102-5ce3-4842-9667-6eccb3607c6d', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('a0343102-5ce3-4842-9667-6eccb3607c6d', foundational, marriage_as_secular_individual_contract).
narrative_ontology:cs_axiom_status(marriage_as_secular_individual_contract, holdable).
narrative_ontology:cs_axiom_grounding('a0343102-5ce3-4842-9667-6eccb3607c6d', marriage_as_secular_individual_contract, conventional).
narrative_ontology:cs_axiom('a0343102-5ce3-4842-9667-6eccb3607c6d', foundational, constitutional_gender_equity_over_community_autonomy).
narrative_ontology:cs_axiom_status(constitutional_gender_equity_over_community_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('a0343102-5ce3-4842-9667-6eccb3607c6d', constitutional_gender_equity_over_community_autonomy, deontological).
narrative_ontology:cs_reference_frame('a0343102-5ce3-4842-9667-6eccb3607c6d', constitutional_secularism_framework).
narrative_ontology:cs_drift_state('a0343102-5ce3-4842-9667-6eccb3607c6d', contemporary_majoritarian_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a0343102-5ce3-4842-9667-6eccb3607c6d', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seekers_equitable_rights).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_personal_law_boards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and administers the Special Marriage Act through civil courts and marriage officers. Claims authority over marriage registration for citizens who opt out of personal law, grounding its jurisdiction in constitutional individual rights rather than communal tradition.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_state_authority, agenda_setter,
    institutional, generational, mobile, national).

% Gain legal recognition for marriages across religious lines that many personal laws prohibit or complicate. In doing so, they typically sacrifice community acceptance and family support, facing ostracism that the civil registration process does not mitigate.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, interfaith_couples, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__secular_civil_reading, interfaith_couples, payer).

% Access divorce, maintenance, and inheritance provisions under SMA and associated secular jurisprudence that are often more gender-equitable than those available under codified personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seekers_equitable_rights, beneficiary,
    moderate, biographical, constrained, national).

% Lose the exclusive right to adjudicate and register marriages for community members who choose the secular framework. They contest civil court jurisdiction and argue that marriage is inherently a religious institution.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_personal_law_boards, payer,
    organized, generational, constrained, national).

% Document and analyze how SMA functions within India's constitutional architecture, tracking the tension between secular state authority and legal pluralism, and the debate over a prospective Uniform Civil Code.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, secular_state_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, state-administered legal framework for marriage and divorce that transcends religious boundaries, enabling interfaith unions and reducing legal fragmentation for couples who do not wish to be governed by communal norms.
% TRANSFER_FUNCTION: Transfers authority over marriage registration and adjudication from religious institutions and personal law forums to civil courts and state registries; transfers social legitimacy from communal approval to state sanction, with accompanying social costs borne by those who exit community law.
% ABSENT_VOICES: Conservative religious leaders who regard SMA as illegitimate are audible in public discourse but structurally excluded from adjudicating SMA marriages; low-income couples who lack resources to navigate bureaucratic notice requirements and documentation are underrepresented in the policy conversation.
% DISAPPEARANCE_RATIONALE: If the secular civil reading vanished, interfaith marriages would lose their primary legal avenue and revert to precarious informal status; personal law boards would regain exclusive jurisdiction for their communities; and the state's constitutional claim to secular authority over intimate life would be substantially weakened.
% FOUNDING_PROBLEM: Post-independence India needed a marriage law for citizens who rejected or fell outside religious personal laws, including interfaith couples and secularists, without immediately imposing a full Uniform Civil Code.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative law scholars from outside the immediate beneficiary set attest that the post-independence need for a secular marriage avenue was genuine. However, they are divided on whether that problem remains live today or whether SMA has become a permanent authority-consolidation device without progression toward a Uniform Civil Code.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 because the constraint carries genuine coordination value (interfaith legal recognition, gender equity) alongside real extraction (state authority consolidation, social costs for exitants). Suppression (0.45) reflects active judicial displacement of religious authority claims. Theater ratio (0.28) captures performative secularism that has not translated into broader uniform civil code reform. Accessibility collapse (0.65) is elevated: for couples entering SMA, community acceptance and alternative personal-law frameworks typically collapse. Resistance (0.55) is significant, emanating from religious boards and political formations contesting the secular frame. Temporal measurements show slow accumulation of extractiveness through the late twentieth century, plateauing as majoritarian politics renews contestation.
 *
 * PERSPECTIVAL GAP:
 *   The secular state apparatus experiences the constraint as necessary rights-based coordination; interfaith couples experience it as a legal lifeline that simultaneously severs communal ties; religious personal law boards experience it as authority extraction. The engine will compute divergent per-seat classifications because the state holds institutional power with mobile policy exit, couples hold moderate power with constrained social exit, and religious boards hold organized power with constrained legal exit. The dual-positioning of interfaith_couples (beneficiary + payer) is the structural signature of the Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (interfaith_couples, women_seekers_equitable_rights) derive legal rights and state recognition, placing them toward the beneficiary side of directionality. However, interfaith_couples' effective extraction is amplified because they also appear in the victims array as bearers of social costs, and their exit_options are constrained by community ostracism. Religious_personal_law_boards are declared victims (authority displacement) and have constrained exit, pushing their directionality toward the full-target end. The secular_state_authority is not declared beneficiary or victim; its directionality defaults to the power atom's fallback and is modulated by its mobile exit and institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve a genuine post-independence coordination deficit: citizens who rejected religious personal law needed a secular marriage avenue. That founding problem remains live for interfaith couples and secular individuals, so mandatrophy is not resolved. However, the absence of a sunset clause or progression toward a Uniform Civil Code, combined with the accumulation of social costs on exitants, suggests the coordination function has become partially subordinated to state authority maintenance. The Tangled Rope classification captures this hybridity without collapsing it into either pure coordination (Rope) or pure extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the secular civil reading of marriage authority represent a genuine constitutional commitment to individual rights, or a strategic state mechanism to consolidate authority over religious communities?',
    'Comparative historical analysis of legislative intent for SMA 1954 and subsequent judicial interpretation, cross-referenced with trajectory of personal law reform.',
    'If strategic consolidation, the constraint''s extraction is higher than its coordination; if genuine rights commitment, coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural ambiguity of secular civil reading within marriage authority kernel').

omega_variable(
    social_cost_mechanism,
    'Are the social costs borne by individuals exiting community law intrinsic to the Special Marriage Act''s design, or imposed by external communal enforcement?',
    'Comparative study of social ostracism rates for SMA users versus elopement without any civil registration; if costs persist absent state registration, they are extrinsic.',
    'If intrinsic, the constraint extracts from its beneficiaries and directionality for interfaith_couples shifts toward target; if extrinsic, the extraction is misattributed and the constraint is less extractive than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_mechanism, empirical, 'Attribution of social costs to constraint design vs communal reaction').

omega_variable(
    ucc_trajectory,
    'Is the Special Marriage Act a transitional scaffold toward a Uniform Civil Code, or has it become a permanent parallel system?',
    'Legislative history review and analysis of political will for UCC post-2014; absence of sunset clause and repeated extension via judicial interpretation suggests permanence.',
    'If transitional, the constraint may be reclassified as scaffold despite lack of formal sunset; if permanent, tangled_rope or snare classification stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ucc_trajectory, preference, 'Transitional intent vs permanent institutional reality of SMA').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(secular_civil_marriage_tr_t0, marriage_authority_kernel__secular_civil_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(secular_civil_marriage_tr_t14, marriage_authority_kernel__secular_civil_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(secular_civil_marriage_tr_t28, marriage_authority_kernel__secular_civil_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement(secular_civil_marriage_tr_t42, marriage_authority_kernel__secular_civil_reading, theater_ratio, 42, 0.22).
narrative_ontology:measurement(secular_civil_marriage_tr_t56, marriage_authority_kernel__secular_civil_reading, theater_ratio, 56, 0.26).
narrative_ontology:measurement(secular_civil_marriage_tr_t70, marriage_authority_kernel__secular_civil_reading, theater_ratio, 70, 0.28).

% Extraction over time
narrative_ontology:measurement(secular_civil_marriage_be_t0, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(secular_civil_marriage_be_t14, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 14, 0.38).
narrative_ontology:measurement(secular_civil_marriage_be_t28, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 28, 0.46).
narrative_ontology:measurement(secular_civil_marriage_be_t42, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 42, 0.52).
narrative_ontology:measurement(secular_civil_marriage_be_t56, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 56, 0.5).
narrative_ontology:measurement(secular_civil_marriage_be_t70, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 70, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority_kernel__secular_civil_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority_kernel. The kernel decomposes into multiple structurally distinct claims about the source of marriage/family law authority. This reading (secular_civil) coexists with religious personal law readings; they form a constraint family linked by mutual structural pressure within Indian legal pluralism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
