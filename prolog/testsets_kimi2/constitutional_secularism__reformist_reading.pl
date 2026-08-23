% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__reformist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__reformist_reading, []).

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
 *   constraint_id: constitutional_secularism__reformist_reading
 *   human_readable: Affirmative State Duty to Eliminate Oppressive Religious Practices (Reformist Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   An interpretation of constitutional secularism that imposes an
 *   affirmative duty on the state to actively identify and eliminate
 *   religious practices oppressing marginalized groups, particularly
 *   scheduled castes and women, even when this requires overriding religious
 *   autonomy claims and personal law. This is the reformist reading of the
 *   constitutional secularism kernel, more interventionist than principled
 *   permission and directly opposed to strict neutrality. The constraint
 *   operates through legislative prohibition, executive enforcement, and
 *   judicial review of religious customs.
 *
 * KEY AGENTS:
 *   - state_apparatus: Primary agenda-setter (institutional/arbitrage) â enforces the duty through legislation and administration
 *   - constitutional_court: Co-agenda-setter (institutional/arbitrage) â interprets and mandates intervention
 *   - scheduled_castes: Primary beneficiary (organized/constrained) â receives protection against caste-based religious discrimination
 *   - women_within_religious_communities: Primary beneficiary (organized/constrained) â receives protection against gender-discriminatory religious customs
 *   - religious_conservatives: Primary payer (organized/constrained) â bears loss of communal autonomy
 *   - religious_institutions: Secondary payer (organized/constrained) â loses jurisdiction over personal law and worship regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, 0.78).
domain_priors:suppression_score(constitutional_secularism__reformist_reading, 0.82).
domain_priors:theater_ratio(constitutional_secularism__reformist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(constitutional_secularism__reformist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__reformist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__reformist_reading, "Affirmative State Duty to Eliminate Oppressive Religious Practices (Reformist Reading)").
narrative_ontology:topic_domain(constitutional_secularism__reformist_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__reformist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__reformist_reading, 'd7027788-7fb4-4cfd-933c-8712f6eef1a0').
narrative_ontology:cs_kernel_codification('d7027788-7fb4-4cfd-933c-8712f6eef1a0', formalized).
narrative_ontology:cs_authority_grounding('d7027788-7fb4-4cfd-933c-8712f6eef1a0', lineage).
narrative_ontology:cs_interpretation_layer_present('d7027788-7fb4-4cfd-933c-8712f6eef1a0').
narrative_ontology:cs_reading_relation('d7027788-7fb4-4cfd-933c-8712f6eef1a0', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('d7027788-7fb4-4cfd-933c-8712f6eef1a0', constitutional_secularism__principled_intervention_reading, coexists_with).
narrative_ontology:cs_axiom('d7027788-7fb4-4cfd-933c-8712f6eef1a0', foundational, affirmative_duty_to_reform_religion).
narrative_ontology:cs_axiom_status(affirmative_duty_to_reform_religion, holdable).
narrative_ontology:cs_axiom_grounding('d7027788-7fb4-4cfd-933c-8712f6eef1a0', affirmative_duty_to_reform_religion, deontological).
narrative_ontology:cs_axiom('d7027788-7fb4-4cfd-933c-8712f6eef1a0', foundational, religious_autonomy_subordinate_to_egalitarianism).
narrative_ontology:cs_axiom_status(religious_autonomy_subordinate_to_egalitarianism, holdable).
narrative_ontology:cs_axiom_grounding('d7027788-7fb4-4cfd-933c-8712f6eef1a0', religious_autonomy_subordinate_to_egalitarianism, deontological).
narrative_ontology:cs_reference_frame('d7027788-7fb4-4cfd-933c-8712f6eef1a0', constitutional_egalitarian_mandate).
narrative_ontology:cs_drift_state('d7027788-7fb4-4cfd-933c-8712f6eef1a0', contemporary_judicial_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7027788-7fb4-4cfd-933c-8712f6eef1a0', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__reformist_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, scheduled_castes).
narrative_ontology:constraint_beneficiary(constitutional_secularism__reformist_reading, women_within_religious_communities).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_conservatives).
narrative_ontology:constraint_victim(constitutional_secularism__reformist_reading, religious_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts legislation and empowers administrative bodies to prohibit religious practices deemed oppressive to scheduled castes and women, overriding communal autonomy claims through constitutional mandate and executive enforcement.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets constitutional provisions to mandate state intervention in religious affairs, striking down personal laws and customs that discriminate against marginalized groups within religious communities.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, constitutional_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive constitutional and statutory protection against religiously sanctioned caste discrimination; rely on state enforcement to access temples, burial grounds, and social participation previously denied by dominant community norms.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, scheduled_castes, beneficiary,
    organized, generational, constrained, national).

% Benefit from judicial and legislative override of religious personal laws and customs that exclude them from institutions or permit unilateral divorce; their rights claims are enforced against community resistance.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, women_within_religious_communities, beneficiary,
    organized, biographical, constrained, national).

% Uphold traditional religious practices and personal law governed by scriptural or customary authority; bear the loss of communal self-governance as state institutions define and prohibit their norms as oppressive.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_conservatives, payer,
    organized, biographical, constrained, national).

% Lose exclusive jurisdiction over personal law, worship regulation, and community membership criteria as courts and legislatures determine which practices violate constitutional equality mandates.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, religious_institutions, payer,
    organized, generational, constrained, national).

% Organize litigation and public campaigns arguing that constitutional equality requires active state suppression of oppressive religious customs; they neither pay costs nor collect rents but advance the normative framework.
narrative_ontology:constraint_stakeholder(constitutional_secularism__reformist_reading, reform_advocates, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective action problem of marginalized groups within religious communities being unable to overcome internally oppressive practices due to communal power asymmetries; provides an external enforcement mechanism for rights claims that would otherwise be suppressed by religious authority structures.
% TRANSFER_FUNCTION: Moves authority over religious personal law and practice from religious communities and institutions to state courts and legislatures, and transfers protective legal standing to scheduled castes and women within religious communities.
% ABSENT_VOICES: Religious conservatives from minority communities who oppose state intervention but are politically marginalized; dissenting theologians who argue for internal reform rather than state imposition; and secular critics who warn that state power selectively targets minority religions while shielding majority customs.
% DISAPPEARANCE_RATIONALE: If the state's affirmative duty vanished overnight, religious personal law and customary authority would immediately reassert control over contested practices such as marriage, divorce, temple entry, and inheritance; scheduled castes and women would lose external enforcement of their rights claims, and the constitutional balance would shift dramatically toward communal autonomy.
% FOUNDING_PROBLEM: Caste-based exclusion and gender discrimination embedded in religious personal laws and customs that communities failed to reform internally, leaving marginalized members without recourse against internally legitimate but externally oppressive practices.
% FOUNDING_PROBLEM_CORROBORATION: Scheduled caste organizations and women's rights groups attest the problem is live; religious conservatives dispute both the diagnosis and the remedy. Independent constitutional historians and social scientists outside both camps document the persistence of the practices, though they divide on whether state intervention or internal reform is the appropriate remedy.
narrative_ontology:disappearance_verdict(constitutional_secularism__reformist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__reformist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__reformist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__reformist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__reformist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__reformist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__reformist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__reformist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint systematically transfers authority over religious practice from communities to state institutions, overriding autonomy claims. Suppression is higher (0.82) because the arrangement depends on actively enforcing prohibitions against religious practices that communities would otherwise continue. Theater ratio is moderate (0.40): much intervention is substantive, but a visible share serves performative state-building rather than measurable emancipation. Accessibility collapse is substantial (0.70) because legal alternatives to state-defined equality are foreclosed for affected communities. Resistance is high (0.75) because religious conservatives and institutions actively contest the constraint through litigation, political mobilization, and social resistance. The measurement series share a single time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (scheduled castes, women) experience the constraint as protective coordination that solves an internal collective-action problem they cannot solve alone. The payer seats (religious conservatives, institutions) experience the same structure as extractive overreach that destroys communal self-governance. The state apparatus experiences it as constitutional mandate. The engine computes this divergence from the structural asymmetry in exit options and cost-bearing, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Scheduled castes and women are structural beneficiaries: the constraint subsidizes their rights claims against internal community power. Religious conservatives and institutions are structural targets: the constraint extracts authority and autonomy from them. The state and court are near-symmetric administrators exercising power without personal cost or gain. Directionality is structurally derived: beneficiaries have constrained exit but receive protection; payers have constrained exit and bear losses; the state defines the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by preserving the genuine coordination function: marginalized groups within religious communities do receive enforceable rights they would lack under communal autonomy. It also prevents mislabeling as pure coordination (rope) by acknowledging the asymmetric extraction on religious conservatives and the active enforcement required to suppress alternatives. If the protective function atrophied while the enforcement persisted, the constraint would drift toward piton or snare; temporal measurements track whether extractiveness rises relative to emancipatory outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (reformist) of a contested kernel (constitutional secularism); do the sibling readings (strict neutrality, principled intervention) represent structurally separable constraints or merely different policy postures within a single framework?',
    'Comparative classification of each sibling reading as an independent constraint story; if epsilon values diverge significantly, they are distinct constraints under the epsilon-invariance principle.',
    'If separable, this reading''s high extractiveness is intrinsic to its specific normative structure; if unified, the variance is observer-relative and the kernel requires a single type assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Whether the kernel decomposes into distinct constraints or one variable constraint').

omega_variable(
    selective_enforcement_ambiguity,
    'Is the state''s intervention applied consistently across all religions, or does it selectively target minority religious practices while shielding majority-community customs?',
    'Quantitative analysis of judicial and legislative interventions by religion of affected community, controlling for practice type and constitutional salience.',
    'If selective, the constraint functions as asymmetric extraction on minority autonomy under cover of universal reform, shifting classification toward snare; if consistent, extraction is evenly distributed and the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_ambiguity, empirical, 'Whether state secularism reform is applied uniformly across communities').

omega_variable(
    oppression_diagnosis_source,
    'Does the state accurately identify oppressive practices, or does it conflate internal community disagreement with objective oppression?',
    'Independent ethnographic and rights-based audit of state-prohibited practices against the self-understanding of affected community members, particularly marginalized subgroup voices.',
    'If the state misidentifies practices, the coordination function is partially false and extraction on religious conservatives is inflated; if accurate, the beneficiary structure is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oppression_diagnosis_source, empirical, 'Accuracy of state identification of oppressive practices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__reformist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csr_ref_tr_t0, constitutional_secularism__reformist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(csr_ref_tr_t10, constitutional_secularism__reformist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(csr_ref_tr_t20, constitutional_secularism__reformist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(csr_ref_tr_t30, constitutional_secularism__reformist_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(csr_ref_tr_t40, constitutional_secularism__reformist_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(csr_ref_tr_t50, constitutional_secularism__reformist_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(csr_ref_be_t0, constitutional_secularism__reformist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(csr_ref_be_t10, constitutional_secularism__reformist_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(csr_ref_be_t20, constitutional_secularism__reformist_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(csr_ref_be_t30, constitutional_secularism__reformist_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(csr_ref_be_t40, constitutional_secularism__reformist_reading, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(csr_ref_be_t50, constitutional_secularism__reformist_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(csr_ref_su_t0, constitutional_secularism__reformist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(csr_ref_su_t10, constitutional_secularism__reformist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(csr_ref_su_t20, constitutional_secularism__reformist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(csr_ref_su_t30, constitutional_secularism__reformist_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(csr_ref_su_t40, constitutional_secularism__reformist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(csr_ref_su_t50, constitutional_secularism__reformist_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__reformist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__reformist_reading, principled_intervention_reading).

% DUAL FORMULATION NOTE:
% The constitutional secularism kernel decomposes into three structurally distinct constraints: strict neutrality (non-interference), principled intervention (permissive reform), and reformist duty (affirmative override). Each reading has a unique epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
