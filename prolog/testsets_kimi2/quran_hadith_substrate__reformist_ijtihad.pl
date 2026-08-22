% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__reformist_ijtihad
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__reformist_ijtihad, []).

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
 *   constraint_id: quran_hadith_substrate__reformist_ijtihad
 *   human_readable: Reformist Ijtihad Prioritizing Quranic Ethical Trajectory
 *   domain: religious/legal
 *
 * SUMMARY:
 *   This constraint story models the reformist_ijtihad reading of the
 *   quran_hadith_substrate kernel. The standing arrangement is that
 *   contextual ijtihad is mandated when classical fiqh rulings conflict with
 *   contemporary ethics, human rights norms, or maslaha (public interest),
 *   prioritizing the Quran's ethical trajectory over literalist hadith
 *   application. This reading benefits marginalized Muslim
 *   communitiesâwomen, LGBTQ+ Muslims, religious minorities, and
 *   progressive Muslimsâby generating religiously grounded legal
 *   alternatives to classical discriminatory rulings. It extracts
 *   interpretive monopoly and institutional prestige from traditional ulema
 *   and classical madhhab institutions, who lose their gatekeeping role. The
 *   constraint is vulnerable to traditionalist counter-mobilization,
 *   indicating it requires active discursive and institutional enforcement to
 *   maintain its coordination function.
 *
 * KEY AGENTS:
 *   - reformist_scholars: agenda_setter (moderate/constrained) â develop contextual hermeneutics
 *   - muslim_women: beneficiary (powerless/identity_locked) â seek equitable family-law rulings
 *   - lgbtq_muslims: beneficiary (powerless/identity_locked) â seek protection from punitive classical rulings
 *   - religious_minorities: beneficiary (powerless/identity_locked) â benefit from universalized ethical readings
 *   - traditional_ulema: payer (institutional/constrained) â lose interpretive monopoly
 *   - classical_madhhab_institutions: payer (institutional/constrained) â curricula and endowments undermined
 *   - human_rights_observers: observer (institutional/analytical) â monitor implementation gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__reformist_ijtihad, 0.42).
domain_priors:suppression_score(quran_hadith_substrate__reformist_ijtihad, 0.4).
domain_priors:theater_ratio(quran_hadith_substrate__reformist_ijtihad, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, extractiveness, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_hadith_substrate__reformist_ijtihad, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__reformist_ijtihad, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__reformist_ijtihad, "Reformist Ijtihad Prioritizing Quranic Ethical Trajectory").
narrative_ontology:topic_domain(quran_hadith_substrate__reformist_ijtihad, "religious/legal").

domain_priors:requires_active_enforcement(quran_hadith_substrate__reformist_ijtihad).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__reformist_ijtihad, '3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be').
narrative_ontology:cs_kernel_codification('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', fixed_text).
narrative_ontology:cs_authority_grounding('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', expertise).
narrative_ontology:cs_interpretation_layer_present('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be').
narrative_ontology:cs_reading_relation('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', quran_hadith_substrate__traditionalist_taqlid, forecloses).
narrative_ontology:cs_reading_relation('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', foundational, quran_ethical_trajectory_supersedes_literalist_hadith).
narrative_ontology:cs_axiom_status(quran_ethical_trajectory_supersedes_literalist_hadith, holdable).
narrative_ontology:cs_axiom_grounding('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', quran_ethical_trajectory_supersedes_literalist_hadith, theological).
narrative_ontology:cs_axiom('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', foundational, maslaha_and_contemporary_ethics_as_ijtihad_constraints).
narrative_ontology:cs_axiom_status(maslaha_and_contemporary_ethics_as_ijtihad_constraints, holdable).
narrative_ontology:cs_axiom_grounding('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', maslaha_and_contemporary_ethics_as_ijtihad_constraints, instrumental).
narrative_ontology:cs_reference_frame('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', quran_ethical_trajectory_priority).
narrative_ontology:cs_drift_state('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3eb05e9d-be88-43ff-a7ce-e1bed5b9f5be', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, progressive_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, muslim_women).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__reformist_ijtihad, religious_minorities).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, traditional_ulema).
narrative_ontology:constraint_victim(quran_hadith_substrate__reformist_ijtihad, classical_madhhab_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and advocate contextual hermeneutics that prioritize the Quran's ethical trajectory over literalist hadith application; issue fatwas and scholarly opinions; face institutional exclusion from traditional seminaries and state religious bureaucracies in many countries.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, reformist_scholars, agenda_setter,
    moderate, generational, constrained, global).

% Congregations and civil-society groups organizing around gender equality, human rights, and pluralism; use reformist scholarly output to justify internal religious practice and public advocacy.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, progressive_muslims, beneficiary,
    moderate, biographical, constrained, national).

% Seek equitable rulings on marriage, divorce, inheritance, and bodily autonomy; benefit when reformist ijtihad produces progressive fatwas; remain subject to classical family law in many states.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, muslim_women, beneficiary,
    powerless, biographical, identity_locked, national).

% Seek recognition and protection from punitive classical rulings; depend on reformist interpretive frameworks that contextualize or re-read source texts; face severe social and legal consequences in traditionalist jurisdictions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, lgbtq_muslims, beneficiary,
    powerless, biographical, identity_locked, national).

% Non-Muslim minorities in Muslim-majority states benefit when reformist ijtihad limits classical differential treatment and prioritizes universal ethical principles over dhimmi-status restrictions.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, religious_minorities, beneficiary,
    powerless, biographical, identity_locked, national).

% Derive institutional legitimacy from mastery and transmission of classical madhhab rulings; lose interpretive monopoly and social prestige when reformist ijtihad bypasses their authority; mobilize to defend classical consensus.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, traditional_ulema, payer,
    institutional, generational, constrained, global).

% Seminaries, endowments, and certification bodies organized around the four schools; their curricula, credentialing, and financial models depend on the authority of classical fiqh; reformist ijtihad undermines their epistemic gatekeeping.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, classical_madhhab_institutions, payer,
    institutional, civilizational, constrained, global).

% Reject contextual ijtihad as religious innovation (bid'ah); structurally excluded from reformist scholarly networks and state legal-reform commissions while remaining dominant in traditional seminaries and popular religious markets.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, literalist_scholars, excluded,
    institutional, generational, constrained, global).

% International bodies and NGOs monitor whether reformist legal theory translates into state policy changes; document gaps between reformist scholarly consensus and lived legal reality.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__reformist_ijtihad, human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of adapting medieval fiqh to modern ethical and human-rights conditions by providing a religiously grounded methodology (contextual ijtihad) that prioritizes Quranic ethical trajectory and maslaha over literalist hadith application.
% TRANSFER_FUNCTION: Moves interpretive authority and social legitimacy from classical madhhab institutions to reformist scholars; moves legal protection, religious agency, and social recognition to marginalized Muslims (women, LGBTQ+ individuals, religious minorities).
% ABSENT_VOICES: Literalist hadith scholars and quietist traditionalist communities reject contextual ijtihad as bid'ah; they are structurally excluded from reformist institutional spaces and state legal-reform commissions, though they dominate classical seminaries and popular religious markets.
% DISAPPEARANCE_RATIONALE: If the reformist ijtihad constraint vanished overnight, progressive rulings on gender, minority rights, and bodily autonomy would revert to classical frameworks; marginalized Muslims would lose religiously grounded legal alternatives; and traditional ulema would regain interpretive monopoly.
% FOUNDING_PROBLEM: Classical fiqh rulings developed in medieval contexts produce outcomes inconsistent with contemporary ethics, human rights, and public interest (maslaha), creating a legitimacy crisis for Islamic law in modern states.
% FOUNDING_PROBLEM_CORROBORATION: Muslim feminist scholars, human rights organizations, and some state legal-reform commissions attest to the problem from outside the traditionalist beneficiary structure; traditional ulema deny that classical rulings are ethically deficient, attributing the crisis to modernist deviation.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__reformist_ijtihad, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__reformist_ijtihad, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__reformist_ijtihad, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_hadith_substrate__reformist_ijtihad, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__reformist_ijtihad, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__reformist_ijtihad_tests).
:- end_tests(quran_hadith_substrate__reformist_ijtihad_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the constraint genuinely coordinates religious-legal adaptation for marginalized groups, but it systematically displaces traditional authority, constituting real extraction from that seat. Suppression (0.40) is moderate-lower than a traditionalist monopoly would be, because the reformist reading structurally permits interpretive pluralism; yet some suppression of traditional alternatives is inherent in institutionalizing reformist outcomes. Theater ratio (0.30) reflects moderate performativityâstates and organizations may adopt reformist rhetoric without substantive legal change. Accessibility collapse (0.45) is moderate: traditionalist alternatives remain widely available and institutionally entrenched. Resistance (0.65) is high because traditional ulema actively contest the reformist framework. Measurements show extraction rising slowly as reformist scholarship accumulates institutional footholds, alongside rising theater as symbolic adoption outpaces substantive reform.
 *
 * PERSPECTIVAL GAP:
 *   Reformist scholars and marginalized beneficiaries experience the constraint as liberation and necessary ethical updating; traditional ulema experience the same constraint as epistemic colonialism and loss of rightful authority. The engine computes this divergence from structural data: agenda_setters and beneficiaries have low directionality, while payers have high directionality. A human_rights_observer seat would compute a different type again, seeing the gap between scholarly theory and state practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (progressive_muslims, muslim_women, lgbtq_muslims, religious_minorities) receive low directionality because the constraint subsidizes their legal standing and religious agency. The agenda_setter (reformist_scholars) also sits near the beneficiary end because the constraint amplifies their scholarly authority, though they bear the labor of production. Payers (traditional_ulema, classical_madhhab_institutions) sit near the full-target end because the constraint directly undermines their institutional legitimacy and economic base. No override is needed because the structural derivation matches these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmedieval fiqh producing ethically untenable outcomes in modern contextsâis contested but live. The arrangement is not a piton because there are identifiable, concentrated beneficiaries and the coordination function (ethical reconciliation) is actively performed. It is not a snare because the coordination is genuine: reformist ijtihad does produce actionable religious guidance that solves real problems for its beneficiaries. It is not a rope because the extraction from traditional authority is asymmetric and structural, not incidental. The Tangled Rope classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates the reformist_ijtihad reading of kernel quran_hadith_substrate; siblings traditionalist_taqlid and state_hybrid would structurally invert the beneficiary/victim map and alter enforcement patterns. What changes if a sibling reading is adopted as dominant?',
    'Comparative analysis of legal outcomes and institutional power distributions under each reading in matched jurisdictions.',
    'If traditionalist_taqlid is structurally dominant, this constraint''s extraction is reversed (traditionalists become beneficiaries, reformists and marginalized groups become victims), flipping directionality and effective extraction profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural location of this reading within the contested kernel').

omega_variable(
    quran_ethical_trajectory_determinacy,
    'Is the Quranic ethical trajectory sufficiently determinate to adjudicate specific conflicts (e.g., LGBTQ+ rights, gender equality) without collapsing into subjective preference?',
    'Systematic review of reformist jurisprudential outputs to measure inter-subject agreement on contested cases.',
    'If underdetermined, the constraint''s coordination function is weaker than claimed and may mask conventional preference-formation; if determinate, the extraction from traditional authority is justified by genuine hermeneutic recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quran_ethical_trajectory_determinacy, conceptual, 'Whether the Quranic ethical trajectory provides determinate guidance').

omega_variable(
    state_backing_dependency,
    'Does the reformist ijtihad constraint depend on state power for enforcement, or can it persist through civil-scholarly networks alone?',
    'Compare persistence and uptake in state-backed versus non-state reformist communities (diaspora, online).',
    'If state-dependent, classification shifts toward enforcement_mechanism with higher suppression; if autonomous, the coordination function is more robust and extraction is genuinely discursive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_backing_dependency, empirical, 'Whether reformist ijtihad requires state enforcement to persist').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of traditionalist alternatives structural (state or institutional exclusion) or internalized (loss of epistemic confidence and self-censorship among traditional scholars)?',
    'Track publication, tenure, and public-position patterns in religious seminaries post-reformist institutional uptake.',
    'If internalized, effective suppression exceeds the structural measure; traditional ulema may self-censor even without formal exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of traditional authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__reformist_ijtihad, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 0, 0.2).
narrative_ontology:measurement(qura_tr_t10, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 10, 0.24).
narrative_ontology:measurement(qura_tr_t20, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 20, 0.28).
narrative_ontology:measurement(qura_tr_t30, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 30, 0.32).
narrative_ontology:measurement(qura_tr_t40, quran_hadith_substrate__reformist_ijtihad, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(qura_be_t10, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(qura_be_t20, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(qura_be_t30, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(qura_be_t40, quran_hadith_substrate__reformist_ijtihad, base_extractiveness, 40, 0.5).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(quran_hadith_substrate__reformist_ijtihad, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__reformist_ijtihad, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__reformist_ijtihad, quran_hadith_substrate__state_hybrid).

% DUAL FORMULATION NOTE:
% This story is one member of the quran_hadith_substrate constraint family. The kernel (Quran and hadith as legal substrate) decomposes into three structurally distinct readings: reformist_ijtihad (this file), traditionalist_taqlid, and state_hybrid. Each reading carries a different epsilon, beneficiary/victim structure, and classification. Linkages enable contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
