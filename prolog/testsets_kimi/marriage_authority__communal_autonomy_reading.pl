% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy Reading of Marriage Authority
 *   domain: legal_constitutional_family
 *
 * SUMMARY:
 *   This constraint instantiates the communal_autonomy_reading of the
 *   marriage_authority kernel: marriage and family law authority is grounded
 *   in community religious tradition, with the state enforcing but not
 *   authoring the norms. Personal law variation is institutionalized,
 *   legislative amendments require community consent, and religious
 *   leadership retains interpretive control over family matters. The state
 *   judiciary applies these norms in civil disputes, creating a legally
 *   pluralist regime where democratic family law is deferred to communal
 *   authorities.
 *
 * KEY AGENTS:
 *   - religious_leadership: Primary beneficiary (organized/constrained) â collects deference and normative authority over community family life.
 *   - state_judiciary: Enforcing agenda-setter (institutional/analytical) â applies religious personal law without authoring it.
 *   - intra_community_dissenters: Primary target (powerless/identity_locked) â bear the costs of norms they cannot exit without social rupture.
 *   - secular_legislature: Observer (institutional/analytical) â could legislate reform but defers to community consent.
 *   - inter_faith_families: Excluded party (moderate/constrained) â falls between communal categories and is invisible to the framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.56).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.64).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Reading of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_constitutional_family").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '724fc00b-182c-4cdf-a0f2-bd135a97f287').
narrative_ontology:cs_kernel_codification('724fc00b-182c-4cdf-a0f2-bd135a97f287', fixed_text).
narrative_ontology:cs_authority_grounding('724fc00b-182c-4cdf-a0f2-bd135a97f287', lineage).
narrative_ontology:cs_interpretation_layer_present('724fc00b-182c-4cdf-a0f2-bd135a97f287').
narrative_ontology:cs_reading_relation('724fc00b-182c-4cdf-a0f2-bd135a97f287', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('724fc00b-182c-4cdf-a0f2-bd135a97f287', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('724fc00b-182c-4cdf-a0f2-bd135a97f287', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('724fc00b-182c-4cdf-a0f2-bd135a97f287', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('724fc00b-182c-4cdf-a0f2-bd135a97f287', foundational, communal_religious_tradition_as_family_law_source).
narrative_ontology:cs_axiom_status(communal_religious_tradition_as_family_law_source, holdable).
narrative_ontology:cs_axiom_grounding('724fc00b-182c-4cdf-a0f2-bd135a97f287', communal_religious_tradition_as_family_law_source, theological).
narrative_ontology:cs_axiom('724fc00b-182c-4cdf-a0f2-bd135a97f287', foundational, legislative_amendment_requires_community_consent).
narrative_ontology:cs_axiom_status(legislative_amendment_requires_community_consent, holdable).
narrative_ontology:cs_axiom_grounding('724fc00b-182c-4cdf-a0f2-bd135a97f287', legislative_amendment_requires_community_consent, conventional).
narrative_ontology:cs_reference_frame('724fc00b-182c-4cdf-a0f2-bd135a97f287', communal_religious_autonomy).
narrative_ontology:cs_drift_state('724fc00b-182c-4cdf-a0f2-bd135a97f287', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('724fc00b-182c-4cdf-a0f2-bd135a97f287', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains normative authority over marriage, divorce, and inheritance for their community through state-enforced personal law statutes. The state codifies and enforces religious leadership's interpretations without requiring democratic legislative revision. Their authority persists as long as the community remains a legally recognized category with separate family law.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary,
    organized, generational, constrained, national).

% Enforces personal law statutes derived from religious traditions in civil courts and administrative tribunals. Does not author substantive family norms but adjudicates disputes under them, deferring to religious scholarship for interpretation. Reluctant to reform personal law without explicit community leadership consent.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Community members, disproportionately women and liberal reformers, who seek equitable marriage or divorce norms but are bound by religious personal law enforced by the state. Exit via conversion, civil marriage, or apostasy carries severe social ostracism and legal ambiguity; their communal identity locks them into normative frameworks they did not choose.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, national).

% Holds constitutional authority to legislate a uniform civil code but defers to religious community leadership due to political costs of overriding communal autonomy. Observes the pluralist arrangement from a distance, amending personal laws only when community elites signal consent.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, secular_legislature, observer,
    institutional, generational, analytical, national).

% Fall between personal law categories or are compelled to adopt one partner's community law. Their specific family-form needs are unaddressed by the communal autonomy framework, and they are excluded from the legislative bargain that preserves religious leadership authority.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, inter_faith_families, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows distinct religious communities to govern marriage and family life according to their own traditions, preventing majoritarian legislative domination and preserving cultural autonomy through state recognition and judicial enforcement of separate personal law codes.
% TRANSFER_FUNCTION: Transfers authority over marriage, divorce, and inheritance norm-setting from the democratic legislature to religious community leadership, and transfers compliance obligations from community members to religiously-derived personal law enforced by state courts.
% ABSENT_VOICES: Intra-community dissenters seeking gender-equitable reform, inter-faith families, and secularist advocates for a uniform civil code are structurally sidelined because amendments require community consent and the state treats religious leadership as the legitimate interlocutor for each community.
% DISAPPEARANCE_RATIONALE: If the state withdrew enforcement of community personal law, religious leadership would lose state-backed coercive authority over family matters, intra-community dissenters would gain access to secular or reformed norms, and the legislature would face immediate pressure to craft uniform or opt-out frameworks â the pluralist architecture would collapse.
% FOUNDING_PROBLEM: Post-colonial and post-partition states needed to accommodate deep religious diversity without imposing homogeneous family law on minority communities, and to secure community loyalty by respecting religious identity in private law.
% FOUNDING_PROBLEM_CORROBORATION: Religious leadership attests the problem is still live, citing threats to community identity from majoritarian nationalism. Secular historians and constitutional scholars outside the beneficiary set argue the founding problem of minority protection has been superseded by constitutional equality guarantees and that the arrangement now primarily preserves patriarchal authority; women's rights organizations and intra-community reformers corroborate the dead-problem reading from the victim seat.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.56, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.56) because state enforcement transfers real authority to religious leadership, but the arrangement retains a genuine coordination function in protecting minority legal identity. Suppression is higher (0.64) because dissent is actively suppressed by state courts applying religious norms and by social identity-lock. Theater ratio is moderate (0.38): enforcement involves performative state neutrality that masks asymmetric extraction. Accessibility collapse (0.60) reflects that formal alternatives exist (civil marriage, conversion) but are socially inaccessible for identity-locked members. Resistance (0.50) captures active but fragmented reform movements.
 *
 * PERSPECTIVAL GAP:
 *   Religious leadership experiences the constraint as rope â protected authority necessary for community survival. Intra-community dissenters experience it as snare â enforced extraction with no viable exit. The state judiciary experiences it as tangled rope â a necessary coordination mechanism with unacceptable side effects. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership sits near the beneficiary end (low d): they collect authority and deference, and while their exit is constrained by institutional role, the constraint subsidizes their power. Intra-community dissenters sit near the full-target end (high d): they bear the normative costs, have powerless status, and are identity-locked into the framework. State judiciary sits near symmetric (moderate d): they enforce but do not collect extraction, and their analytical exit keeps them from being targets. Inter-faith families are excluded entirely â the constraint is not oriented toward them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of minority community protection is contested as live or dead. If dead, the constraint would be a piton candidate; however, active enforcement and concentrated beneficiary capture persist, and minority protection claims are still sincerely invoked by non-beneficiaries. The coordination function has atrophied partially but not fully. The theater ratio captures the remaining performative coordination. The mandate has not definitively outlived its function, preventing piton classification despite substantial extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_autonomy_vs_elite_capture,
    'Does the institutionalization of personal law variation protect minority communities from majoritarian domination, or does it primarily entrench religious leadership authority at the expense of intra-community dissenters?',
    'Comparative analysis of reform trajectories across communities with and without state-enforced personal law, examining whether dissenters gain voice when state enforcement is withdrawn or made optional.',
    'If elite capture dominates, the constraint is more extractive than its coordination framing suggests and effective extraction on intra-community dissenters is higher. If minority protection dominates, the coordination function is genuine and extraction is bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_autonomy_vs_elite_capture, empirical, 'Whether personal law pluralism serves community protection or elite capture.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of intra-community dissent structural (state courts enforcing religious norms) or internalized (community social sanctions and identity-lock)?',
    'Post-reform trajectory analysis: if dissenters assert rights immediately upon legal reform (e.g., optional civil marriage), suppression was primarily structural. If reform uptake is slow despite legal availability, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s extractiveness on identity-locked targets is higher than base extractiveness suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    kernel_reading_location,
    'This constraint is the communal_autonomy_reading of the marriage_authority kernel. How would the structural classification change if the secularist_reading (Uniform Civil Code legislative authority) were adopted instead?',
    'Compare jurisdictions that have moved from personal law pluralism to uniform civil codes: measure extraction redistribution from religious leadership to the state, and changes in dissenters'' exit options.',
    'Adoption of the secularist reading would eliminate the religious_leadership beneficiary seat and the intra_community_dissenter victim seat, converting the constraint into a state-centered enforcement mechanism with different directionalities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural delta between communal autonomy and secularist readings of marriage authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__communal_autonomy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__communal_autonomy_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__communal_autonomy_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__communal_autonomy_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(marr_be_t8, marriage_authority__communal_autonomy_reading, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(marr_be_t16, marriage_authority__communal_autonomy_reading, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(marr_be_t24, marriage_authority__communal_autonomy_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(marr_be_t32, marriage_authority__communal_autonomy_reading, base_extractiveness, 32, 0.53).
narrative_ontology:measurement(marr_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t8, marriage_authority__communal_autonomy_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(marr_su_t16, marriage_authority__communal_autonomy_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(marr_su_t24, marriage_authority__communal_autonomy_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(marr_su_t32, marriage_authority__communal_autonomy_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement(marr_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority kernel. The communal_autonomy_reading grounds authority in religious tradition with state enforcement; sibling readings offer secular legislative, gender-equality, federalist consociational, and judicial harmonization framings. Decomposition follows the epsilon-invariance principle: each reading has a distinct beneficiary/victim structure and extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
