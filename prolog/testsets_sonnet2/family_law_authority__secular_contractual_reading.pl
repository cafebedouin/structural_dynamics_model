% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Secular Contractual Reading of Marriage under Civil Registration Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested family-law-authority
 *   kernel: the secular contractual reading, under which marriage is a civil
 *   contract between autonomous individuals validated solely by state
 *   registration, with gender-symmetric statutory rights and no religious
 *   requirement. It deliberately does not describe the contest among readings
 *   — the Hindu dharmashastra, Muslim shariat, Christian canonical, and Parsi
 *   Zoroastrian readings are separate constraints (see network links) with
 *   their own ε, beneficiaries, and victims. Within this reading alone: state
 *   registration provides genuine coordination value (a neutral forum for
 *   interfaith and cross-community marriage, predictable rights enforcement)
 *   but also extracts jurisdictional authority from religious communities and
 *   disadvantages parties to unregistered customary unions who lack access to
 *   or awareness of the registration apparatus.
 *
 * KEY AGENTS:
 *   - civil_registration_authorities: institutional agenda_setter administering the uniform civil marriage statute
 *   - interfaith_couples: beneficiaries who gain a marriage route outside any single religious jurisdiction
 *   - spouses_seeking_gender_symmetric_rights: beneficiaries of statutory rights parity
 *   - religious_minorities_preferring_personal_law: payers whose communal jurisdiction is displaced
 *   - parties_to_unregistered_customary_unions: powerless payers who lose recognition through a formal-validity technicality
 *   - religious_authorities_and_clergy: excluded from the civil registration process despite retaining informal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.28).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.22).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Secular Contractual Reading of Marriage under Civil Registration Law").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '713a9f75-e152-4409-a1ea-22f7215ec7f1').
narrative_ontology:cs_kernel_codification('713a9f75-e152-4409-a1ea-22f7215ec7f1', formalized).
narrative_ontology:cs_authority_grounding('713a9f75-e152-4409-a1ea-22f7215ec7f1', extraction).
narrative_ontology:cs_interpretation_layer_present('713a9f75-e152-4409-a1ea-22f7215ec7f1').
narrative_ontology:cs_reading_relation('713a9f75-e152-4409-a1ea-22f7215ec7f1', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('713a9f75-e152-4409-a1ea-22f7215ec7f1', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('713a9f75-e152-4409-a1ea-22f7215ec7f1', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('713a9f75-e152-4409-a1ea-22f7215ec7f1', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('713a9f75-e152-4409-a1ea-22f7215ec7f1', foundational, state_registration_as_sole_validity_criterion).
narrative_ontology:cs_axiom_status(state_registration_as_sole_validity_criterion, holdable).
narrative_ontology:cs_axiom_grounding('713a9f75-e152-4409-a1ea-22f7215ec7f1', state_registration_as_sole_validity_criterion, conventional).
narrative_ontology:cs_axiom('713a9f75-e152-4409-a1ea-22f7215ec7f1', foundational, gender_symmetric_marital_rights_regardless_of_religion).
narrative_ontology:cs_axiom_status(gender_symmetric_marital_rights_regardless_of_religion, holdable).
narrative_ontology:cs_axiom_grounding('713a9f75-e152-4409-a1ea-22f7215ec7f1', gender_symmetric_marital_rights_regardless_of_religion, deontological).
narrative_ontology:cs_reference_frame('713a9f75-e152-4409-a1ea-22f7215ec7f1', colonial_era_personal_law_pluralism).
narrative_ontology:cs_drift_state('713a9f75-e152-4409-a1ea-22f7215ec7f1', contemporary_uniform_civil_code_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('713a9f75-e152-4409-a1ea-22f7215ec7f1', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, civil_registration_authorities).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, spouses_seeking_gender_symmetric_rights).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_minorities_preferring_personal_law).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, parties_to_unregistered_customary_unions).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, state_supremacy_over_personal_status_law).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, marital_contractualism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers a uniform civil marriage statute that treats registration as the sole validity criterion, independent of religious ceremony. Adjudicates disputes, issues certificates, and enforces gender-symmetric rights and obligations (property, divorce grounds, maintenance) uniformly across all citizens who opt in or fall under its jurisdiction by default.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, civil_registration_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Marry across religious lines that no single personal-law system would fully recognize. The civil contract framework gives them a route to state-recognized union without either party converting or submitting to the other's religious authority. Their exit from religious personal-law jurisdiction is precisely what the constraint enables.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, mobile, national).

% Typically women seeking divorce grounds, maintenance, or property rights that are more favorable or more symmetric than under a given religious personal-law regime. They benefit from the secular contract's formal gender neutrality, though enforcement in practice still depends on courts and social pressure, and exit from a religious-law marriage into civil jurisdiction can carry social cost.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, spouses_seeking_gender_symmetric_rights, beneficiary,
    moderate, biographical, constrained, national).

% Communities that regard marriage as fundamentally a religious act governed by their own doctrinal authority experience the secular contractual regime as a state-imposed override that displaces community jurisdiction, especially where civil registration is treated as legally supreme over or a mandatory supplement to religious solemnization. They bear the cost of having their institution's own self-understanding subordinated to a competing legal framework not of their making.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_minorities_preferring_personal_law, payer,
    organized, generational, constrained, national).

% Couples married by community or religious rite but never civilly registered discover that the secular framework treats registration, not ceremony or cohabitation, as dispositive for state-recognized rights — inheritance, spousal maintenance, custody. They bear the cost of a validity rule they did not know governed them, often the economically weaker party (frequently women) in rural or low-literacy contexts.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, parties_to_unregistered_customary_unions, payer,
    powerless, biographical, trapped, national).

% Clergy and religious court functionaries whose role in solemnizing and adjudicating marriage is bypassed or subordinated by the civil framework. They are not parties to the state's registration process and have no formal say in how the secular contract's rules are set, though they retain informal social authority.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_authorities_and_clergy, excluded,
    organized, generational, constrained, national).

% Study how personal-status pluralism and state contractualism interact across jurisdictions, documenting where uniform civil codes reduce discrimination and where they erase minority self-governance without offering commensurate community input.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, comparative_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, civil_registration_authorities).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, state-enforceable legal status for marriage that does not depend on which religious tradition (if any) the parties belong to, enabling interfaith unions, uniform inheritance and custody adjudication, and consistent enforcement of rights regardless of community membership.
% TRANSFER_FUNCTION: Moves adjudicative authority over marital status, validity, and dissolution from religious community institutions to the state; correspondingly moves some individuals' practical rights protections upward (toward statutory gender-symmetric guarantees) while moving unregistered or community-married individuals' formal legal recognition downward (toward non-recognition) if they do not register.
% ABSENT_VOICES: Religious authorities and community elders who see marriage as constituted by rite, not registration, are not parties to the civil registration process and have no seat in setting its validity rules; economically marginal spouses in unregistered customary unions, who are most affected by the registration-as-sole-criterion rule, are rarely consulted in its design.
% DISAPPEARANCE_RATIONALE: If the secular civil contract option vanished, interfaith couples would lose their only route to state-recognized marriage without religious conversion, gender-symmetric statutory protections would revert entirely to whatever each personal-law system provides, and disputes currently adjudicated by secular family courts would default back to religious tribunals or become jurisdictionally contested.
% FOUNDING_PROBLEM: Colonial and post-colonial states inherited plural, community-specific personal-law systems that could not marry citizens across religious lines, offered uneven rights protections (especially to women), and left the state without a neutral forum to adjudicate marital disputes involving parties from different or no religious communities.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and law commissions (state bodies, not direct beneficiaries of the registration apparatus) have repeatedly affirmed the founding problem remains live, citing ongoing interfaith marriage litigation and unequal personal-law outcomes. Religious community associations, external to the civil registration authority, contest that the problem is overstated and that the secular framework's real effect is jurisdictional displacement rather than rights protection — this is documented in dissenting submissions to law reform commissions, not merely by parties who administer the registration system.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).
:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28 at interval end) because the coordination function — a neutral, gender-symmetric forum for marriage across religious lines — is genuine and substantial, not merely cover. It rises modestly over the interval as civil registration becomes the default legal reference point and unregistered customary unions are increasingly treated as legally invisible, a form of accumulating extraction from parties who never opted into the civil framework in any meaningful sense. Suppression is low-moderate (0.22): the secular reading does not coerce anyone into civil marriage over religious marriage, but does coerce recognition — an unregistered union simply does not exist for state purposes, which is a soft but real form of exclusion. Theater ratio is low (0.2): registration bureaucracy is mostly functional, though ceremonial aspects of state registration (witnessing, notarization theater) have grown somewhat over time without matching substantive change.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil registration authorities are the agenda-setting institutional seat with analytical exit (they administer, they do not personally bear costs of the system). Interfaith couples and rights-seeking spouses are structural beneficiaries — the constraint exists partly to serve them and derives low d (subsidized) from that. Religious minorities preferring personal law and unregistered customary-union parties are structural targets — the former lose institutional jurisdiction, the latter lose legal recognition outright — and derive high d. Religious authorities are excluded rather than coordinated: their absence from the rule-setting process is the point, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial-era jurisdictional fragmentation and unequal personal-law outcomes) remains partly live — interfaith couples and rights-disadvantaged spouses still need this route — which is why founding_problem_status is contested rather than dead. Classifying this as tangled_rope rather than pure rope or pure snare prevents two mislabeling errors: treating it as pure benign coordination (ignoring the real cost imposed on communities whose self-governance is displaced and on customary-union parties stripped of recognition), and treating it as pure extraction (ignoring the genuine and substantial coordination value for interfaith couples and gender-equality seekers who have no other route to symmetric rights).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    registration_as_recognition_vs_registration_as_supremacy,
    'Does the secular contractual reading function as a neutral, optional supplementary recognition layer alongside personal-law systems, or as a supremacy claim that subordinates religious marriage law wherever the two conflict?',
    'Examine whether courts in the jurisdiction treat civil registration as one valid recognition path among several (personal law remaining independently sufficient) or as legally dispositive over personal-law claims in property, custody, and maintenance disputes.',
    'If registration is genuinely optional and coexists with recognized personal-law marriage, this reading is closer to rope (coordination without displacement). If registration is treated as legally supreme, the tangled_rope classification is conservative and the extraction from displaced religious jurisdictions is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(registration_as_recognition_vs_registration_as_supremacy, empirical, 'Whether civil registration coexists with or supersedes religious personal-law recognition.').

omega_variable(
    gender_symmetry_formal_vs_substantive,
    'Does the statutory gender symmetry this reading provides translate into substantive outcome parity, or does social enforcement and unequal bargaining power within the civil system reproduce the asymmetries it formally eliminates?',
    'Compare divorce settlement outcomes, maintenance award rates, and custody outcomes for women under the civil contract framework versus under the personal-law systems it displaces, controlling for socioeconomic status.',
    'If substantive parity holds, the beneficiary classification for spouses_seeking_gender_symmetric_rights is well-founded. If social enforcement reproduces asymmetry despite formal statutory neutrality, the benefit is partly theatrical and theater_ratio is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_symmetry_formal_vs_substantive, empirical, 'Whether formal statutory gender symmetry produces real outcome parity.').

omega_variable(
    registration_access_barrier_nature,
    'Is the exclusion of unregistered customary-union parties from legal recognition a deliberate feature preserving state control over marital status, or an unintended consequence of administrative and literacy barriers that the state has not adequately remedied?',
    'Assess state investment in registration outreach, mobile registration units, and literacy-accessible registration processes relative to known rates of customary unmarried cohabitation.',
    'If deliberate, the treatment of unregistered-union parties as victims is well-founded and points toward snare-like features in this sub-population. If unintended and remediable, the harm is better characterized as a fixable administrative gap rather than structural extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(registration_access_barrier_nature, conceptual, 'Whether registration-access exclusion is structural extraction or remediable administrative gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fami_tr_t8, family_law_authority__secular_contractual_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(fami_tr_t16, family_law_authority__secular_contractual_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(fami_tr_t24, family_law_authority__secular_contractual_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(fami_tr_t32, family_law_authority__secular_contractual_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fami_be_t8, family_law_authority__secular_contractual_reading, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(fami_be_t16, family_law_authority__secular_contractual_reading, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(fami_be_t24, family_law_authority__secular_contractual_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement(fami_be_t32, family_law_authority__secular_contractual_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(fami_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(fami_su_t8, family_law_authority__secular_contractual_reading, suppression_requirement, 8, 0.17).
narrative_ontology:measurement(fami_su_t16, family_law_authority__secular_contractual_reading, suppression_requirement, 16, 0.18).
narrative_ontology:measurement(fami_su_t24, family_law_authority__secular_contractual_reading, suppression_requirement, 24, 0.19).
narrative_ontology:measurement(fami_su_t32, family_law_authority__secular_contractual_reading, suppression_requirement, 32, 0.21).
narrative_ontology:measurement(fami_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(family_law_authority__secular_contractual_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language concept 'marriage as a legal institution' under the family_law_authority kernel. Each sibling reading (secular_contractual, hindu_dharmashastra, muslim_shariat, christian_canonical, parsi_zoroastrian) is authored as an independent constraint with its own ε, beneficiary/victim structure, and claimed type, per the ε-invariance principle — the kernel itself (what validates a marriage, who has authority to solemnize and dissolve it) is read differently by each tradition and by the state, and these differences are structural, not merely observational. This reading tends to exert downstream pressure on the others by making civil registration a practical precondition for state-enforceable rights even where religious solemnization is independently valid, without logically foreclosing any of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
