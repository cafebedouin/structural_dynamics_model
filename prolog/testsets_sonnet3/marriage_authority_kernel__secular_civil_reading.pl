% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Special Marriage Act 1954 — Secular Civil Reading of Marriage Authority
 *   domain: legal/religious governance
 *
 * SUMMARY:
 *   This story instantiates the secular civil reading of the marriage
 *   authority kernel in India: the Special Marriage Act 1954, grounded in
 *   constitutional individual-rights doctrine, functions as an alternative
 *   jurisdiction that lets citizens marry without personal-law mediation. The
 *   Act genuinely coordinates a problem no personal-law regime solves
 *   (inter-religious and inter-caste marriage without conversion) but layers
 *   a real cost — the mandatory 30-day public notice — onto exactly the
 *   couples most likely to face family or community coercion for making that
 *   choice, and imposes social exit costs on those who use it. This is why
 *   the story authors as tangled_rope rather than a clean rope: genuine
 *   coordination function, plus an asymmetric extraction mechanism
 *   (notice-period exposure, loss of community standing) that requires the
 *   state's active enforcement apparatus (registrars, courts) to operate.
 *
 * KEY AGENTS:
 *   - civil_courts_and_marriage_registrars: administer and enforce the secular path
 *   - inter_religious_couples: primary intended beneficiaries
 *   - women_seeking_civil_remedies: gain gender-equal legal terrain
 *   - notice_period_exposed_couples: bear the procedural exposure the Act's own mechanism creates
 *   - couples_facing_community_ostracism: bear diffuse long-term social costs of exit
 *   - personal_law_boards_and_religious_authorities: excluded from adjudication, lobby against the pathway
 *   - constitutional_courts: analytical observer testing the Act's own constitutionality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.38).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.42).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Special Marriage Act 1954 — Secular Civil Reading of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "legal/religious governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, '68d67286-cecb-43ae-8696-6c884383e6bb').
narrative_ontology:cs_kernel_codification('68d67286-cecb-43ae-8696-6c884383e6bb', formalized).
narrative_ontology:cs_authority_grounding('68d67286-cecb-43ae-8696-6c884383e6bb', expertise).
narrative_ontology:cs_interpretation_layer_present('68d67286-cecb-43ae-8696-6c884383e6bb').
narrative_ontology:cs_reading_relation('68d67286-cecb-43ae-8696-6c884383e6bb', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('68d67286-cecb-43ae-8696-6c884383e6bb', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('68d67286-cecb-43ae-8696-6c884383e6bb', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('68d67286-cecb-43ae-8696-6c884383e6bb', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('68d67286-cecb-43ae-8696-6c884383e6bb', foundational, constitutional_individual_right_supersedes_community_marriage_jurisdiction).
narrative_ontology:cs_axiom_status(constitutional_individual_right_supersedes_community_marriage_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('68d67286-cecb-43ae-8696-6c884383e6bb', constitutional_individual_right_supersedes_community_marriage_jurisdiction, deontological).
narrative_ontology:cs_axiom('68d67286-cecb-43ae-8696-6c884383e6bb', secondary, marriage_eligibility_must_be_religion_blind).
narrative_ontology:cs_axiom_status(marriage_eligibility_must_be_religion_blind, holdable).
narrative_ontology:cs_axiom_grounding('68d67286-cecb-43ae-8696-6c884383e6bb', marriage_eligibility_must_be_religion_blind, conventional).
narrative_ontology:cs_reference_frame('68d67286-cecb-43ae-8696-6c884383e6bb', constitutional_individual_rights_framework).
narrative_ontology:cs_drift_state('68d67286-cecb-43ae-8696-6c884383e6bb', contemporary_privacy_jurisprudence_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('68d67286-cecb-43ae-8696-6c884383e6bb', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, individuals_exiting_community_law).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_civil_remedies).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, couples_facing_community_ostracism).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, notice_period_exposed_couples).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, constitutional_supremacy_over_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__secular_civil_reading, individual_rights_as_marriage_ground).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer registration, the mandatory 30-day public notice procedure, and adjudicate disputes under the Special Marriage Act. They set and enforce the secular procedural path that bypasses religious authorities entirely, treating marriage as a civil contract between two constitutionally equal individuals rather than a sacrament or community-regulated status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts_and_marriage_registrars, agenda_setter,
    institutional, generational, analytical, national).

% Use the Act as the only legal path to marry across religious lines without either party converting. Gain state-recognized union and access to civil courts for property, maintenance, and divorce on gender-neutral grounds unavailable under most personal laws. The benefit is real but purchased through public exposure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, constrained, national).

% Gain equal-grounds divorce, inheritance under the Indian Succession Act rather than a personal-law regime that may disadvantage them, and maintenance provisions interpreted through constitutional equality doctrine rather than community custom. For many, the civil register is the only forum where their claim is heard on gender-neutral terms.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_civil_remedies, beneficiary,
    moderate, biographical, constrained, national).

% Must submit to a 30-day public notice displayed at the registrar's office, historically enabling families, community vigilante groups, and in some states police 'moral policing' units to locate and intervene against the marriage before it is solemnized. The very mechanism that grants secular legal access also creates the window in which coercion, family violence, or forced separation occurs.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, notice_period_exposed_couples, payer,
    powerless, immediate, trapped, regional).

% Having exited their community's personal-law framework to marry under the secular Act, they lose standing in caste or religious community structures — inheritance expectations, ritual participation, family financial support, and marriage-market access for siblings can all be withdrawn. The state protects the legal marriage but does not compensate for the social capital lost.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, couples_facing_community_ostracism, payer,
    powerless, biographical, trapped, local).

% Have no adjudicatory role once a couple elects the secular path; they experience the Act as a jurisdictional bypass eroding their community's regulatory authority over marriage. They are not party to the civil proceeding but actively lobby against expanding its use and against removing the notice-period provision.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, personal_law_boards_and_religious_authorities, excluded,
    organized, generational, mobile, national).

% Adjudicate challenges to the notice-period provision (Supreme Court and High Court petitions have argued it violates privacy and autonomy) and periodically test whether the secular framework's own procedures are consistent with the constitutional rights it claims to vindicate.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__secular_civil_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__secular_civil_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single civil registration and adjudication pathway for marriage that does not require either party to belong to, convert to, or be governed by a specific religious community — solving the coordination problem of inter-faith and inter-caste unions for which no religious personal law offers a shared forum.
% TRANSFER_FUNCTION: Moves adjudicatory authority over marriage, divorce, inheritance, and maintenance away from religious community structures and family/caste councils toward the state, and moves procedural exposure (the 30-day public notice) onto the couple seeking to use that pathway.
% ABSENT_VOICES: Couples who abandoned the civil route after community intimidation during the notice period are not visible in registrar records — their attempted use of the Act left no trace once withdrawn. Religious authorities argue their community's own dispute-resolution competence is bypassed without a hearing; they are not represented in the registration process itself.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act vanished, inter-religious and inter-caste couples would lose the only marriage path not mediated by a religious authority; conversions to enable personal-law marriage would rise, community leverage over marriage choice would strengthen substantially, and constitutional-rights-based divorce and inheritance claims would revert to whichever personal law applied by birth.
% FOUNDING_PROBLEM: Post-independence India needed a marriage law that could hold across religious communities consistent with Article 21 and Article 15 constitutional guarantees, and that did not require citizens to either convert or remain trapped inside a single community's marriage rules to exercise the right to marry the person of their choice.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts (Supreme Court rulings on the Shafin Jahan case and subsequent privacy/autonomy jurisprudence) attest the founding problem — protecting the individual's constitutional right to marry across community lines — remains live and unresolved by personal law regimes. Women's legal aid organizations, operating outside the registrar system that administers the Act, independently corroborate that the notice-period exposure is a live harm rather than settled procedure, supporting reform petitions rather than the registrars' own framing of the notice as neutral administrative process.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.38) and rising only slightly over seven decades: this is a functioning coordination mechanism, not primarily an extraction vehicle, but the notice-period requirement has never been removed despite documented harassment/intimidation harms, so a small persistent extraction sits on top of the coordination function. Suppression (0.42) reflects that using this pathway at all requires public exposure that religious-law marriage does not — a structural cost of choosing the constitutional route. Accessibility_collapse is low-moderate (0.35): personal-law alternatives remain fully available; nothing collapses them, the couple retains a genuine choice, which is itself part of why this differs from the personal-law siblings. Resistance (0.55) is comparatively high because personal law boards and community structures actively resist the pathway's use and lobby against notice-period reform in opposite directions (some for removing it as a rights violation, some for retaining it as a community safeguard).
 *
 * PERSPECTIVAL GAP:
 *   From the registrar/court seat, the Act is a neutral procedural safeguard ensuring no bigamous or coerced marriage slips through — administrative due diligence. From the notice-period-exposed couple's seat, the identical 30-day public window is the single point of maximum vulnerability in the entire process, the moment their families or community can mobilize against them. The engine's per-seat computation should register this asymmetry structurally rather than resolve it toward either seat's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Inter-religious couples and women seeking gender-equal remedies sit near the beneficiary end: the Act subsidizes a right otherwise unavailable to them. Notice-period-exposed couples and community-ostracized couples sit near the target end: the same registration mechanism that grants them legal recognition is the mechanism that exposes them to coercion or social cost — d is high specifically because their exit from the extractive exposure (withdrawing the application) also means losing the coordination benefit entirely, a genuine trap rather than a free choice once the process begins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional right to inter-community marriage) remains fully live per the R5 interview, which forecloses classifying this as a piton or mandatrophy case — the coordination function is not vestigial. The tangled_rope classification prevents mislabeling this as pure extraction (a snare reading would ignore the genuine rights-expanding coordination function it performs for tens of thousands of couples annually) while also preventing a false rope classification that would ignore the documented, persistent notice-period harm that the state has had opportunities to fix (via in-camera or reduced-notice reform) and largely has not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notice_period_necessity_vs_extraction,
    'Is the 30-day public notice requirement a necessary safeguard against bigamy and coercion, or is it a vestigial colonial-era procedural artifact that now functions primarily as an extraction/exposure mechanism with no offsetting verification benefit given modern identity/document verification capacity?',
    'Comparative analysis of bigamy/coercion detection rates attributable specifically to the public notice period versus rates in jurisdictions with private registration and modern document verification; Law Commission of India review data; Supreme Court petition outcomes (e.g. petitions arguing the provision violates Article 21 privacy).',
    'If the notice serves negligible verification function relative to modern alternatives, the extraction/suppression scores here understate the constraint''s actual extractive character and the classification should move toward snare-adjacent; if it materially prevents bigamous or coerced marriages, the tangled_rope classification''s coordination component is stronger than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notice_period_necessity_vs_extraction, empirical, 'Whether the notice-period mechanism is functionally necessary or an extraction artifact.').

omega_variable(
    kernel_disaggregation_locus,
    'Where exactly does the marriage_authority_kernel disagreement live — is it about WHO has authority to solemnize/dissolve marriage (institutional locus), or about WHAT SUBSTANTIVE RULES apply (grounds for divorce, inheritance shares, minimum age, consent standards)? The secular_civil_reading changes both simultaneously, which makes it harder to isolate which structural delta drives the equity gains versus which drives the exit costs.',
    'Decompose the substantive-rules delta from the institutional-locus delta by comparing outcomes for couples who use the Special Marriage Act''s registration mechanism but whose substantive dispute (e.g. inheritance) still gets adjudicated with reference to a personal law choice-of-law rule, versus couples for whom both locus and substance are fully secular.',
    'If the equity gains are substantive-rule-driven rather than institutional-locus-driven, other kernel readings could in principle adopt the substantive equity provisions without adopting full secular jurisdiction — meaning the sibling readings'' equity deficits are not structurally necessary consequences of retaining religious authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_disaggregation_locus, conceptual, 'Whether this reading''s structural delta is institutional (who decides) or substantive (what rules), and whether the two are separable.').

omega_variable(
    exit_cost_internalization,
    'Is the social/community cost borne by couples who exit personal-law jurisdiction (ostracism, loss of inheritance standing, marriage-market damage to siblings) properly attributable to the secular_civil_reading constraint itself, or to the sibling community-law constraints'' own enforcement of exit penalties?',
    'Trace whether ostracism outcomes are administered/threatened by the community institutions governing the sibling personal-law readings (in which case the cost belongs on those siblings'' ledgers) versus arising from the secular Act''s own structure (in which case it belongs here).',
    'Reassigning this cost to the sibling constraints rather than this one would lower this story''s authored extractiveness and victim set, since the community-ostracism payer group''s harm originates in the enforcement machinery of the personal-law kernel readings, not in the Special Marriage Act''s own operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_internalization, conceptual, 'Whether exit-cost harms belong to this reading''s ledger or to the sibling readings whose community enforcement produces them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1990, 0.17).
narrative_ontology:measurement(marr_tr_t2006, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement(marr_tr_t2018, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2018, 0.21).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.3).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(marr_be_t1990, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1990, 0.34).
narrative_ontology:measurement(marr_be_t2006, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2006, 0.36).
narrative_ontology:measurement(marr_be_t2018, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2018, 0.37).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1970, 0.33).
narrative_ontology:measurement(marr_su_t1990, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(marr_su_t2006, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2006, 0.4).
narrative_ontology:measurement(marr_su_t2018, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2018, 0.41).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__secular_civil_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This story is one of five siblings decomposing the natural-language concept 'marriage/family law authority in India' per the ε-invariance principle. Each personal-law reading (Hindu, Muslim, Christian, Parsi) and this secular civil reading ground marriage authority in structurally distinct institutions with distinct victim sets, distinct ε values, and distinct enforcement mechanisms. They are linked here rather than merged because measuring 'marriage law authority' against the observable 'gender equity of divorce grounds' versus 'availability of inter-community marriage' versus 'community continuity' yields different rankings across the five readings — exactly the signal that indicates five constraints, not one measured five ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
