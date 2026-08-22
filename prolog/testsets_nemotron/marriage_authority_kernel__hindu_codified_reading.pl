% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Act 1955 Authority as Interpreted by Civil Courts
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Hindu Marriage Act 1955 codified Hindu marriage law as a uniform
 *   statute for all Hindus (including Buddhists, Jains, Sikhs), interpreted
 *   by civil courts. It abolished polygamy, introduced divorce on specified
 *   grounds, and granted women limited rights to maintenance and guardianship
 *   — a major reform over uncodified custom. However, the Act preserved
 *   patriarchal structures: coparcenary property rights remained
 *   male-dominated (partially reformed only in 2005), guardianship defaults
 *   to father, and the law's 'Hindu' identity excludes LGBTQ marriages. State
 *   courts became the sole authoritative interpreters, displacing diverse
 *   customary bodies. The constraint presents as coordination (uniform law,
 *   state enforcement) but extracts asymmetrically: male householders and
 *   dominant castes gain legal authority; women, lower castes, and LGBTQ
 *   individuals bear the gaps. Community leaders and the judiciary benefit
 *   institutionally. The claimed type is tangled_rope — genuine coordination
 *   function married to asymmetric extraction requiring active judicial
 *   enforcement to hold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.38).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 Authority as Interpreted by Civil Courts").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '6d883202-1601-4647-a92b-52b4fe48b9dc').
narrative_ontology:cs_kernel_codification('6d883202-1601-4647-a92b-52b4fe48b9dc', formalized).
narrative_ontology:cs_authority_grounding('6d883202-1601-4647-a92b-52b4fe48b9dc', lineage).
narrative_ontology:cs_interpretation_layer_present('6d883202-1601-4647-a92b-52b4fe48b9dc').
narrative_ontology:cs_reading_relation('6d883202-1601-4647-a92b-52b4fe48b9dc', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d883202-1601-4647-a92b-52b4fe48b9dc', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d883202-1601-4647-a92b-52b4fe48b9dc', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d883202-1601-4647-a92b-52b4fe48b9dc', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('6d883202-1601-4647-a92b-52b4fe48b9dc', foundational, hindu_law_codification_as_authentic_dharma).
narrative_ontology:cs_axiom_status(hindu_law_codification_as_authentic_dharma, holdable).
narrative_ontology:cs_axiom_grounding('6d883202-1601-4647-a92b-52b4fe48b9dc', hindu_law_codification_as_authentic_dharma, conventional).
narrative_ontology:cs_axiom('6d883202-1601-4647-a92b-52b4fe48b9dc', foundational, state_courts_as_legitimate_interpreters_of_hindu_law).
narrative_ontology:cs_axiom_status(state_courts_as_legitimate_interpreters_of_hindu_law, holdable).
narrative_ontology:cs_axiom_grounding('6d883202-1601-4647-a92b-52b4fe48b9dc', state_courts_as_legitimate_interpreters_of_hindu_law, conventional).
narrative_ontology:cs_reference_frame('6d883202-1601-4647-a92b-52b4fe48b9dc', post_independence_hindu_law_reform_settlement).
narrative_ontology:cs_drift_state('6d883202-1601-4647-a92b-52b4fe48b9dc', contemporary_constitutional_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6d883202-1601-4647-a92b-52b4fe48b9dc', '2026-06-20T12:00:00Z').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_lgbtq_individuals).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, codified_religious_law_supremacy_in_personal_law).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__hindu_codified_reading, state_judiciary_as_authentic_interpreter_of_dharma).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy preferential rights in marriage dissolution, property, guardianship under the codified Act. Their authority in the household is legally reinforced. They bear costs of litigation and social conformity but net benefit from the structure. Exit would mean renouncing community membership and legal protections.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_male_householders, payer).

% Gain some protections under the codified Act (monogamy, divorce grounds, maintenance) that did not exist in uncodified custom, but remain structurally disadvantaged in property rights, guardianship, and divorce access. Their identity as Hindu women makes exit from the personal law system nearly unthinkable — secular law is an option but socially stigmatized.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_women, beneficiary).

% Excluded entirely from marriage recognition under the Act. The codified law presumes heterosexual marriage. No exit within the personal law system; secular Special Marriage Act is technically available but socially and familially blocked. Bear full exclusion cost with zero benefit.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_lgbtq_individuals, payer,
    powerless, biographical, trapped, national).

% Formally covered by the uniform Act but customary practices often override statutory rights. Court access is costly and biased. Benefit from codification's anti-discrimination clauses on paper but extraction persists through customary non-compliance. Exit to secular law exists but community ostracism is a real barrier.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindus, payer,
    moderate, biographical, constrained, national).

% Hold interpretive monopoly over the Act. Their readings define what 'Hindu law' means in practice. Benefit from institutional authority, case volume, and legitimacy as guardians of a 'reformed' tradition. Can move between personal law benches and general civil jurisdiction — high exit optionality.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_court_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious and social leaders who mobilize around 'protecting Hindu law' from secular encroachment. Gain political capital and community control. Their authority depends on the Act remaining the exclusive regime for Hindus. Exit would mean losing the platform for communal politics.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders, beneficiary,
    organized, generational, constrained, national).

% Advocate for a Uniform Civil Code or expanded Special Marriage Act. Are structurally excluded from the personal law adjudication process. Their voices appear in legislative debates and PILs but not in the routine interpretation of the Act. Can campaign externally but cannot change the constraint from within.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_rights_activists, excluded,
    organized, generational, mobile, national).

% Analyze the kernel's readings across communities. Track how codification, judicial interpretation, and constitutional challenges reshape the authority structure. No material stake in any reading's victory.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, legal_scholars_constitutional, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, state-enforced framework for Hindu marriage, divorce, maintenance, and guardianship — replacing fragmented, uncodified customs with a single statute adjudicated by civil courts, enabling legal certainty and interoperability with secular institutions.
% TRANSFER_FUNCTION: Transfers interpretive authority from diverse customary bodies to state courts; transfers substantive rights from women and marginalized castes to male householders and dominant-caste norms; transfers political legitimacy to community leaders who claim guardianship of the codified tradition.
% ABSENT_VOICES: Hindu women's groups demanding full equality within personal law; Dalit and Adivasi organizations challenging customary overrides; LGBTQ collectives demanding marriage recognition; all are structurally excluded from the Act's interpretive process and appear only in reform petitions.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act vanished overnight, Hindu marriage law would revert to uncodified custom (highly variable, caste-differentiated) or default to the secular Special Marriage Act. Courts would lose their primary personal law docket. Community leaders would lose their legal platform. Women would lose statutory protections (however incomplete). The legal landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: Pre-1955 Hindu marriage law was uncodified, diverse across regions and castes, and offered women virtually no exit from marriage. The Act was built to provide a uniform, reformed statutory framework that abolished polygamy, introduced divorce, and gave women limited rights — while preserving the 'Hindu' character of the law against a secular code.
% FOUNDING_PROBLEM_CORROBORATION: The state and dominant community leaders attest the problem is live — custom still threatens uniformity, and secular encroachment threatens Hindu identity. Women's rights organizations and Dalit groups attest the problem is dead for the privileged but live for the marginalized — the Act's protections are real but its equity promise is unfulfilled. No single corroborator outside the beneficiary set endorses the official narrative wholly.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the Act's dual character: it delivers real coordination (uniformity, state enforcement, some women's rights) but extracts from women, lower castes, and LGBTQ persons through substantive gaps. Suppression (0.38) is moderate: the Act is state law, not private enforcement, but customary non-compliance and social pressure to stay within personal law act as soft suppression. Theater (0.28) has risen as courts increasingly perform 'protecting Hindu law' while delivering incremental reforms. Accessibility collapse (0.45) is partial: alternatives exist (Special Marriage Act, constitutional challenges) but are socially costly. Resistance (0.52) is significant: women's groups, LGBTQ collectives, and Dalit organizations actively contest the Act's gaps. Measurements use a shared 7-point grid from 1955 to 2024.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the Act is a successful coordination mechanism: uniform law, constitutional compliance, incremental reform. From Hindu women's seat, it is a compromised reform that locks in patriarchal defaults while offering just enough to prevent systemic rejection. From LGBTQ seat, it is a snare — total exclusion disguised as community law. The engine will compute these divergences from the structural data; the authored claim (tangled_rope) captures the macro pattern but seat-level types will differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu male householders are net beneficiaries (d ~ 0.25) — they gain legal authority in marriage, property, guardianship with constrained exit (leaving community is costly). Hindu women are net payers (d ~ 0.75) — they gain some statutory rights but remain structurally disadvantaged; identity_locked exit makes secular law socially inaccessible. Hindu LGBTQ individuals are full targets (d ~ 0.95) — total exclusion, trapped exit. Lower-caste Hindus are payers (d ~ 0.7) — formal inclusion but customary override; constrained exit. Civil court judiciary are agenda_setters with arbitrage exit (d ~ 0.15) — they control interpretation and can rotate benches. Community leaders are beneficiaries with constrained exit (d ~ 0.3) — their platform depends on the Act's exclusivity. Secular activists are excluded (d ~ 0.5 symmetric) — they operate outside the constraint. Scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uniformity, women's minimal rights) is contested: partially solved for privileged women, unsolved for marginalized. The constraint persists not because the problem is live, but because the Act's 'Hindu' identity makes it a site of communal politics — community leaders and the judiciary benefit from its maintenance. Mandatrophy is unresolved: the coordination function (uniformity) is real but the extraction function (gender/caste/sexuality asymmetry) has outlived even the reformist justification. The 2005 amendment (daughters as coparceners) shows reform is possible but the core asymmetry remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    codification_vs_custom_persistence,
    'To what extent does the Act''s statutory uniformity actually displace customary practice, versus creating a dual regime where statute governs court cases but custom governs lived reality?',
    'Empirical study of court filings vs. community dispute resolution; comparison of statutory rights claimed vs. rights realized across caste/region.',
    'If custom substantially overrides statute, the coordination function is weaker than claimed and extraction from lower-caste women is higher — the constraint operates as a snare for them. If statute dominates, the tangled_rope classification holds with coordination as the primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(codification_vs_custom_persistence, empirical, 'Whether codification achieved its stated uniformity or created a performative layer over persistent custom.').

omega_variable(
    judicial_reform_capacity,
    'Can civil courts, as the designated interpreters, drive the Act toward gender equality through interpretation, or are they structurally constrained by the Act''s ''Hindu'' framing and communal politics?',
    'Longitudinal analysis of Supreme Court and High Court judgments on HMA provisions since 1955; tracking doctrinal trajectory vs. legislative amendments.',
    'If courts can and do drive equality, the extraction component may decay over time (tangled_rope -> rope trajectory). If courts are blocked by communal-political constraints, extraction is structural and persistent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_reform_capacity, conceptual, 'Whether the interpretive monopoly is a site of progressive evolution or communal entrenchment.').

omega_variable(
    secular_alternative_viability,
    'Is the Special Marriage Act a genuine exit option for Hindus, or is its social cost (family ostracism, community exclusion) so high that it functions only as a theoretical alternative?',
    'Sociological data on SMA usage rates by community; qualitative studies of couples who choose SMA vs. HMA; tracking social consequences.',
    'If SMA is a viable exit, Hindu women''s identity_locked status weakens toward constrained — directionality shifts, effective extraction drops. If SMA is socially blocked, identity_locked holds and extraction is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_alternative_viability, empirical, 'Whether the secular alternative provides real exit or merely theoretical escape.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t1976, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t1985, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2001, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2005, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2005, 0.26).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2013, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.35).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t1976, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1976, 0.38).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t1985, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2001, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2005, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2013, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2013, 0.41).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.3).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t1976, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1976, 0.32).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t1985, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2001, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2001, 0.36).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2005, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2013, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2013, 0.38).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, uniform_civil_code_debate).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, special_marriage_act_1954).

% DUAL FORMULATION NOTE:
% This reading is one of five in the marriage_authority_kernel family. The kernel is the authority structure for marriage/family law in India's constitutional pluralism. Each reading instantiates a different constraint with distinct ε, beneficiaries, victims, and coordination-extraction balance. This reading (hindu_codified) has moderate extractiveness (0.42) with state-court adjudication — higher coordination than muslim_shariat_reading (uncodified, board-adjudicated), lower equity than secular_civil_reading (constitutional individual rights). All five stories link via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, organized, 0.3).
constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, moderate, 0.7).
constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
