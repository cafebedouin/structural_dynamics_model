% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Hindu Marriage Act Authority as Interpreted by Civil Courts
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   The Hindu Marriage Act 1955 codified Hindu personal law into a uniform
 *   statute administered by civil courts. This reading of the
 *   marriage_authority_kernel instantiates state-enforced religious law: the
 *   Act's provisions on marriage validity, divorce grounds, maintenance,
 *   guardianship, and succession are authoritatively interpreted by the
 *   Supreme Court and High Courts. The constraint coordinates a uniform
 *   framework for ~80% of India's population but extracts gendered and
 *   caste-asymmetric costs. The claimed type is tangled_rope: genuine
 *   coordination (uniform justiciable rules) coexists with asymmetric
 *   extraction (women, lower castes, interfaith/LGBTQ parties bear
 *   disproportionate burdens). Metrics reflect the 2024 endpoint; the
 *   measurement series shows extractiveness declining 1955-2015 (progressive
 *   amendments, judicial expansion of women's rights) then rising 2015-2024
 *   (stalling reforms, anti-conversion laws, same-sex marriage denial).
 *   Theater ratio rises as coordination function (uniform adjudication) is
 *   increasingly performed while substantive equity gains plateau.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.58).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.52).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act Authority as Interpreted by Civil Courts").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'f37fd472-0533-4922-a493-918b163d2ce3').
narrative_ontology:cs_kernel_codification('f37fd472-0533-4922-a493-918b163d2ce3', formalized).
narrative_ontology:cs_authority_grounding('f37fd472-0533-4922-a493-918b163d2ce3', lineage).
narrative_ontology:cs_interpretation_layer_present('f37fd472-0533-4922-a493-918b163d2ce3').
narrative_ontology:cs_reading_relation('f37fd472-0533-4922-a493-918b163d2ce3', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('f37fd472-0533-4922-a493-918b163d2ce3', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('f37fd472-0533-4922-a493-918b163d2ce3', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f37fd472-0533-4922-a493-918b163d2ce3', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('f37fd472-0533-4922-a493-918b163d2ce3', foundational, codified_hindu_law_governs_hindu_marriage).
narrative_ontology:cs_axiom_status(codified_hindu_law_governs_hindu_marriage, holdable).
narrative_ontology:cs_axiom_grounding('f37fd472-0533-4922-a493-918b163d2ce3', codified_hindu_law_governs_hindu_marriage, conventional).
narrative_ontology:cs_axiom('f37fd472-0533-4922-a493-918b163d2ce3', foundational, state_courts_authoritatively_interpret_hindu_law).
narrative_ontology:cs_axiom_status(state_courts_authoritatively_interpret_hindu_law, holdable).
narrative_ontology:cs_axiom_grounding('f37fd472-0533-4922-a493-918b163d2ce3', state_courts_authoritatively_interpret_hindu_law, conventional).
narrative_ontology:cs_axiom('f37fd472-0533-4922-a493-918b163d2ce3', secondary, gender_equity_within_hindu_law_is_legislative_not_judicial).
narrative_ontology:cs_axiom_status(gender_equity_within_hindu_law_is_legislative_not_judicial, holdable).
narrative_ontology:cs_axiom_grounding('f37fd472-0533-4922-a493-918b163d2ce3', gender_equity_within_hindu_law_is_legislative_not_judicial, empirically_contingent).
narrative_ontology:cs_reference_frame('f37fd472-0533-4922-a493-918b163d2ce3', codified_hindu_law_framework).
narrative_ontology:cs_drift_state('f37fd472-0533-4922-a493-918b163d2ce3', contemporary_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f37fd472-0533-4922-a493-918b163d2ce3', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_male_heirs).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, dominant_caste_hindu_families).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, state_courts).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_religious_institutions).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindus).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples_hindu_partner).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_lgbtq_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from unequal inheritance rights, easier divorce access, and guardianship preference under HMA. Their position is reinforced by judicial interpretation that treats male lineage as primary. Exit to secular code (Special Marriage Act) requires family consent and social ostracism risk.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_male_heirs, beneficiary,
    organized, biographical, constrained, national).

% Bear unequal divorce grounds (cruelty vs desertion asymmetries), limited maintenance rights, secondary guardianship, and restricted property claims. Judicial interpretation has expanded rights incrementally (2005 amendment, 2020 coparcenary ruling) but structural asymmetries persist. Exit via Special Marriage Act faces family/community pressure.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    moderate, biographical, constrained, national).

% Adjudicate and interpret HMA provisions, creating binding precedent. Supreme Court and High Courts have expanded women's rights incrementally while maintaining the codified religious law framework. They hold institutional authority to define the content of 'Hindu law' and their interpretations are enforced by state machinery.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, state_courts, agenda_setter,
    institutional, generational, analytical, national).

% Use HMA framework to consolidate property and status within caste endogamy. Court enforcement of restitution of conjugal rights and guardianship serves caste boundary maintenance. They have resources to navigate litigation and shape judicial outcomes through precedent.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, dominant_caste_hindu_families, beneficiary,
    powerful, generational, mobile, regional).

% Mathas, temples, and caste associations intervene in litigation (amicus, party) to defend 'Hindu law' authenticity. They benefit from state recognition of religious authority over family. Their interpretive claims constrain judicial innovation, though courts occasionally override them.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_religious_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__hindu_codified_reading, hindu_religious_institutions, agenda_setter).

% Customary marriage/divorce practices overridden by codified HMA which reflects dominant-caste norms. Lose community-based dispute resolution and face court procedures biased toward propertied litigants. Conversion to Buddhism/Christianity sometimes used as exit but triggers new legal disabilities.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, lower_caste_hindus, payer,
    moderate, biographical, constrained, regional).

% Hindu partner faces pressure to convert non-Hindu spouse or marry under Special Marriage Act (30-day notice enables family/community interference). HMA does not recognize interfaith marriage; state courts enforce this boundary. Anti-conversion laws in several states compound the constraint.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples_hindu_partner, payer,
    moderate, biographical, constrained, national).

% HMA defines marriage as heterosexual sacrament; no recognition of same-sex or trans marriages. 2018 Navtej decriminalization did not extend to marriage. Special Marriage Act similarly excludes. Litigation pending (Supreme Court 2023) but legislative route blocked. No functional exit within Indian law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_lgbtq_individuals, payer,
    powerless, biographical, trapped, national).

% Produce doctrinal critiques, empirical studies of judicial outcomes, and law reform proposals. Document how HMA interpretation reproduces gender hierarchy despite formal equality guarantees. Their work informs litigation strategy and legislative advocacy but they hold no adjudicative power.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, feminist_legal_scholars, observer,
    analytical, generational, analytical, national).

% Administer parallel Muslim personal law system (Shariat Application Act 1937). Would object to uniform civil code that HMA model might generalize. Excluded from HMA adjudication but structurally positioned as sibling reading in the kernel contest. Their authority derives from community recognition, not state codification.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, muslim_personal_law_board, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides uniform, state-enforceable rules for Hindu marriage, divorce, maintenance, guardianship, and succession — replacing fragmented customs with a single legislated code administrable by civil courts.
% TRANSFER_FUNCTION: Transfers gendered rights (unilateral divorce access, maintenance quantum, guardianship preference, coparcenary property) from women to men; transfers dispute-resolution authority from community/kin to state courts; transfers interpretive authority from religious texts to judicial precedent.
% ABSENT_VOICES: Muslim women under uncodified shariat (triple talaq until 2019, polygamy, unequal inheritance); Christian women under 1872 Act (limited divorce grounds until 2001); Parsi women under 1936 Act (gender asymmetry in adultery grounds); secular couples under Special Marriage Act (30-day notice enables harassment); all would object to their respective constraints but are not parties to HMA adjudication.
% DISAPPEARANCE_RATIONALE: If HMA and its judicial interpretation vanished overnight, Hindu marriage/divorce would revert to uncodified customs (highly variable, caste-specific, no uniform court enforcement) or shift to Special Marriage Act (secular but procedurally burdensome). Gender equity outcomes would diverge: some customs more egalitarian, others less. State courts would lose primary jurisdiction over Hindu family disputes.
% FOUNDING_PROBLEM: Post-independence need to codify Hindu personal law into a uniform, modern statute administrable by secular courts — replacing uncodified smriti texts and diverse customs that British courts had selectively enforced — to fulfill constitutional directive (Article 44) while respecting religious identity (Articles 25-26).
% FOUNDING_PROBLEM_CORROBORATION: Constituent Assembly debates (1948-50) and Hindu Code Bill debates (1951-55) record the legislative intent: Ambedkar, Nehru, and progressive members framed codification as gender justice; conservative members opposed it as religious interference. Feminist historians (Agnes 2001, Dhagamwar 1992, Vatuk 2013) attest the founding problem was gender reform within religious framework — not mere administrative uniformity. Judicial records (Shah Bano 1985, Sarla Mudgal 1995, Shayara Bano 2017) show the contest persists.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects persistent gender asymmetry in divorce grounds (cruelty vs desertion), maintenance (husband's duty but wife's income counted), guardianship (father primary), and property (2005 amendment equalized daughters but implementation lags). Suppression (0.52) comes from state enforcement of religious boundary: HMA applies automatically to Hindus (broadly defined), Special Marriage Act exit requires 30-day public notice enabling interference, anti-conversion laws penalize religious switching. Theater (0.38) captures the gap between judicial rhetoric of 'gender justice' and doctrinal conservatism (e.g., 2023 same-sex marriage ruling deferring to legislature). Accessibility collapse (0.58): alternatives exist (customary law, SMA, conversion) but each carries high social/legal cost. Resistance (0.48): sustained feminist litigation and legislative advocacy but institutionalized within the framework.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state courts) computes as rope/tangled_rope: they see genuine coordination function (uniform adjudication, incremental rights expansion) and experience the constraint as institutional mandate. The payer seats (women, lower castes, interfaith/LGBTQ) compute as snare/tangled_rope: same structure operates as enforced extraction with suppressed alternatives. The beneficiary seats (male heirs, dominant castes, religious institutions) compute as rope: they experience net coordination benefit. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu male heirs and dominant-caste families are structural beneficiaries (d ~0.2-0.3): they collect inheritance, divorce, and guardianship advantages; exit to SMA is low-cost for them. Hindu women are primary targets (d ~0.7-0.8): bear extraction, constrained exit (family pressure, child custody risk). State courts sit near symmetric (d ~0.5): they administer the constraint and gain institutional authority but face legitimacy challenges. Religious institutions are beneficiaries (d ~0.25): state recognition of their interpretive claims. Lower-caste Hindus and interfaith/LGBTQ parties are trapped targets (d ~0.85-0.9): no functional exit, structural exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (codify Hindu law for gender justice + uniform administration) is contested: progressive framers say the problem persists (gender equity incomplete); conservative framers say codification itself was the error (religious autonomy violated); originalist framers say the problem was solved by 1955/2005 amendments. The constraint persists because no coalition can agree on replacement: UCC advocates want secular code; religious conservatives want uncodified custom; feminists want reformed HMA. This deadlock is mandatrophy — the arrangement's mandate (gender justice via codification) has outlived its consensus but no exit coalition forms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the hindu_codified_reading a genuine coordination mechanism (uniform justiciable rules) or a religious extraction mechanism (state enforcement of patriarchal norms)?',
    'Counterfactual: if HMA were replaced by secular civil code with identical procedural provisions but gender-neutral substantive rules, would coordination costs increase? If yes, the religious framing carries coordination value; if no, it is pure extraction.',
    'If coordination value is irreducible, the tangled_rope classification holds. If extraction is reducible without coordination loss, the constraint is a snare with coordination cover. This determines whether reform should target interpretation (within HMA) or replacement (UCC).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the religious framing of HMA carries irreducible coordination value or is extractive cover.').

omega_variable(
    gender_equity_comparability,
    'Can ''moderate gender equity'' be measured comparably across the five kernel readings when each operates on different normative baselines (religious text, custom, constitutional rights)?',
    'Develop a structured index of substantive rights (divorce access, maintenance, custody, property, remarriage) scored per reading, then weight by enforcement effectiveness. Compare deltas from a counterfactual ''formal equality'' baseline.',
    'If comparability fails, the ''moderate'' claim is ungrounded and cross-reading extraction rankings are unreliable. The engine''s coupling analysis across readings would rest on incommensurable metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gender_equity_comparability, conceptual, 'Commensurability of gender equity measurement across structurally different personal law systems.').

omega_variable(
    state_enforcement_legitimacy,
    'Does state court enforcement of codified Hindu law violate constitutional secularism (basic structure) or fulfill constitutional directive (Article 44 UCC)?',
    'Supreme Court constitutional bench ruling on whether personal law codification is ''law in force'' (Article 372) immune from Part III challenge, or ''law'' subject to fundamental rights review (pending since Narasu 1951, referred 2019).',
    'If enforcement is unconstitutional, the constraint''s suppression component is illegitimate state action — reclassification toward snare. If constitutional, the tangled_rope stands with state as legitimate agenda-setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_legitimacy, empirical, 'Constitutional legitimacy of state enforcing religious personal law.').

omega_variable(
    caste_gender_intersection_extraction,
    'Does HMA extraction operate primarily on gender axis, caste axis, or their intersection — and does the kernel reading framework capture intersectional extraction?',
    'Empirical study of litigation outcomes disaggregated by caste and gender: do lower-caste women face compounded disadvantages in maintenance, custody, property claims under HMA adjudication compared to dominant-caste women and lower-caste men?',
    'If intersectional extraction is significant, the current beneficiary/victim lists (separate gender and caste groups) understate structural extraction. The constraint may be snare for lower-caste women but tangled_rope for dominant-caste women.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(caste_gender_intersection_extraction, empirical, 'Whether extraction is intersectional and the kernel reading framework captures it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.25).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t1976, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1976, 0.3).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t1985, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2005, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2015, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_tr_t2024, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.62).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t1976, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1976, 0.65).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t1985, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2005, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2015, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t1976, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1976, 0.55).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t1985, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2005, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2015, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(marriage_authority_kernel__hindu_codified_reading_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, special_marriage_act_1954).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, hindu_succession_act_1956).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, anti_conversion_laws_state_level).

% DUAL FORMULATION NOTE:
% This constraint is the hindu_codified_reading of the marriage_authority_kernel. The kernel decomposes into five constraint stories (one per reading) because each reading has distinct ε (extractiveness), distinct beneficiary/victim structures, and distinct coordination/extraction balance. The ε-invariance principle requires separate stories: measuring 'marriage authority' via Hindu law yields different structural properties than via Shariat or secular code. The stories are linked via affects_constraints. The HMA reading influences the secular_civil_reading (SMA exists as exit option, creates competitive pressure) and is influenced by muslim_shariat_reading (Shah Bano 1985 triggered HMA amendment debates; Shayara Bano 2017 triple talaq ruling cited HMA reform trajectory).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, institutional, 0.45).
constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, organized, 0.25).
constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, moderate, 0.75).
constraint_indexing:directionality_override(marriage_authority_kernel__hindu_codified_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
