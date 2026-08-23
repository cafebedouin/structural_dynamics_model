% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Gender Category Membership by Self-Identification
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   This constraint story captures the identity_reading of the contested
 *   sex_gender_category kernel: the claim that category membership in
 *   'woman'/'man' is determined solely by subjective gender identity
 *   (self-identification), without medical or biological prerequisites. This
 *   reading has been adopted in varying forms in Argentina (2012), Malta
 *   (2015), Ireland (2015), Denmark (2014), and multiple other jurisdictions,
 *   and drives current policy debates in UK, US, Canada, Australia, and EU.
 *   The constraint presents as a coordination mechanism (simple, dignified
 *   recognition) but generates asymmetric impacts: trans people gain legal
 *   recognition and access, while cis women lose exclusive claim to sex-based
 *   protections and gender-critical feminists lose their analytical
 *   framework. Conflict over space access (prisons, shelters, sports,
 *   changing rooms) is high despite low administrative enforcement costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.45).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.35).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Gender Category Membership by Self-Identification").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, 'b41b986a-49d9-4abd-82fe-6dee1e2321a7').
narrative_ontology:cs_kernel_codification('b41b986a-49d9-4abd-82fe-6dee1e2321a7', distributed).
narrative_ontology:cs_authority_grounding('b41b986a-49d9-4abd-82fe-6dee1e2321a7', distributed).
narrative_ontology:cs_reading_relation('b41b986a-49d9-4abd-82fe-6dee1e2321a7', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('b41b986a-49d9-4abd-82fe-6dee1e2321a7', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('b41b986a-49d9-4abd-82fe-6dee1e2321a7', foundational, self_identification_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(self_identification_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('b41b986a-49d9-4abd-82fe-6dee1e2321a7', self_identification_sufficient_for_category_membership, deontological).
narrative_ontology:cs_axiom('b41b986a-49d9-4abd-82fe-6dee1e2321a7', foundational, trans_women_are_women).
narrative_ontology:cs_axiom_status(trans_women_are_women, holdable).
narrative_ontology:cs_axiom_grounding('b41b986a-49d9-4abd-82fe-6dee1e2321a7', trans_women_are_women, deontological).
narrative_ontology:cs_axiom('b41b986a-49d9-4abd-82fe-6dee1e2321a7', secondary, gender_self_determination_is_a_human_right).
narrative_ontology:cs_axiom_status(gender_self_determination_is_a_human_right, holdable).
narrative_ontology:cs_axiom_grounding('b41b986a-49d9-4abd-82fe-6dee1e2321a7', gender_self_determination_is_a_human_right, deontological).
narrative_ontology:cs_reference_frame('b41b986a-49d9-4abd-82fe-6dee1e2321a7', self_determination_framework).
narrative_ontology:cs_drift_state('b41b986a-49d9-4abd-82fe-6dee1e2321a7', contemporary_gender_recognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b41b986a-49d9-4abd-82fe-6dee1e2321a7', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, nonbinary_people).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, nonbinary_people).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_self_determination_right).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, inclusive_category_membership).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, depathologization_of_trans_identities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal recognition as women through self-declaration without medical gatekeeping. Access women's spaces, services, and legal protections. Also become visible targets for misogyny and transphobia. Their gender identity is core to self-concept; exit from the category is not a live option.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Gain legal recognition as men through self-declaration. Access men's spaces and legal status. Face misogyny (as former women) and transphobia. Some lose access to women's spaces they previously used (shelters, healthcare). Gender identity is core to self-concept.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, national).

% Gain recognition for non-binary gender markers where available (X markers, self-ID). The binary woman/man framework of self-ID laws may not fully accommodate non-binary identities; some laws only allow switching between M/F. Benefit from depathologization but may remain partially unrecognized.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, nonbinary_people, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, nonbinary_people, payer).

% Lose exclusive legal claim to sex-based protections (sports categories, intimate spaces, shortlists, data collection). Some experience this as extraction of hard-won rights; others experience expanded solidarity. Cannot exit sex class (reproductive biology constrains exit). Feminist organizations split on whether inclusion strengthens or undermines sex-based analysis.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, cis_women, beneficiary).

% Lose the legal and conceptual framework that grounds sex-based oppression analysis in immutable biology. Their advocacy for sex-based rights is recast as exclusionary. Face professional and social sanctions for dissent. Cannot exit the sex class that structures their political analysis.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, payer,
    organized, biographical, constrained, national).

% Implement self-ID laws: update birth certificates, passports, driver's licenses on declaration. Manage downstream conflicts — prison placement, shelter access, sports eligibility, data collection. Bear administrative costs of transition but gain simplified procedure over medical-gatekeeping model.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legal_administrative_systems, agenda_setter,
    institutional, generational, analytical, national).

% Lose gatekeeping authority over legal gender recognition (psychiatric diagnosis, hormone requirements, surgery requirements). Professional identity built on clinical expertise in gender transition is displaced by administrative self-declaration. Some clinicians support depathologization; others see loss of clinical oversight as harmful.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, medical_gatekeepers, excluded,
    powerful, biographical, trapped, national).

% Track impacts on sex-discrimination law, gender pay gap reporting, violence against women statistics, service provision. Must decide whether to monitor by sex, gender identity, or both. Their methodological choices shape what harms become visible.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, equality_monitoring_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administratively simple rule for legal gender recognition that respects self-determination and includes trans people in their affirmed categories without medical gatekeeping barriers.
% TRANSFER_FUNCTION: Moves the authority to define category boundaries from medical/legal gatekeepers (psychiatric diagnosis, hormonal/surgical requirements) to individual self-declaration; moves access to sex-based protections, spaces, and data categories from cis-women-only to inclusive of trans women (and trans men to men's categories).
% ABSENT_VOICES: Detransitioners who report harm from rapid affirmation; people with disorders of sexual development (intersex) whose biological categories are collapsed into gender identity frameworks; women in global south jurisdictions where sex-segregated spaces are critical for safety and religious practice; religious communities with sex-segregated worship and ritual; parents of gender-questioning minors excluded from medical decision-making under affirmation-only models.
% DISAPPEARANCE_RATIONALE: If self-ID recognition vanished overnight, legal systems would revert to biology-based or hybrid (medical-gatekeeping) models. Trans people's identity documents would mismatch their presentation, creating daily friction. Sports, prisons, shelters, and single-sex services would revert to sex-based rules. Data collection on sex discrimination would regain coherence but trans people would lose legal recognition.
% FOUNDING_PROBLEM: The medical gatekeeping model (hybrid_reading) pathologized trans identities, required invasive and sterilizing procedures for legal recognition, created years-long waiting lists, and denied trans people autonomy over their legal classification — treating gender variance as a psychiatric condition requiring professional oversight rather than a human variation deserving self-determination.
% FOUNDING_PROBLEM_CORROBORATION: WHO ICD-11 (2019) depathologized trans identities, removing them from mental disorders chapter. UN Independent Expert on SOGI, Human Rights Committee, and Council of Europe have affirmed self-determination model. Major medical associations (WPATH, AMA, APA, BMA) support depathologization and informed-consent care. Gender-critical feminists and some clinicians contest whether the problem was correctly diagnosed, arguing the medical model protected against regret and ensured differential diagnosis.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).
:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects asymmetric transfer: cis women and gender-critical feminists bear costs (lost exclusivity of sex-based categories) while trans people gain recognition. Not pure extraction because the coordination function (dignified, barrier-free recognition) is genuine and valued by beneficiaries. Suppression (0.35) is moderate: the constraint suppresses biology-based classification in law and policy, but enforcement is administrative (changing a marker on a form) rather than coercive — though downstream space-access conflicts generate social suppression. Theater ratio (0.22) is low: the recognition function is real, not performative, though performative compliance (compelled pronoun use, diversity statements) exists at margins. Accessibility collapse (0.58) is moderate: once self-ID is the legal rule, biology-based alternatives collapse for administrative purposes, but social reality maintains parallel categorizations. Resistance (0.68) is high: organized opposition from gender-critical feminists, some religious groups, some medical professionals, and segments of the public.
 *
 * PERSPECTIVAL GAP:
 *   From trans people's seat: the constraint is a rope — genuine coordination solving the problem of medical gatekeeping, with minimal coercion. From gender-critical feminists' seat: the constraint is a snare — the inclusion story covers extraction of sex-based rights, enforced by institutional capture. From cis women's seat (split): some experience rope (solidarity expansion), others experience tangled_rope (genuine inclusion + real costs). The engine computes this divergence from the structural data: identity_locked exit for trans people vs constrained exit for cis women, different power positions, asymmetric beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women/men/nonbinary people are structural beneficiaries (d near 0.0): constraint subsidizes their recognition, identity_locked exit means they cannot leave the category without self-negation. Cis women are structural payers (d near 0.8): bear costs of lost sex-based exclusivity, constrained exit (cannot exit sex class). Gender-critical feminists are payers (d near 0.9): lose analytical framework, face sanctions for dissent, constrained exit. Legal/administrative systems are agenda_setters (d ~0.3): they administer and gain simplified procedure but bear downstream conflict costs. Medical gatekeepers are excluded (d ~0.7): lose authority, professional identity trapped. Equality bodies are observers (d=0.5): analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (medical gatekeeping pathologization) remains live in many jurisdictions (UK, US states, parts of EU still require medical diagnosis). Where self-ID is adopted, the mandate has partially resolved the founding problem but generated new contested problems (space access, data integrity, safeguarding). The constraint shows mandatrophy risk if self-ID laws persist unchanged while the space-access conflicts intensify — the administrative simplicity that justified the constraint becomes a liability when downstream conflicts require nuanced solutions the constraint forbids.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_kind_vs_social_construction,
    'Is the category ''woman'' a natural kind grounded in reproductive biology, or a social construction open to self-determination?',
    'Interdisciplinary consensus across philosophy of biology, feminist theory, and trans studies — unlikely to be resolved empirically; remains a conceptual framing contest.',
    'If natural kind, biology_reading is the Mountain and identity_reading is a constructed Snare. If social construction, identity_reading is a Rope/Tangled Rope and biology_reading is a Piton (degraded natural-law claim). Classification of all three readings hinges on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_kind_vs_social_construction, conceptual, 'Whether sex/gender categories are natural kinds or social constructions — the root framing ambiguity for the entire kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of biology-based classification structural (legal/administrative replacement) or internalized (ideological capture making dissent unspeakable)?',
    'Post-reform tracking: if dissent persists and alternative categorizations remain socially available, suppression is primarily structural. If dissent is silenced and self-censorship spreads, internalized component is significant.',
    'If internalized suppression is high, the constraint''s effective suppression exceeds the structural measure — targets carry the suppression with them. Would increase effective extraction for gender-critical feminists and cis women who cannot articulate dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the displacement of biology-based frameworks.').

omega_variable(
    extraction_measurement_under_contested_victimhood,
    'How to measure extraction when both trans people (denied recognition) and cis women (losing sex-based protections) claim victimhood?',
    'Comparative rights-impact assessment: quantify loss of legal protections for cis women vs gain for trans people; track space-access conflict outcomes; measure deterrence of dissent.',
    'If extraction is symmetrical (both sides lose/gain comparably), tangled_rope is confirmed. If extraction flows overwhelmingly one way, classification shifts toward rope or snare. Current metrics assume moderate asymmetry favoring trans beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_measurement_under_contested_victimhood, conceptual, 'Methodological ambiguity in measuring extraction when victimhood is claimed by opposing groups.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (identity_reading) of the sex_gender_category kernel. How do the sibling readings (biology_reading, hybrid_reading) structurally differ in their beneficiary/victim sets and coordination functions?',
    'Author separate constraint stories for biology_reading and hybrid_reading with their own ε, stakeholders, and classifications. Link via network.affects_constraints. Compare computed seat types across readings.',
    'If biology_reading computes as Mountain (natural law) for all seats, identity_reading''s claim to be coordination is challenged. If hybrid_reading computes as Tangled Rope with different victim set, the kernel''s contest is mapped structurally rather than rhetorically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural delta: this reading''s beneficiary/victim/enforcement profile vs sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_id_tr_t2010, sex_gender_category__identity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(sgc_id_tr_t2014, sex_gender_category__identity_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(sgc_id_tr_t2018, sex_gender_category__identity_reading, theater_ratio, 2018, 0.16).
narrative_ontology:measurement(sgc_id_tr_t2021, sex_gender_category__identity_reading, theater_ratio, 2021, 0.19).
narrative_ontology:measurement(sgc_id_tr_t2025, sex_gender_category__identity_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(sgc_id_be_t2010, sex_gender_category__identity_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(sgc_id_be_t2014, sex_gender_category__identity_reading, base_extractiveness, 2014, 0.3).
narrative_ontology:measurement(sgc_id_be_t2018, sex_gender_category__identity_reading, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement(sgc_id_be_t2021, sex_gender_category__identity_reading, base_extractiveness, 2021, 0.42).
narrative_ontology:measurement(sgc_id_be_t2025, sex_gender_category__identity_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(sgc_id_su_t2010, sex_gender_category__identity_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(sgc_id_su_t2014, sex_gender_category__identity_reading, suppression_requirement, 2014, 0.2).
narrative_ontology:measurement(sgc_id_su_t2018, sex_gender_category__identity_reading, suppression_requirement, 2018, 0.28).
narrative_ontology:measurement(sgc_id_su_t2021, sex_gender_category__identity_reading, suppression_requirement, 2021, 0.32).
narrative_ontology:measurement(sgc_id_su_t2025, sex_gender_category__identity_reading, suppression_requirement, 2025, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, single_sex_service_provision).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, gender_based_violence_data_collection).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sports_eligibility_rules).

% DUAL FORMULATION NOTE:
% This constraint (identity_reading) and its siblings (biology_reading, hybrid_reading) form a constraint family decomposing the 'sex/gender category' kernel. Each reading has distinct ε, beneficiary/victim sets, and coordination functions. The identity_reading treats self-ID as sufficient (low enforcement, high inclusion); biology_reading treats biology as necessary/sufficient (high naturalness, zero inclusion of trans people); hybrid_reading treats medical transition as necessary (moderate enforcement, conditional inclusion). They are not the same constraint viewed differently — they are structurally distinct arrangements for the same category boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, organized, 0.75).
constraint_indexing:directionality_override(sex_gender_category__identity_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
