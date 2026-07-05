% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Category Membership by Self-Identified Gender (Identity Reading)
 *   domain: social/legal/political
 *
 * SUMMARY:
 *   This story instantiates the identity_reading of the sex_gender_category
 *   kernel: category membership (for legal sex markers, single-sex space
 *   access, and sports eligibility) is determined by an individual's
 *   self-identified gender rather than by reproductive biology or a hybrid
 *   medical-gatekeeping standard. This reading substantially expands the
 *   beneficiary class (trans women and nonbinary individuals gain recognition
 *   and access without medical preconditions) while shifting the cost of
 *   managing the coordination problem (who belongs in a sex-defined category)
 *   onto groups whose protections were built around biology-linked
 *   vulnerability — cis women in shelters and prisons, female athletes, and
 *   institutions that depend on granular sex data. It is generated as a
 *   standalone, ε-invariant constraint; the biology_reading and
 *   hybrid_reading are separate constraints with their own ε values,
 *   beneficiary sets, and classifications, linked here only through the
 *   shared kernel and network edges.
 *
 * KEY AGENTS:
 *   - trans_women: Primary beneficiary (moderate/identity_locked) — gains recognition and access
 *   - cis_women_in_single_sex_spaces: Primary payer (organized/constrained) — loses exclusive biology-based category claim
 *   - female_athletes_in_open_categories: Secondary payer (moderate/constrained) — loses competitive parity basis
 *   - gender_identity_rights_advocacy_organizations: Agenda-setter (organized/mobile) — sets and defends the standard
 *   - detransitioners: Excluded voice (powerless/trapped) — complicates the framework's premise, rarely represented
 *   - sex_based_data_collectors: Institutional payer (institutional/constrained) — loses data granularity
 *   - legislators_and_courts: Analytical observer (institutional/analytical) — adjudicates between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.42).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.38).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Category Membership by Self-Identified Gender (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal/political").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '009ec20d-8ef0-4511-8313-99fd60be502b').
narrative_ontology:cs_kernel_codification('009ec20d-8ef0-4511-8313-99fd60be502b', distributed).
narrative_ontology:cs_authority_grounding('009ec20d-8ef0-4511-8313-99fd60be502b', distributed).
narrative_ontology:cs_reading_relation('009ec20d-8ef0-4511-8313-99fd60be502b', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('009ec20d-8ef0-4511-8313-99fd60be502b', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('009ec20d-8ef0-4511-8313-99fd60be502b', foundational, gender_identity_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('009ec20d-8ef0-4511-8313-99fd60be502b', gender_identity_is_sufficient_for_category_membership, deontological).
narrative_ontology:cs_axiom('009ec20d-8ef0-4511-8313-99fd60be502b', secondary, medical_or_legal_transition_verification_is_not_required_for_recognition).
narrative_ontology:cs_axiom_status(medical_or_legal_transition_verification_is_not_required_for_recognition, holdable).
narrative_ontology:cs_axiom_grounding('009ec20d-8ef0-4511-8313-99fd60be502b', medical_or_legal_transition_verification_is_not_required_for_recognition, instrumental).
narrative_ontology:cs_reference_frame('009ec20d-8ef0-4511-8313-99fd60be502b', medical_gatekeeping_prior_standard).
narrative_ontology:cs_drift_state('009ec20d-8ef0-4511-8313-99fd60be502b', contemporary_legislative_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('009ec20d-8ef0-4511-8313-99fd60be502b', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, nonbinary_individuals).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_rights_advocacy_organizations).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_in_single_sex_spaces).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, female_athletes_in_open_categories).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, sex_based_data_collectors).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, detransitioners).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_identity_is_the_relevant_category_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition as women without requiring medical transition or surgery as a precondition, and gain access to single-sex spaces, sports categories, and protections previously conditioned on biological sex. Their exit from this framework is not meaningful — the framework's recognition of their identity is the thing they need; abandoning the self-ID standard would mean abandoning recognition itself.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Lose the ability to define single-sex spaces (shelters, prisons, changing rooms, sports categories) by reproductive biology alone; must now share spaces designed around vulnerability to male-pattern violence or physical advantage with anyone who self-identifies as a woman regardless of transition status. Their exit options are limited to private alternatives (where affordable) or organizing politically to contest the category rule.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_in_single_sex_spaces, payer,
    organized, biographical, constrained, national).

% Compete against athletes who retained pubertal androgenization advantages, in categories that were carved out specifically to compensate for average sex-based performance gaps. Their recourse is largely limited to individual sport federations, many of which have moved independently toward biology-based eligibility rules in tension with the broader self-ID legal standard.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, female_athletes_in_open_categories, payer,
    moderate, biographical, constrained, national).

% Lobby for, draft, and litigate to establish self-identification as the legal standard for category membership; derive institutional funding, political influence, and organizational purpose from advancing and defending this standard. Face low direct cost from the rule's operation and substantial exit options (staff and leadership are not personally bound by the categories they advocate for).
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_rights_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, gender_identity_rights_advocacy_organizations, beneficiary).

% Individuals who identified under this standard, transitioned socially or medically, and later reverted; their experience — that self-identification did not track a stable underlying reality for them — complicates the framework's premise but is rarely represented in the advocacy organizations that set the standard's terms, and their accounts are frequently treated as embarrassing outliers rather than data.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, detransitioners, excluded,
    powerless, biographical, trapped, national).

% Public health agencies, criminal justice records systems, and medical researchers who rely on birth-sex data for epidemiology, forensic identification, and clinical risk stratification; self-ID category rules increasingly override or commingle with birth-sex fields in official records, degrading the granularity of the data these institutions depend on.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, sex_based_data_collectors, payer,
    institutional, generational, constrained, national).

% Adjudicate disputes between the competing readings of the category kernel, hear testimony from all affected groups, and produce the statutory or case-law text that either entrenches or narrows the self-ID standard.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, legislators_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, diffuse).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, low-friction administrative rule for determining category membership in law, medicine, and public accommodation — avoiding case-by-case medical or bureaucratic gatekeeping and reducing the psychological and administrative burden on transitioning individuals.
% TRANSFER_FUNCTION: Moves access to single-sex spaces, sports categories, legal sex markers, and associated protections from a biology-based allocation toward a self-declaration-based allocation; the beneficiaries gain access and recognition, while groups whose protections were designed around reproductive-biology-linked vulnerability (physical safety, athletic parity, epidemiological tracking) bear the resulting exposure.
% ABSENT_VOICES: Detransitioners and gender-critical feminists who accept trans people's right to live free of discrimination but contest the self-ID category standard specifically are frequently excluded from the advocacy and legislative drafting process, and are often characterized as illegitimate participants in the debate rather than as a stakeholder class with a competing claim.
% DISAPPEARANCE_RATIONALE: If the self-ID standard were removed and category membership reverted to biology-based or hybrid gatekeeping, legal sex markers, prison and shelter placements, and sports eligibility rules would all require re-litigation; trans women without medical transition would lose legal recognition as women in jurisdictions currently using this standard, and organizations built around defending it would need a new institutional purpose.
% FOUNDING_PROBLEM: Medical and legal gatekeeping models (requiring surgery, hormone therapy duration, or psychiatric sign-off before legal recognition) imposed severe, sometimes irreversible, medical requirements on individuals seeking recognition of their gender identity, and were criticized as invasive, slow, and disconnected from lived need for accurate documentation and physical safety.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethicists and some detransitioner advocacy groups (outside the gender-identity-rights organizations that benefit from the standard) attest that the original gatekeeping problem was real but argue the self-ID solution overcorrected past the point needed to solve it, creating a new set of costs for a different population; family court judges and prison administrators in multiple jurisdictions have independently flagged unresolved tension between the legal standard and physical-safety-based space allocation.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end) rather than high: the identity_reading genuinely solves a real coordination problem (reducing invasive gatekeeping burden on transitioning individuals) but does so by reallocating a scarce good — access to sex-defined space and categories — without compensating the groups who lose exclusive claim to it. Suppression is moderate (0.38): enforcement operates mainly through anti-discrimination law, institutional policy, and social sanction rather than criminal coercion, but has intensified as the standard has been codified into more jurisdictions and institutions. Theater ratio is low (0.22): the standard performs a real administrative function (reduced gatekeeping friction) rather than being predominantly performative, though some institutional adoption is symbolic/compliance-driven rather than operationally load-bearing. Resistance is high (0.72) reflecting substantial organized pushback from cis women's advocacy groups, sports federations, and some feminist organizations. Accessibility collapse is moderate (0.35): biology-based and hybrid alternatives remain live in law and policy in many jurisdictions — this reading has not achieved anything like the alternative-collapse of a mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (trans women, advocacy organizations), the arrangement reads as rope: a coordination fix for an unjust and medically invasive prior gatekeeping regime. From the payer seat (cis women in single-sex spaces, female athletes), the same arrangement reads as tangled rope shading toward snare: a genuine recognition problem was solved by transferring the cost of managing category boundaries onto a group that did not consent to bear it and has organized resistance without full recourse. The engine computes these divergent seat classifications from the same structural data — the story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women and nonbinary individuals are declared beneficiaries: the standard removes a legal and medical barrier to recognition, so directionality sits near the beneficiary end despite their exit_options being identity_locked (their stake in the standard is not something they can costlessly walk away from, but the constraint's function subsidizes rather than extracts from them). Cis women in single-sex spaces and female athletes are declared victims: the standard reallocates a good (biology-linked category exclusivity) they previously held, and their exit_options are constrained (private alternatives exist but are costly or unavailable). Gender identity rights advocacy organizations are agenda-setters with mobile exit — they experience low personal cost regardless of the rule's downstream effects, which is why they occupy institutional/organized power without bearing the category's costs directly. Sex-based data collectors are institutional payers whose cost is diffuse (degraded data quality) rather than acute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — invasive, medically gatekept, slow recognition processes — was real and severe for the population it targeted. Its status is contested rather than dead: some jurisdictions still operate hybrid or biology-based standards, so the underlying need for a low-friction recognition mechanism persists somewhere. What is contested is whether self-identification specifically (versus a less totalizing procedural reform) was the correct or proportionate fix, and whether the standard has now outrun the specific harm it was built to address by reallocating costs onto an different population (cis women, female athletes) who were not party to the original gatekeeping harm. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: treating the whole arrangement as pure extraction (which erases the real coordination problem it solves) or treating it as pure coordination (which erases the real, uncompensated costs imposed on the payer class).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is subjective self-identification the correct criterion for category membership, or does it improperly displace a biology-based or hybrid criterion that better serves the categories'' original protective function (single-sex safety, athletic parity)?',
    'This is not empirically resolvable within this story — it is the committer-level disagreement the kernel itself is contested on. Legislative and judicial resolution across jurisdictions, tracked over time, would show which reading(s) persist, get overridden, or coexist in different domains (e.g., legal sex markers vs. sports eligibility vs. prison placement) rather than resolving into a single uniform standard.',
    'If the biology_reading or hybrid_reading is adopted instead in a given domain, the beneficiary and victim sets for that domain invert or rebalance: cis women and female athletes move toward beneficiary status, trans women without medical transition move toward payer/excluded status for that domain''s category rule.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'The unresolved committer-level disagreement over which kernel reading should govern category membership.').

omega_variable(
    victim_set_expansion_scope,
    'Does the identity_reading''s expansion of the ''woman'' category to include trans women meaningfully expand the class of people vulnerable to misogyny-driven harm (as the reading''s proponents argue), or does it primarily create a new distinct victim class (cis women in single-sex spaces) without net-reducing harm to the original class?',
    'Comparative incident data on violence and harassment outcomes in single-sex spaces under self-ID vs. biology-based admission policies, collected across jurisdictions with differing standards, tracked longitudinally.',
    'If misogyny-driven harm to trans women is substantially reduced without a corresponding rise in harm or loss of felt safety to cis women, the tangled_rope classification''s victim side weakens toward rope; if harm to cis women''s felt safety and space allocation rises without commensurate offsetting benefit, the classification shades toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_expansion_scope, empirical, 'Whether the reading''s harm-reduction claim for its beneficiary class is empirically borne out at the cost claimed by the payer class.').

omega_variable(
    self_identification_stability_ambiguity,
    'Is self-identified gender a stable enough basis for legal and institutional category assignment, given that a nontrivial subset of people who self-identify under this standard later detransition?',
    'Longitudinal cohort studies tracking self-identification stability rates over time, compared across age groups and transition pathways (social-only vs. medical).',
    'Low stability rates would support the hybrid_reading''s requirement for a transition-verification step as a safeguard; high stability rates would support the identity_reading''s premise that self-report alone is a reliable and sufficient signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_identification_stability_ambiguity, empirical, 'Whether self-identification tracks a stable underlying category membership over an individual''s lifetime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sex__tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(sex__tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__identity_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(sex__be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(sex__be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__identity_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sex__su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(sex__su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__identity_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__identity_reading, 0.08).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the sex_gender_category kernel. biology_reading assigns category membership by reproductive biology at birth (narrower beneficiary set: cis women and female athletes as beneficiaries, trans individuals without surgical/hormonal alignment as victims of exclusion). hybrid_reading assigns membership via a combined biological-and-verified-social-transition standard (medical gatekeeping model, intermediate victim/beneficiary sets, higher boundary-enforcement cost but lower space-access conflict than this reading). Each reading is authored as its own ε-invariant constraint per the ε-invariance principle; do not average across them. All three should be read together to see the full contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
