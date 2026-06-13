% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Female Category Membership via Chromosomal Sex and Reproductive Biology
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint operationalizes membership in the female/woman category
 *   via chromosomal sex (XX), reproductive anatomy (ovaries, uterus, vagina),
 *   and developmental biology (gamete production, hormonal profile). It is
 *   one reading of a contested kernel (woman/female category membership) that
 *   also admits gender-identity and hybrid-contextual readings. Under this
 *   reading, trans women are excluded from female-only spaces and legal
 *   recognition as 'female' or 'woman' because they do not meet the
 *   biological criterion, even if their gender identity aligns with the
 *   category. The constraint is claimed as a necessary protection for natal
 *   females—the founding problem being male sexual violence and reproductive
 *   coercion. However, the measured metrics reflect substantial extraction:
 *   exclusion of trans women and intersex individuals from spaces and
 *   recognition matching their identity or sense of self, enforced by active
 *   institutional machinery. The claim/metric divergence is deliberate and
 *   diagnostic: the constraint is CLAIMED as coordination (protecting real
 *   vulnerabilities) while the authored metrics describe asymmetric
 *   extraction (denial of recognition and access to a targeted group).
 *
 * KEY AGENTS:
 *   - natal_females: biological females (XX, reproductive anatomy intact) who benefit from sex-based legal protections and exclusive female-only spaces
 *   - trans_women: individuals with gender identity aligned to the female/woman category but whose chromosomal sex or reproductive anatomy does not match the biological criterion, excluded by this reading
 *   - intersex_individuals: those with atypical chromosomal or anatomical profile (46,XY with AIS, Klinefelter, etc.) who occupy ambiguous classification status, trapped in institutional limbo
 *   - advocates_for_sex_based_rights: organized actors who articulate and enforce the biological criterion, claiming it is necessary to protect material reality of sex-based oppression
 *   - advocates_for_trans_inclusion: organized actors who object to the biological criterion on grounds it denies trans women recognition and access
 *   - medical_professionals: analysts who document that biological sex and gender identity are developmentally distinct, neutral observers of mechanisms
 *   - legal systems and legislatures: institutional agenda-setters who enact the biological criterion as binding law and policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.72).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Female Category Membership via Chromosomal Sex and Reproductive Biology").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '0eef09c4-cc85-4fb2-9caa-c39c562e9283').
narrative_ontology:cs_kernel_codification('0eef09c4-cc85-4fb2-9caa-c39c562e9283', distributed).
narrative_ontology:cs_authority_grounding('0eef09c4-cc85-4fb2-9caa-c39c562e9283', extraction).
narrative_ontology:cs_reading_relation('0eef09c4-cc85-4fb2-9caa-c39c562e9283', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('0eef09c4-cc85-4fb2-9caa-c39c562e9283', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('0eef09c4-cc85-4fb2-9caa-c39c562e9283', foundational, biological_sex_is_determinative_criterion).
narrative_ontology:cs_axiom_status(biological_sex_is_determinative_criterion, holdable).
narrative_ontology:cs_axiom_grounding('0eef09c4-cc85-4fb2-9caa-c39c562e9283', biological_sex_is_determinative_criterion, empirically_contingent).
narrative_ontology:cs_axiom('0eef09c4-cc85-4fb2-9caa-c39c562e9283', foundational, natal_female_status_immutable_and_exhaustive).
narrative_ontology:cs_axiom_status(natal_female_status_immutable_and_exhaustive, holdable).
narrative_ontology:cs_axiom_grounding('0eef09c4-cc85-4fb2-9caa-c39c562e9283', natal_female_status_immutable_and_exhaustive, deontological).
narrative_ontology:cs_reference_frame('0eef09c4-cc85-4fb2-9caa-c39c562e9283', sex_based_protection_requires_stable_biological_criterion).
narrative_ontology:cs_drift_state('0eef09c4-cc85-4fb2-9caa-c39c562e9283', contemporary_identity_politics_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0eef09c4-cc85-4fb2-9caa-c39c562e9283', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_excluded_from_female_only_spaces).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_individuals_with_atypical_chromosomal_or_anatomical_profile).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, advocates_for_trans_inclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from sex-based legal protections in reproductive healthcare, domestic violence shelters, prison classification, and single-sex spaces; their biological category membership is recognized as the legitimate basis for these protections under this reading. Exit would require abandoning claim to natal female status, which is constitutive of identity for most in this group.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females, beneficiary,
    moderate, biographical, identity_locked, universal).

% Excluded from female-only spaces (shelters, prisons, sports, bathrooms, changing facilities) and from the legal category 'woman' or 'female' under this reading, despite gender identity alignment with that category. Denied access to sex-segregated services and institutions that might match their gender identity. Exit from the constraint would require denying their gender identity or relocating to jurisdictions with different categorization rules.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    moderate, biographical, identity_locked, universal).

% Occupy ambiguous positions relative to the XX/XY chromosome-and-anatomy criterion: those with androgen insensitivity syndrome (46,XY phenotypically female), Klinefelter syndrome (47,XXY), or sex-determining region mutations do not fit cleanly into the binary criterion. Classification becomes arbitrary and contested, trapping them in bureaucratic limbo with unstable access to sex-segregated services.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, intersex_individuals, payer,
    powerless, biographical, trapped, universal).

% Articulate and enforce the biological-sex criterion for female category membership; argue it is necessary to protect the material reality of sex-based oppression and to preserve spaces historically built for biological females. Actively oppose expansion of the female category to include trans women or contextual membership. Enforce the criterion through legal advocacy, ballot measures, and institutional policy.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, advocates_for_sex_based_rights, agenda_setter,
    organized, generational, mobile, universal).

% Object to the sex-biology criterion on grounds that it denies trans women recognition of their gender identity and excludes them from spaces matching that identity. Are excluded from the agenda-setting conversation in jurisdictions where the sex-biology reading is institutionalized; their objections are treated as denying or ignoring material sex-based reality rather than as legitimate competing claims.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, advocates_for_trans_inclusion, payer,
    organized, generational, mobile, universal).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, advocates_for_trans_inclusion, excluded).

% Observe that biological sex and gender identity are developmentally distinct (endocrine, neurological, social factors act at different stages); that chromosome, anatomy, hormone profile, and psychological identity can diverge; and that each has diagnostic relevance to specific medical questions. Their role is to provide evidence about biological mechanisms without adjudicating which criterion is legitimate for category membership.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_professionals, observer,
    institutional, biographical, analytical, universal).

% Enact and enforce sex-segregation law and policy, determining which criterion (biology, identity, or hybrid) governs access to shelters, prisons, sports, medical protocols. Their choice of criterion operationalizes the reading and makes it binding on all other stakeholders. Power is structured by jurisdiction.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, legal_systems_and_legislatures, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, advocates_for_sex_based_rights).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production and maintenance of single-sex spaces, sex-segregated institutions, and sex-based legal protections by establishing a boundary-rule for category membership. The coordination problem: groups seeking sex-segregated spaces for legitimate material reasons (physical safety in prisons and shelters, reproductive healthcare privacy, protection from male sexual violence) require a defensible membership criterion that others cannot arbitrarily enter.
% TRANSFER_FUNCTION: Transfers recognition, access, and legal status from trans women and intersex individuals to natal females by exclusively anchoring 'female' and 'woman' to chromosomal sex and reproductive anatomy rather than to gender identity or hybrid criteria. The transfer is exclusionary: trans women are denied access to female-only spaces and legal recognition as female, while natal females consolidate exclusive claim to sex-based protections and spaces built historically for biological females.
% ABSENT_VOICES: Trans women and intersex individuals are substantially excluded from the jurisdictions and institutions where this reading is most forcefully enforced. Their voices are present in meta-level policy debate but are not seated in the concrete spaces (prisons, shelters, legislatures) where access and membership are determined. Advocates for trans inclusion are organizationally present but face structural barriers to influence in legislatures controlled by advocates for sex-based rights.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight, institutional access would reorganize: prisons would reclassify inmates by criteria other than natal sex; shelters would alter admission rules; legal systems would redefine 'woman' either by gender identity or hybrid criteria. Sex-segregated spaces would persist under alternative boundary rules, but the specific exclusion of trans women would collapse and institutional access would shift. This is not a natural fact that would reassert itself; it is a choice point where the constraint's removal would trigger institutional reorganization.
% FOUNDING_PROBLEM: Biological sex is materially real and has been the historic basis for patterns of male sexual violence, reproductive coercion, and sex-based labor exploitation. Protecting spaces and rights built to mitigate these patterns requires a stable, non-arbitrary criterion for membership. The founding problem is ensuring that sex-segregated spaces intended for biological females cannot be dissolved or infiltrated by those without the shared material reality of reproductive and sexual vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for sex-based rights and many natal females testify that sex-based violence and reproductive vulnerability are enduring material facts that justify exclusive female spaces. Medical literature confirms chromosomal sex, reproductive anatomy, and hormonal profiles are distinct from gender identity and have diagnostic relevance to sexual assault risk and medical outcomes. However, advocates for trans inclusion and gender-critical scholars corroborate that gender identity is also a material social fact with psychological reality and that exclusion of trans women from spaces matching their identity produces documented harms (increased vulnerability, denial of dignity, reduced access to services). No party outside the sex-biology-reading beneficiary set corroborates that the founding problem justifies universal exclusion of trans women from all female-only spaces; the corroboration is conditional on accepting the reading's specific boundary rule.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint denies recognition and institutional access to trans women and intersex individuals, not as a side effect but as the defining operation of the boundary rule. The extraction is the exclusion itself—the constraint's entire function is to ensure that only natal females can access female-only spaces and legal status. Suppression is similarly high (0.72) because exclusion of trans women from policy-making forums and from institutional spaces is structural: they are legally barred or practically prevented from participating in the spaces where membership is determined. Theater ratio (0.41) reflects that the constraint's stated justification—protection of natal females from sex-based violence—is a genuine founding problem, but the measured trajectory shows rising suppression and stakes without corresponding deepening of the safety benefit; institutional resources shift toward enforcement of exclusion rather than toward strengthening actual protections for natal females. The coercion grid shows asymmetric pressure across levels: at the individual level, trans women and intersex people face collapsed alternatives (accessibility_collapse 0.82 at t40) and high stakes (stakes_inflation 0.75); at the organizational level, advocacy organizations holding trans-inclusive readings face suppression (0.74); at the structural level, the constraint is relatively stable and encounters only moderate resistance (0.79). The measurements are authored on a single shared time grid (every metric at t0, 8, 16, 24, 32, 40) so that temporal analysis has aligned data across dimensions.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (natal females) and the payer seats (trans women, intersex individuals) compute radically different constraint types from the same structural data. From the beneficiary seat, the constraint is genuinely coordinating—providing a stable, non-arbitrary membership criterion for sex-based protections that sex-based violence makes necessary. From the payer seats, the same structure is pure extraction: exclusion from recognition and access enforced by institutional power, with no compensation and no exit. The engine computes these divergences per-seat from power, exit options, and the beneficiary/victim declarations. The sex-biology-reading's internal framing (the constraint as coordination) and the payer-seat phenomenology (the constraint as extraction) are both real descriptions of the same constraint, but from different structural positions. This is exactly the analytic task the per-seat computation performs.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females are the structural beneficiary (d near 0.0): they benefit from exclusive access to spaces and legal recognition, bear minimal direct cost (the constraint requires enforcement but natal females do not pay enforcement costs), and have arbitrage-grade exit (they can always access female-only spaces because they meet the biological criterion). Trans women and intersex individuals are the structural targets (d near 1.0): they are denied access to spaces and legal recognition, have no exit except to deny their gender identity or relocate, and are trapped by the identity-locked exit option. Advocates for sex-based rights are the institutional beneficiaries and agenda-setters (moderate power, mobile exit via political and legal careers, collected authority through legislative power). Advocates for trans inclusion are payers in the sense that their policy agenda is systematically excluded and their institutional voice is suppressed (moderate power, mobile exit but at cost of abandoning the policy goal).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows potential mandatrophy signals: the founding problem (sex-based violence, reproductive vulnerability) is real and partially persistent, but the measured trajectory suggests rising suppression and theater without proportional increase in the actual protection of natal females from violence. The constraint's operation appears to be shifting from coordination (establishing safe spaces) toward extraction (enforcing exclusion and institutional affirmation of the sex-biology reading). The high theater ratio (0.41 at t40) and rising suppression (0.58→0.72 over the interval) suggest that institutional resources are increasingly devoted to defending the boundary rule itself rather than to protecting natal females. This would be consistent with mandatrophy: the function (protecting sex-based safety) has become decoupled from the constraint (enforcing the sex-biology criterion), and the constraint persists through institutional inertia and power rather than because it solves the founding problem more effectively than alternatives would.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_foreclusion,
    'Does biological sex as a definitional criterion for female category membership logically foreclose or merely compete with gender-identity and hybrid-contextual readings?',
    'Philosophical analysis of the internal coherence of each reading when restricted to a single institutional framework (one prison system, one shelter organization, one legal system). Can a coherent institutional framework hold both the sex-biology criterion and the gender-identity criterion simultaneously, or does adopting one necessitate rejecting the other as a matter of logical consistency?',
    'If the readings are logically foreclosing (mutually exclusive), the sex-biology reading claims authority to disallow alternative readings in a unified framework. If they coexist (different parties can hold different readings without logical contradiction), the contest is about legitimacy and power, not logical necessity. This classification difference affects cs_structure.reading_relations assignment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_foreclusion, conceptual, 'Whether the sex-biology criterion logically forecloses or coexists with sibling readings.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression of trans women''s objections structural (external barriers to participation in policy-making, legal exclusion from standing) or internalized (trans women have come to accept the reading as legitimate through repeated institutional enforcement)?',
    'Longitudinal interview study tracking whether trans women''s resistance to the criterion decreases after long-term institutional enforcement (suggesting internalization) or persists unchanged (suggesting structural suppression only). Survey of trans women regarding perception of suppression mechanism.',
    'If internalized, the measured suppression metric understates the constraint''s effective suppressive force, because suppression persists in the target''s cognition even after exit from direct institutional contact. If structural only, the suppression is tied to specific institutions and would weaken if enforcement capacity decayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of objections to the sex-biology reading is structural or internalized.').

omega_variable(
    biological_sex_materiality_boundaries,
    'Where exactly does the biological criterion apply? Does it apply universally (all institutional contexts) or only to contexts where the founding problem (sexual violence risk, reproductive vulnerability) is present?',
    'Policy analysis across contexts: sexual-violence shelters (high founding-problem relevance), employment law (low relevance), sports policy (mixed relevance—athletic advantage), healthcare (mixed relevance—reproductive vs. general medicine). Do jurisdictions apply the sex-biology criterion uniformly or do they vary by context in ways that contradict the universality claim?',
    'If the criterion is context-sensitive in actual deployment despite being stated as universal, the reading''s claim to stability is undermined. If the criterion is applied universally even in contexts where the founding problem does not apply (e.g., mandatory single-sex pronouns in non-safety contexts), the extraction component may be larger than the coordination component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_sex_materiality_boundaries, empirical, 'Whether the sex-biology criterion applies universally or only in contexts where sex-based safety or reproductive vulnerability is materially relevant.').

omega_variable(
    intersex_individual_boundary_arbitrariness,
    'How are intersex individuals classified under the sex-biology criterion when their chromosomal, anatomical, or hormonal profile is atypical? Is there a principled rule, or does classification become arbitrary and dependent on which medical authority or legal jurisdiction decides?',
    'Systematic audit of how legal systems and institutions classify intersex individuals (Androgen Insensitivity Syndrome, 46,XY complete gonadal dysgenesis, chimeric/mosaic individuals, etc.). Document variation across jurisdictions and time. Interview intersex individuals about stability and fairness of their own classification.',
    'If classification is principled and stable, the sex-biology criterion can handle the edge cases and the constraint''s internal coherence is preserved. If classification is arbitrary, the criterion fails to deliver the stability it promises and the measured extractiveness may include the cost of bureaucratic limbo for intersex individuals. High arbitrariness would suggest the underlying criterion is less biologically determinate than claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_individual_boundary_arbitrariness, empirical, 'Whether the sex-biology criterion produces consistent classification for intersex individuals or devolves into arbitrary institutional decisions.').

omega_variable(
    alternative_reading_kernel_contest,
    'This constraint is one reading of the contested woman/female category kernel. The alternative readings (gender_identity_reading, hybrid_contextual_reading) reflect genuinely different philosophical commitments or different empirical claims about sex and gender?',
    'Textual analysis of the foundational arguments in each reading. Do they disagree about empirical facts (is gender identity innate? do trans women pose statistically higher assault risk?) or about normative questions (is gender identity or biological sex the more legitimate basis for rights and recognition)? Distinguish empirical from philosophical disagreement.',
    'If the disagreement is empirical, resolution may come from evidence (longitudinal studies, neuroimaging, institutional data). If it is normative/philosophical, the readings may be permanently contested and resolution may come only through political power rather than evidence. This affects how the three-reading family should be modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_kernel_contest, conceptual, 'Whether the kernel contest reflects empirical disagreement or irreducibly normative disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__sex_biology_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(woma_tr_t8, observed).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__sex_biology_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(woma_tr_t16, observed).
narrative_ontology:measurement(woma_tr_t24, woman_female_category__sex_biology_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(woma_tr_t24, observed).
narrative_ontology:measurement(woma_tr_t32, woman_female_category__sex_biology_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(woma_tr_t32, observed).
narrative_ontology:measurement(woma_tr_t40, woman_female_category__sex_biology_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(woma_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t8, woman_female_category__sex_biology_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(woma_be_t8, observed).
narrative_ontology:measurement(woma_be_t16, woman_female_category__sex_biology_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(woma_be_t16, observed).
narrative_ontology:measurement(woma_be_t24, woman_female_category__sex_biology_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(woma_be_t24, observed).
narrative_ontology:measurement(woma_be_t32, woman_female_category__sex_biology_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(woma_be_t32, observed).
narrative_ontology:measurement(woma_be_t40, woman_female_category__sex_biology_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(woma_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t8, woman_female_category__sex_biology_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(woma_su_t8, observed).
narrative_ontology:measurement(woma_su_t16, woman_female_category__sex_biology_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(woma_su_t16, observed).
narrative_ontology:measurement(woma_su_t24, woman_female_category__sex_biology_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(woma_su_t24, observed).
narrative_ontology:measurement(woma_su_t32, woman_female_category__sex_biology_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(woma_su_t32, observed).
narrative_ontology:measurement(woma_su_t40, woman_female_category__sex_biology_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(woma_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(woma_grid_01, woman_female_category__sex_biology_reading, accessibility_collapse(class), 0, 0.71).
narrative_ontology:measurement(woma_grid_02, woman_female_category__sex_biology_reading, accessibility_collapse(class), 40, 0.79).
narrative_ontology:measurement(woma_grid_03, woman_female_category__sex_biology_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(woma_grid_04, woman_female_category__sex_biology_reading, accessibility_collapse(individual), 40, 0.82).
narrative_ontology:measurement(woma_grid_05, woman_female_category__sex_biology_reading, accessibility_collapse(organizational), 0, 0.75).
narrative_ontology:measurement(woma_grid_06, woman_female_category__sex_biology_reading, accessibility_collapse(organizational), 40, 0.84).
narrative_ontology:measurement(woma_grid_07, woman_female_category__sex_biology_reading, accessibility_collapse(structural), 0, 0.8).
narrative_ontology:measurement(woma_grid_08, woman_female_category__sex_biology_reading, accessibility_collapse(structural), 40, 0.81).
narrative_ontology:measurement(woma_grid_09, woman_female_category__sex_biology_reading, resistance(class), 0, 0.64).
narrative_ontology:measurement(woma_grid_10, woman_female_category__sex_biology_reading, resistance(class), 40, 0.61).
narrative_ontology:measurement(woma_grid_11, woman_female_category__sex_biology_reading, resistance(individual), 0, 0.71).
narrative_ontology:measurement(woma_grid_12, woman_female_category__sex_biology_reading, resistance(individual), 40, 0.68).
narrative_ontology:measurement(woma_grid_13, woman_female_category__sex_biology_reading, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(woma_grid_14, woman_female_category__sex_biology_reading, resistance(organizational), 40, 0.76).
narrative_ontology:measurement(woma_grid_15, woman_female_category__sex_biology_reading, resistance(structural), 0, 0.82).
narrative_ontology:measurement(woma_grid_16, woman_female_category__sex_biology_reading, resistance(structural), 40, 0.79).
narrative_ontology:measurement(woma_grid_17, woman_female_category__sex_biology_reading, stakes_inflation(class), 0, 0.65).
narrative_ontology:measurement(woma_grid_18, woman_female_category__sex_biology_reading, stakes_inflation(class), 40, 0.72).
narrative_ontology:measurement(woma_grid_19, woman_female_category__sex_biology_reading, stakes_inflation(individual), 0, 0.62).
narrative_ontology:measurement(woma_grid_20, woman_female_category__sex_biology_reading, stakes_inflation(individual), 40, 0.75).
narrative_ontology:measurement(woma_grid_21, woman_female_category__sex_biology_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(woma_grid_22, woman_female_category__sex_biology_reading, stakes_inflation(organizational), 40, 0.71).
narrative_ontology:measurement(woma_grid_23, woman_female_category__sex_biology_reading, stakes_inflation(structural), 0, 0.54).
narrative_ontology:measurement(woma_grid_24, woman_female_category__sex_biology_reading, stakes_inflation(structural), 40, 0.63).
narrative_ontology:measurement(woma_grid_25, woman_female_category__sex_biology_reading, suppression(class), 0, 0.71).
narrative_ontology:measurement(woma_grid_26, woman_female_category__sex_biology_reading, suppression(class), 40, 0.79).
narrative_ontology:measurement(woma_grid_27, woman_female_category__sex_biology_reading, suppression(individual), 0, 0.68).
narrative_ontology:measurement(woma_grid_28, woman_female_category__sex_biology_reading, suppression(individual), 40, 0.76).
narrative_ontology:measurement(woma_grid_29, woman_female_category__sex_biology_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(woma_grid_30, woman_female_category__sex_biology_reading, suppression(organizational), 40, 0.74).
narrative_ontology:measurement(woma_grid_31, woman_female_category__sex_biology_reading, suppression(structural), 0, 0.51).
narrative_ontology:measurement(woma_grid_32, woman_female_category__sex_biology_reading, suppression(structural), 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The woman/female category kernel is instantiated by three structurally distinct readings with different ε values, beneficiary sets, and victim sets. Each reading is a separate constraint story. The sex_biology_reading (this story) defines membership by chromosomal and anatomical criteria, producing high extraction for trans women and intersex individuals. The gender_identity_reading (sibling story) defines membership by self-identification, producing high extraction for natal females who feel their material sex-based category is erased. The hybrid_contextual_reading (sibling story) distinguishes contexts, producing lower extraction on both sides but high contestation over context boundaries. All three readings represent live policy positions held by different factions; none has foreclosed the others in contemporary institutional practice, though individual jurisdictions often enforce one reading exclusively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_female_category__sex_biology_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
