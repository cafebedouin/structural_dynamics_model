% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Sex-Based Category Membership: Chromosomal/Reproductive Biology Reading
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   woman_female_category kernel: the sex-biology reading, under which
 *   category membership in 'woman/female' is determined by chromosomal sex,
 *   reproductive anatomy, and developmental history (including the presence
 *   or absence of male puberty), rather than by gender identity or by
 *   context-dependent criteria. This reading is applied in specific
 *   institutional domains — competitive sport eligibility, prison housing
 *   placement, and domestic violence shelter admission — where its proponents
 *   argue biological criteria track safety-relevant and fairness-relevant
 *   physiological facts. Under this reading, trans women who do not meet the
 *   chromosomal/developmental criterion are excluded from female-designated
 *   spaces and categories regardless of legal gender recognition elsewhere.
 *   This is a distinct constraint from the gender_identity_reading (which
 *   would produce the opposite victim/beneficiary structure) and the
 *   hybrid_contextual_reading (which would split the domains differently).
 *   Per the ε-invariance principle, these are not the same constraint viewed
 *   three ways — each reading has its own beneficiary/victim structure and
 *   its own ε, and they are linked here only through
 *   network.affects_constraints, not merged.
 *
 * KEY AGENTS:
 *   - natal_females_seeking_sex_based_protections: primary beneficiary (organized/constrained) — gains guaranteed access to sex-segregated safety and competitive spaces
 *   - trans_women_seeking_female_category_access: primary target (powerless/trapped) — bears exclusion from spaces and categories regardless of legal or social gender recognition
 *   - womens_sports_organizations, domestic_violence_shelter_operators, prison_administrators: institutional agenda-setters who administer and enforce the biological criterion in their respective domains
 *   - trans_rights_advocacy_organizations: excluded voice — contests the criterion's safety rationale but does not control the institutions applying it
 *   - courts_and_legislatures: analytical observer — adjudicates between competing readings without resolving the underlying kernel dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.42).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.55).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Sex-Based Category Membership: Chromosomal/Reproductive Biology Reading").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, '4c0eaafb-4481-4982-824e-120d80f35cdf').
narrative_ontology:cs_kernel_codification('4c0eaafb-4481-4982-824e-120d80f35cdf', distributed).
narrative_ontology:cs_authority_grounding('4c0eaafb-4481-4982-824e-120d80f35cdf', distributed).
narrative_ontology:cs_reading_relation('4c0eaafb-4481-4982-824e-120d80f35cdf', woman_female_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('4c0eaafb-4481-4982-824e-120d80f35cdf', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('4c0eaafb-4481-4982-824e-120d80f35cdf', foundational, reproductive_biology_is_the_category_ground).
narrative_ontology:cs_axiom_status(reproductive_biology_is_the_category_ground, holdable).
narrative_ontology:cs_axiom_grounding('4c0eaafb-4481-4982-824e-120d80f35cdf', reproductive_biology_is_the_category_ground, empirically_contingent).
narrative_ontology:cs_axiom('4c0eaafb-4481-4982-824e-120d80f35cdf', secondary, sex_segregated_safety_spaces_require_biological_not_self_identified_criteria).
narrative_ontology:cs_axiom_status(sex_segregated_safety_spaces_require_biological_not_self_identified_criteria, holdable).
narrative_ontology:cs_axiom_grounding('4c0eaafb-4481-4982-824e-120d80f35cdf', sex_segregated_safety_spaces_require_biological_not_self_identified_criteria, instrumental).
narrative_ontology:cs_reference_frame('4c0eaafb-4481-4982-824e-120d80f35cdf', biological_sex_dimorphism_as_category_ground).
narrative_ontology:cs_drift_state('4c0eaafb-4481-4982-824e-120d80f35cdf', contemporary_legal_gender_recognition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4c0eaafb-4481-4982-824e-120d80f35cdf', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, womens_sports_organizations).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, domestic_violence_shelter_operators).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women_seeking_female_category_access).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_dimorphism_is_biologically_categorical).
narrative_ontology:constraint_vindicates(woman_female_category__sex_biology_reading, sex_based_protections_require_biological_definition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek single-sex spaces (prisons, shelters, changing rooms, sports categories) defined by biological sex on grounds of physical safety, privacy, and competitive fairness rooted in average sex-linked physiological differences. Benefit when category membership tracks biology because it excludes anyone with male puberty-derived physical advantages or male anatomy from spaces designed around female vulnerability or competitive parity. Cannot exit the category system itself; can only advocate for which definition governs it.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, national).

% Administer eligibility rules for female competitive categories. Under this reading, they set and enforce chromosomal/developmental criteria (or physiological proxies like testosterone thresholds tied to male puberty) to preserve competitive integrity of the female category, which they argue exists specifically because of average performance gaps produced by male puberty. Their institutional legitimacy depends on being able to defend a bounded category.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, womens_sports_organizations, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, womens_sports_organizations, agenda_setter).

% Operate residential facilities for women fleeing male violence. Under this reading, admission and space allocation (dormitories, showers) are organized around biological sex to manage the physical vulnerability and trauma responses of residents, many of whom have specific trauma responses to male bodies regardless of the source's gender identity. They enforce entry criteria and bear legal and reputational risk for either inclusion or exclusion decisions.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, domestic_violence_shelter_operators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, domestic_violence_shelter_operators, agenda_setter).

% Live and are legally/socially recognized as women in most contexts but are excluded from female-designated single-sex spaces and competitive categories under this reading because their chromosomal sex and developmental history (including any period of male puberty) do not meet the biological criterion. Bear the cost of exclusion from spaces whose absence can mean unsafe placement in male facilities, exclusion from sport, or social stigma. Exit is not meaningfully available — the category boundary is defined by unchangeable developmental history, not by a choice they can revise.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women_seeking_female_category_access, payer,
    powerless, biographical, trapped, national).

% Decide facility placement for incarcerated individuals. Under this reading, they place people according to chromosomal sex and anatomy rather than self-identified gender, citing documented elevated risk of sexual violence when housing decisions are made otherwise. They enforce this against both prisoner preference and, in some jurisdictions, against competing legal guidance that would use gender identity.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, prison_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Argue that biological criteria misdescribe the actual safety-relevant variable (which they contend is often social presentation, hormonal status, or intent to harm, not chromosomes) and that the sex-biology reading imports a categorical exclusion where an individualized or hormonally-mediated assessment would serve the same safety goals with less harm. Their position is represented in litigation and public discourse but is the losing position within the specific institutional structures (prisons, sports bodies, shelters) that have adopted this reading.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes between the sex-biology reading and competing readings, weighing safety evidence, equality law, and definitional consistency across statutes. Their rulings can shift which reading governs a given institutional domain without resolving the underlying kernel dispute.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, courts_and_legislatures, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, verifiable criterion (chromosomal sex, gamete production capacity, developmental history) for allocating access to spaces and categories built around the physiological realities of average sex differences — competitive sport fairness, physical safety in custodial and shelter settings, and clinical/medical categorization.
% TRANSFER_FUNCTION: Moves the benefit of guaranteed access to single-sex spaces and categories to those meeting the biological criterion, while moving the cost of categorical exclusion — from those spaces, from competitive categories, and in some cases from safer institutional placement — onto trans women who do not meet the biological criterion regardless of legal or social gender recognition.
% ABSENT_VOICES: Trans women excluded by this criterion are present in the debate through advocacy organizations but are structurally absent from the rule-setting bodies (sports federations, shelter boards, prison administrations) that adopt and enforce this reading; intersex individuals whose chromosomal/anatomical status does not cleanly sort into XX/XY are almost entirely unaddressed by this reading's binary framing.
% DISAPPEARANCE_RATIONALE: Proponents say the world would rearrange sharply and dangerously: female-only spaces would admit anyone self-identifying as a woman regardless of biology, which they claim reintroduces the specific safety and fairness harms the category was built to prevent. Opponents say the world would rearrange in the other direction — trans women currently excluded would gain access, and no catastrophic safety effect would follow because the category was never doing the safety work claimed. Because the two sides dispute the underlying empirical and moral premises, not just the outcome, the verdict is genuinely contested rather than resolvable by stipulation.
% FOUNDING_PROBLEM: The category 'woman/female' needed a definition robust enough to organize sex-segregated spaces (sport, custody, shelters, medicine) that exist because of documented average physiological differences between sexes — particularly differences bearing on physical safety and athletic performance.
% FOUNDING_PROBLEM_CORROBORATION: Sports physiologists and some safety researchers, who are not themselves beneficiaries of either reading's political outcome, attest that average post-pubertal physiological differences between the sexes are real and persist in most cases even after hormone therapy, supporting the claim that the founding problem is live for competitive and some safety contexts. Independent legal scholars outside the advocacy organizations on either side note that the same biological differences do not straightforwardly justify exclusion in non-competitive, non-custodial social and legal contexts, which is why the dispute has fragmented into competing readings rather than resolving to one.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, contested).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).
:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than extreme: the reading produces a genuine, non-trivial coordination function (organizing safety-relevant and fairness-relevant space around real average physiological differences) alongside a real, concentrated cost borne by an identifiable group (trans women denied access). Suppression (0.55) reflects that maintaining the biological boundary against a competing legal and social gender-recognition framework requires active enforcement — legislation, litigation, institutional policy — not passive persistence. Theater ratio is low (0.2) because the enforcement mechanisms (medical/legal verification, facility placement policy, sport eligibility panels) do substantive gatekeeping work rather than merely performing it. Resistance is high (0.72) because trans rights organizations, courts, and shifting social consensus actively contest the boundary in ways a settled natural-law constraint would not encounter. Accessibility collapse is moderate (0.4): alternative criteria (self-identification, hybrid context-dependent rules) remain live, contested, and adopted in some jurisdictions — the biological criterion has not achieved anything close to universal, uncontested acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal females seeking sex-based protections and the institutions serving them (sports bodies, shelters, prisons) sit near the beneficiary end: the reading is constructed to serve their stated safety and fairness interests, and they retain agenda-setting power over its application. Trans women sit near the full-target end: the constraint's entire function, under this reading, is to exclude them from a category they otherwise occupy in most social and legal contexts, and this exclusion tracks unchangeable developmental history rather than any revisable choice — hence exit_options: trapped rather than constrained. This is a case where exit is not merely difficult but structurally unavailable: no amount of legal transition changes chromosomal or developmental history, so the excluded population cannot exit the victim category by any action available to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) is deliberate: this reading does possess a genuine, non-fabricated coordination function — average sex-linked physiological differences bearing on athletic performance and physical vulnerability are real and documented, and organizing certain safety-critical and competitive-fairness domains around them solves an actual problem. Calling this reading a pure snare would erase the safety and fairness interests of natal females that motivate its beneficiary group. But calling it a pure rope would erase the concentrated, unchosen cost borne by trans women, who have no meaningful exit from the excluded category. The tangled_rope label holds both: real coordination value AND real, asymmetric, actively-enforced extraction from an identifiable victim group, resolved neither toward pure legitimacy nor pure illegitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developmental_history_vs_current_presentation,
    'Is chromosomal/developmental history the structurally correct proxy for the safety-relevant and fairness-relevant variable (physical strength, vulnerability, threat profile), or does that variable track current hormonal status and social presentation more accurately for at least some of the domains this reading governs?',
    'Domain-specific empirical study: post-transition athletic performance retention studies for sport; documented incident data for shelter and prison safety outcomes under biological vs. hormonal vs. self-identification criteria.',
    'If current hormonal status/presentation is the better proxy in a given domain, the sex-biology reading''s coordination function in that specific domain is weaker than claimed, shifting the classification in that domain toward the extraction end even while the sport-eligibility domain''s stronger physiological evidence base holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_history_vs_current_presentation, empirical, 'Whether chromosomal/developmental criteria or current status/presentation better tracks the safety and fairness variables this reading claims to protect.').

omega_variable(
    intersex_category_gap,
    'How does this reading''s binary chromosomal/developmental criterion handle individuals whose chromosomal sex, anatomy, and hormonal profile do not sort cleanly into XX/male-typical or XY/male-typical development (intersex variations)?',
    'Review of how institutions applying this reading (sports federations, prisons, shelters) have actually resolved intersex cases in practice, versus how the reading''s stated criteria would predict.',
    'If intersex cases are resolved by ad hoc exception rather than by the stated biological criterion, that indicates the criterion is less purely biological in practice than claimed, which would raise the constructed-vs-natural ambiguity for this reading itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intersex_category_gap, conceptual, 'Whether the binary biological criterion coherently covers intersex variation or requires undeclared exception-handling.').

omega_variable(
    reading_selection_and_committer_framing,
    'Given three declared readings of the woman_female_category kernel (sex_biology, gender_identity, hybrid_contextual), is the choice to author this reading as the primary institutional frame in sport/prison/shelter domains a reflection of which reading is empirically or ethically superior, or a reflection of which institutions currently hold rule-setting power in those specific domains?',
    'Compare adoption patterns across jurisdictions and institutions with differing political and legal environments; track whether adoption correlates with empirical safety/fairness evidence or with the composition of the rule-setting body.',
    'If adoption correlates primarily with rule-setter composition rather than evidence, this reading''s institutional dominance in these domains is better described as a contingent political outcome than as convergence on the structurally correct reading — which would not change this story''s own ε but would bear on how much weight the corpus should give to any single reading''s institutional entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_committer_framing, conceptual, 'Whether this reading''s current institutional dominance reflects evidentiary convergence or contingent rule-setting power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__sex_biology_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__sex_biology_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__sex_biology_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__sex_biology_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(woma_be_t4, woman_female_category__sex_biology_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(woma_be_t8, woman_female_category__sex_biology_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(woma_be_t12, woman_female_category__sex_biology_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(woma_be_t16, woman_female_category__sex_biology_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(woma_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t4, woman_female_category__sex_biology_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(woma_su_t8, woman_female_category__sex_biology_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(woma_su_t12, woman_female_category__sex_biology_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(woma_su_t16, woman_female_category__sex_biology_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(woma_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.1).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the woman_female_category kernel. woman_female_category__gender_identity_reading inverts the beneficiary/victim structure (trans women as beneficiaries; natal-female-only advocates as the constrained payer group in that reading's frame). woman_female_category__hybrid_contextual_reading applies this reading's criterion only to medical/sports/safety contexts and the gender_identity_reading's criterion to social/legal contexts, producing a third distinct ε and beneficiary/victim split. All three are ε-invariant, independently classified constraints and must not be merged or averaged; they are linked here solely to support contamination-propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
