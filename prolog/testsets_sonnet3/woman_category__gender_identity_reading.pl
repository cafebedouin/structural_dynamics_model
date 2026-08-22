% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender-Identity Reading of the 'Woman' Category
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'woman' category
 *   kernel: the gender-identity reading, under which category membership is
 *   determined entirely by internal gender identity, with no biological-sex
 *   threshold. The reading emerged primarily to solve genuine non-recognition
 *   harms faced by transgender people under sex-only legal frameworks, and
 *   functions as coordination for that population. But as the reading has
 *   been adopted into eligibility and access rules for sex-segregated sport
 *   and facilities historically organized around reproductive/developmental
 *   sex differences, it has generated a second population — natal women in
 *   those specific contexts — who bear costs through the same structure that
 *   delivers the coordination benefit. This is why the story claims
 *   tangled_rope rather than rope or snare: a real coordination function
 *   coexists with an asymmetric extraction that requires active institutional
 *   enforcement (litigation, policy mandates, sports-body rule changes) to
 *   sustain against resistance. Two sibling readings of the same kernel
 *   (sex_biology_reading, intersex_accommodation_reading) are NOT part of
 *   this file; they carry their own ε and stakeholder sets in separate
 *   constraint stories and are linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - transgender_women: primary beneficiary (moderate/identity_locked) — gains recognition and access under this reading's criterion
 *   - gender_identity_rights_advocacy_organizations: agenda-setter (organized/mobile) — drafts and enforces the self-identification standard
 *   - natal_women_in_sex_segregated_sport: primary payer in the sport domain (moderate/constrained) — displaced from historically sex-based competitive categories
 *   - natal_women_in_carceral_and_shelter_settings: primary payer in the facility-access domain (powerless/trapped) — assigned housing without choice
 *   - sex_based_protection_advocates: excluded/payer (organized/constrained) — reframed as discriminatory for asserting a competing claim
 *   - courts_and_regulators: analytical observer (institutional/analytical) — adjudicates between this reading and its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.62).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender-Identity Reading of the 'Woman' Category").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '35b31e3a-b0d2-4d99-ba27-789355f3d987').
narrative_ontology:cs_kernel_codification('35b31e3a-b0d2-4d99-ba27-789355f3d987', distributed).
narrative_ontology:cs_authority_grounding('35b31e3a-b0d2-4d99-ba27-789355f3d987', distributed).
narrative_ontology:cs_reading_relation('35b31e3a-b0d2-4d99-ba27-789355f3d987', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('35b31e3a-b0d2-4d99-ba27-789355f3d987', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('35b31e3a-b0d2-4d99-ba27-789355f3d987', foundational, identity_is_sufficient_for_category_membership).
narrative_ontology:cs_axiom_status(identity_is_sufficient_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('35b31e3a-b0d2-4d99-ba27-789355f3d987', identity_is_sufficient_for_category_membership, deontological).
narrative_ontology:cs_axiom('35b31e3a-b0d2-4d99-ba27-789355f3d987', foundational, biology_is_not_necessary_for_category_membership).
narrative_ontology:cs_axiom_status(biology_is_not_necessary_for_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('35b31e3a-b0d2-4d99-ba27-789355f3d987', biology_is_not_necessary_for_category_membership, deontological).
narrative_ontology:cs_reference_frame('35b31e3a-b0d2-4d99-ba27-789355f3d987', sex_based_legal_categories_pre_gender_recognition_statutes).
narrative_ontology:cs_drift_state('35b31e3a-b0d2-4d99-ba27-789355f3d987', contemporary_gender_recognition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35b31e3a-b0d2-4d99-ba27-789355f3d987', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_rights_advocacy_organizations).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_women_in_sex_segregated_sport).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_women_in_carceral_and_shelter_settings).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_based_protection_advocates).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, self_identification_as_sufficient_category_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition as women consistent with their identified gender. Under this reading, they gain access to sex-segregated spaces, documents, and categories (sport, prisons, shelters, changing rooms) without a biological-sex threshold. Their exit from this framework would mean returning to a classification scheme many experience as a denial of their identity; the relationship to the category is not one of convenience but of selfhood.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Draft model legislation, litigate test cases, and lobby institutions (sports bodies, prisons, medical boards) to adopt self-identification as the sole criterion for category membership. They administer public messaging that frames the reading as settled civil-rights doctrine and treat sex-based counter-claims as discriminatory. They face few personal costs if the framework fails to hold in any given jurisdiction; their institutional position is comparatively mobile relative to the people whose access rights are directly at stake.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_rights_advocacy_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, gender_identity_rights_advocacy_organizations, beneficiary).

% Compete in categories historically segregated by sex to offset average physical differences arising from male puberty. Under this reading, category membership admits anyone who identifies as a woman regardless of that developmental history, which they experience as displacing them from podiums, scholarships, and safety in contact sports. Their only exits are to leave competitive sport entirely or to compete in an open category with structurally reduced prospects.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_women_in_sex_segregated_sport, payer,
    moderate, biographical, constrained, national).

% Are housed in single-sex prisons, domestic violence shelters, or rape crisis facilities where the population is determined by self-identified gender rather than sex. Some report loss of privacy or safety they entered these facilities specifically to secure. They typically have no choice of facility and no capacity to exit the housing decision once assigned.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_women_in_carceral_and_shelter_settings, payer,
    powerless, immediate, trapped, local).

% Argue that some legal protections and spaces exist specifically because of reproductive biology and vulnerability patterns tied to sex, not gender identity, and that collapsing the categories removes protections without consent of the protected group. Under this reading their position is characterized as exclusionary or discriminatory in public discourse and increasingly in law, narrowing their institutional standing and funding even though their claims concern a distinct population (natal women) from the one this reading centers.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_protection_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, sex_based_protection_advocates, excluded).

% Must set eligibility rules under legal and reputational pressure from both this reading and the sex-biology reading simultaneously. Adopting self-identification avoids discrimination liability under some jurisdictions' law but generates fairness and safety complaints from competitors and federations under others; they administer whichever policy is currently enforceable and bear reputational cost for either choice.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sports_governing_bodies, agenda_setter,
    institutional, biographical, constrained, global).

% Adjudicate disputes between this reading and the sex-biology and intersex-accommodation readings in litigation over sport, prisons, and anti-discrimination statutes. Their rulings determine which reading has legal force in a given jurisdiction at a given time, and they take testimony and evidence from all sides.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively simple criterion (self-identification) for sorting people into sex-differentiated legal and social categories, avoiding the need for medical verification, chromosomal testing, or gatekeeping bureaucracy, and extending recognition to people whose lived gender differs from their sex assigned at birth.
% TRANSFER_FUNCTION: Moves access to sex-segregated categories, spaces, and resources (sporting categories, competitive placements, single-sex facility admission, certain legal protections) from populations previously defined by reproductive/developmental sex to populations defined by self-identified gender, and moves reputational and institutional standing away from advocates of sex-based criteria.
% ABSENT_VOICES: Natal women who report safety or fairness harms in specific contested settings (contact sport, custodial and shelter facilities) are frequently characterized as bad-faith actors or bigots in the advocacy framing this reading relies on, which forecloses their objections from being heard as good-faith competing-rights claims rather than dismissed as hostility.
% DISAPPEARANCE_RATIONALE: If self-identification were withdrawn as the sole criterion for category membership overnight, transgender women would lose legal recognition and access currently secured under this reading in numerous jurisdictions, sports bodies would revert to sex-based or hormone-threshold eligibility tests, and carceral/shelter placement would revert to sex-based assignment — a substantial population's day-to-day legal status and access would visibly change.
% FOUNDING_PROBLEM: Transgender people faced systematic denial of legal recognition, employment, healthcare access, and safety in identity-document and public-accommodation systems that recognized only assigned sex at birth, with no route to correction regardless of transition status.
% FOUNDING_PROBLEM_CORROBORATION: Transgender advocacy organizations and many transgender individuals attest the founding problem (non-recognition, discrimination, and safety risk absent legal gender recognition) remains substantially live. Independent legal scholars and some feminist and disability-rights organizations outside both benefiting camps attest that the founding problem of non-recognition is real but contest whether self-identification-only criteria, as opposed to narrower gender-recognition mechanisms with some verification step, is the necessary or proportionate solution — corroboration for the specific self-ID mechanism, as opposed to the underlying recognition problem, comes substantially from the advocacy organizations themselves.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-to-high 0.58 because the reading's effects diverge sharply by domain: near-negligible in most identity-document and employment contexts (where self-identification imposes minimal cost on third parties) but severe in sports eligibility and sex-segregated intimate/custodial spaces (where a genuine average physical or vulnerability differential is displaced by the category change). The single ε authored here reflects the constraint AS THIS READING'S OWN OPERATION ACROSS ITS full domain, weighted toward the contested high-stakes applications where the coordination/extraction tension is sharpest — consistent with the ε-invariance principle, since this reading (not a blended average across kernel readings) is the single constraint being measured. Suppression (0.62) reflects the increasing use of legal, institutional, and reputational mechanisms (discrimination litigation, professional deplatforming, statutory redefinition) to foreclose the sex-biology criterion as an available alternative, which is a raw structural fact independent of scope or power scaling. Accessibility collapse is moderate (0.4) because sex-based alternatives remain available in some jurisdictions and institutions even as they collapse in others — this is not yet a completed, irreversible closure. Resistance is high (0.72): natal-women's advocacy groups, some sports federations, and several jurisdictions actively contest the reading's application to specific domains, which is inconsistent with a settled coordination arrangement and consistent with tangled_rope's requirement of active enforcement against live resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (transgender women) and the agenda-setter seat (advocacy organizations), the reading is experienced and framed as pure coordination — correcting a historical injustice with no legitimate victims, since sex-based counter-claims are treated as bad-faith bigotry. From the payer seats (natal women in sport and custodial settings), the same structure is experienced as extraction — the loss of a protection or competitive standing they did not consent to lose, defended by institutional and legal enforcement they cannot resist through ordinary channels. The engine computes these as different seat-level classifications from the same structural data; this file does not resolve which perspective is correct, only that the divergence is structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women are declared beneficiaries because the reading's core function — recognition consistent with lived identity — subsidizes their legal and social standing; their exit options are identity_locked because the category question is constitutive of self-understanding, not a matter of convenience. Natal women in sport and custodial/shelter settings are declared victims/payers because the same self-identification criterion that benefits transgender women, applied in those specific high-stakes domains, displaces protections or competitive standing they held under the sex-based criterion, without their consent to the change; their exit options are constrained-to-trapped because leaving the domain (sport, the facility) is the only alternative to bearing the cost. Advocacy organizations are agenda-setters with mobile exit — they bear little direct personal cost if a given jurisdiction's adoption of the reading is later reversed by courts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (systematic non-recognition and discrimination against transgender people under sex-only category schemes) is real and, per the corroboration field, substantially still live — this is why founding_problem_status is authored 'contested' rather than 'dead': the underlying recognition problem persists, but whether the specific self-identification-only MECHANISM (as opposed to narrower gender-recognition procedures with some verification step, as in the intersex_accommodation_reading's more granular approach) is the necessary solution is itself disputed by parties outside the advocacy coalition. Classifying this as tangled_rope rather than snare or rope prevents two mislabelings: calling it a pure snare would deny the real coordination benefit to transgender people; calling it a pure rope would deny the asymmetric, actively-enforced cost imposed on natal women in specific contested domains. The tangled_rope label holds both facts simultaneously without resolving the underlying political dispute, which is exactly what the classification is for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_identification_sufficiency_ambiguity,
    'Is self-identification alone a sufficient and non-arbitrary criterion for category membership in contexts where the category''s original social function (competitive fairness in sport, safety in custodial/shelter settings) was organized around a biological differential rather than identity per se?',
    'Comparative institutional analysis across jurisdictions that have adopted narrower gender-recognition procedures (e.g., verification periods, hormone thresholds) versus pure self-identification, tracking downstream fairness and safety outcomes in contested domains.',
    'If self-identification alone proves adequate to preserve the functions the category served, the tangled_rope classification''s victim set shrinks toward negligible extraction; if a biological differential proves load-bearing for those specific functions, the extraction in sport/custodial domains is structurally irreducible under this reading as currently specified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_identification_sufficiency_ambiguity, conceptual, 'Whether self-identification is a workable substitute for biology in the specific high-stakes domains where this reading is most contested.').

omega_variable(
    reading_relationship_to_siblings,
    'How does adoption of the gender_identity_reading in one jurisdiction''s law structurally affect the legal viability of the sex_biology_reading and intersex_accommodation_reading in the same jurisdiction?',
    'Track statutory and case-law outcomes where jurisdictions adopt self-identification as the sole legal criterion and observe whether sex-based protections survive as a parallel legal category or are struck down as discriminatory under the new standard.',
    'If adoption of this reading tends to foreclose sex-based legal categories entirely (rather than letting them coexist), the reading_relations edge to sex_biology_reading should be reconsidered toward forecloses in specific legal contexts even though it is authored coexists_with here at the framework level; if sex-based categories persist as a parallel legal option, coexists_with remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relationship_to_siblings, empirical, 'Whether this reading''s legal adoption structurally displaces the sex_biology_reading or merely coexists with it.').

omega_variable(
    extraction_domain_heterogeneity,
    'Should this reading actually be decomposed further — e.g. into a ''documents and employment'' constraint (low ε) and a ''sport and intimate/custodial spaces'' constraint (high ε) — rather than authored as one ε-invariant story spanning both?',
    'Apply the ε-invariance test directly: if a future audit finds that measuring ε via the documents/employment domain alone versus the sport/custodial domain alone yields two clearly different values (which this story''s own commentary suggests), the ε-invariance principle requires splitting into two separate constraint files rather than retaining one blended ε.',
    'A split would produce a cleaner rope-like story for documents/employment and a starker tangled_rope-or-snare story for sport/custodial domains, sharpening the classification at the cost of losing the single unified ''gender identity reading'' narrative this file currently presents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_domain_heterogeneity, conceptual, 'Whether this single story should itself be decomposed per the ε-invariance principle rather than blending two domains with divergent extraction profiles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(woma_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(woma_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(woma_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(woma_tr_t16, woman_category__gender_identity_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(woma_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(woma_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_category__gender_identity_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(woma_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(woma_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(woma_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(woma_su_t16, woman_category__gender_identity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language 'woman category' kernel per the ε-invariance principle. sex_biology_reading anchors membership to chromosomal/anatomical/reproductive biology (its own ε, victim set: transgender people denied recognition). intersex_accommodation_reading treats sex as a biological spectrum including intersex variation (its own ε, victim set: intersex people mis-sorted by a strict binary). gender_identity_reading (this file) anchors membership to self-identified gender alone (ε=0.58, victim set: natal women in sex-segregated high-stakes domains, transgender women as beneficiaries). All three share the same underlying social dispute but are structurally distinct constraints with different beneficiary/victim structures and different ε — they are linked here rather than merged into one story with an observable-dependent ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
