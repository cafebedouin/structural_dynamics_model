% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender-Identity Reading of the Woman Category (Self-Identification Standard)
 *   domain: political philosophy / law / social policy / bioethics
 *
 * SUMMARY:
 *   This story instantiates the gender-identity reading of the contested
 *   'woman' category kernel: category membership is determined by
 *   self-declared internal gender identity, independent of natal sex. Under
 *   this reading, the standing arrangement under contest is the current
 *   patchwork of self-identification policies now governing legal sex
 *   markers, single-sex sports categories, prisons, and shelters in several
 *   jurisdictions. The reading treats sex-based eligibility criteria as
 *   themselves a form of discriminatory exclusion, and treats
 *   self-identification as the legitimate and sufficient test. This is a
 *   distinct constraint from the sex-biology reading (which authors ε for the
 *   same domain from the opposite premise, with a different victim set) and
 *   from the intersex-accommodation reading (which rejects binary sufficiency
 *   entirely). Per the ε-invariance principle, each reading is authored as
 *   its own file with its own stable ε; they are linked, not merged.
 *
 * KEY AGENTS:
 *   - transgender_women: primary beneficiary of recognition (moderate power, identity-locked exit)
 *   - gender_identity_legal_advocates: agenda-setters who established the self-identification standard across venues
 *   - natal_women_in_sex_segregated_sports: payers who lose competitive access under this reading
 *   - natal_women_in_carceral_and_shelter_settings: payers bearing involuntary, high-stakes exposure
 *   - detransitioners_and_gender_critical_feminists: excluded dissenting voices
 *   - inclusive_institutions_seeking_liability_shield: beneficiaries administering the standard at low direct cost to themselves
 *   - courts_and_regulators: analytical observers adjudicating the underlying kernel dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.52).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender-Identity Reading of the Woman Category (Self-Identification Standard)").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political philosophy / law / social policy / bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, 'c5b36c3a-bbee-4473-9f35-8449bcf97817').
narrative_ontology:cs_kernel_codification('c5b36c3a-bbee-4473-9f35-8449bcf97817', distributed).
narrative_ontology:cs_authority_grounding('c5b36c3a-bbee-4473-9f35-8449bcf97817', distributed).
narrative_ontology:cs_reading_relation('c5b36c3a-bbee-4473-9f35-8449bcf97817', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('c5b36c3a-bbee-4473-9f35-8449bcf97817', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('c5b36c3a-bbee-4473-9f35-8449bcf97817', foundational, gender_identity_is_dispositive_of_category_membership).
narrative_ontology:cs_axiom_status(gender_identity_is_dispositive_of_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('c5b36c3a-bbee-4473-9f35-8449bcf97817', gender_identity_is_dispositive_of_category_membership, deontological).
narrative_ontology:cs_axiom('c5b36c3a-bbee-4473-9f35-8449bcf97817', secondary, sex_based_eligibility_criteria_are_inherently_discriminatory).
narrative_ontology:cs_axiom_status(sex_based_eligibility_criteria_are_inherently_discriminatory, holdable).
narrative_ontology:cs_axiom_grounding('c5b36c3a-bbee-4473-9f35-8449bcf97817', sex_based_eligibility_criteria_are_inherently_discriminatory, conventional).
narrative_ontology:cs_reference_frame('c5b36c3a-bbee-4473-9f35-8449bcf97817', pre_recognition_medicalized_gatekeeping_regime).
narrative_ontology:cs_drift_state('c5b36c3a-bbee-4473-9f35-8449bcf97817', contemporary_sports_and_carceral_policy_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c5b36c3a-bbee-4473-9f35-8449bcf97817', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, gender_identity_legal_advocates).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, inclusive_institutions_seeking_liability_shield).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_women_in_sex_segregated_sports).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, natal_women_in_carceral_and_shelter_settings).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, detransitioners_and_gender_critical_feminists).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_identity_is_the_operative_criterion_of_sex_category).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek legal and social recognition as women consistent with their identity, gaining access to documents, facilities, sports categories, and anti-discrimination protections keyed to womanhood. Their standing under this reading is not optional or strategic — the identity is the basis of the claim, and exit from the category claim is not a live option for them.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, national).

% Litigate, lobby, and draft policy language establishing self-identification as the legal test for sex/gender category membership across employment, health, prisons, sports, and single-sex services. They set the definitional agenda that institutions then adopt, and can redirect strategy across venues if one forum resists.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_identity_legal_advocates, agenda_setter,
    organized, generational, mobile, national).

% Compete in categories historically segregated by sex for reasons tied to average physical performance differentials. Under this reading, eligibility for the women's category is determined by identity rather than by the biological basis the category was built to track, and they report loss of competitive opportunity, podium positions, and scholarships. Their exit options are limited to leaving the sport or accepting the redefined category.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_women_in_sex_segregated_sports, payer,
    moderate, biographical, constrained, national).

% Are housed in women's prisons, shelters, or refuges alongside anyone who identifies as a woman, including some individuals convicted of violence against women. They did not choose this housing and typically cannot exit the institution; their vulnerability is structurally involuntary in a way the sports context is not.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, natal_women_in_carceral_and_shelter_settings, payer,
    powerless, immediate, trapped, regional).

% Argue that sex-based rather than identity-based categories are necessary to protect female-specific interests (medical, safety, sporting), including some who formerly identified as transgender. They report being excluded from advocacy coalitions, institutional consultation processes, and public platforms as the price of dissenting from the self-identification standard; their objections are treated as illegitimate rather than adjudicated.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, detransitioners_and_gender_critical_feminists, excluded,
    powerless, biographical, constrained, national).

% Adopt self-identification policies partly to avoid discrimination litigation and reputational risk, and partly from genuine commitment. They administer the resulting rules and bear little direct cost from category disputes, since the costs land on the excluded or displaced parties rather than on the institution itself.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, inclusive_institutions_seeking_liability_shield, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, inclusive_institutions_seeking_liability_shield, agenda_setter).

% Adjudicate disputes between sex-based and identity-based readings of statutory sex categories (equality law, sports governance, prison policy). They receive competing evidentiary submissions and are the forum where the kernel contest is formally litigated.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, courts_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, humane, non-invasive administrative test (self-declared identity) for who counts as a woman for legal and social purposes, avoiding medicalized or biologically invasive verification and reducing the dignitary harm of intrusive sex-checking.
% TRANSFER_FUNCTION: Moves access rights (to women's sports categories, single-sex spaces, and legal sex-marker changes) from the group defined by natal female sex to the broader group defined by self-identified gender, and moves the risk/cost of category-boundary disputes onto natal women in involuntary or safety-sensitive settings.
% ABSENT_VOICES: Natal women who do not organize around this issue, intersex people whose bodies do not fit either binary category cleanly, and detransitioners are largely outside the rooms where this standard is adopted; the excluded stakeholders here are treated as making the standard's legitimacy question unaskable rather than as a party to be negotiated with.
% DISAPPEARANCE_RATIONALE: If the self-identification standard were withdrawn, transgender women's legal and institutional recognition as women would revert to case-by-case, often medicalized or excluded status; sports and carceral/shelter eligibility rules would revert to sex-based tests; ongoing litigation and policy across many institutions would need to be re-decided under a different criterion.
% FOUNDING_PROBLEM: Historically, transgender people faced total legal non-recognition, forced sterilization or invasive surgical requirements as a precondition for any legal recognition, and exclusion from documents, employment, and public life consistent with their identity — the self-identification standard was built to end those specific harms.
% FOUNDING_PROBLEM_CORROBORATION: Human rights bodies and clinical gender-medicine associations attest that non-invasive legal recognition remains a live, unresolved need for transgender people generally. Sports governing bodies' own commissioned physiological reviews and several national courts and regulators — parties outside the advocacy coalition that authored the standard — report that in sex-segregated sport and single-sex safety contexts the founding problem (non-invasive dignified recognition) has become entangled with a separate, contested claim (that identity alone determines eligibility for categories built around average sex-based performance or vulnerability differentials), and that these are not the same problem.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored as moderate-to-substantial (0.58) rather than extreme, because the reading genuinely solves a real coordination problem (non-invasive legal recognition for transgender people, ending forced-sterilization-era exclusion) alongside its extractive component. Extraction is highest precisely where 'access rights' for one group collide structurally with 'exclusion rights' for another over a fixed-capacity good — sports podium slots, single-sex refuge bed space, and carceral housing assignments cannot be shared the way identity-document policy can, so the constraint is markedly more extractive in those domains than in documentation. Suppression (0.52) reflects the institutional and reputational cost imposed on dissent (deplatforming, professional consequences, exclusion from consultation) rather than criminal-legal coercion. Theater ratio is comparatively low (0.28) because the core recognition function is genuinely operative, not merely performed — this is not a piton.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and institutional-beneficiary seats, this reading is coordination: a humane administrative simplification replacing invasive, humiliating verification. From the payer seats in sports and involuntary institutional settings, the same structure is experienced as an imposed reallocation of a scarce, sex-conditioned good performed under the banner of anti-discrimination. The engine should compute these seats as diverging in type precisely because both readings are structurally accurate to their own position — that divergence, not an error in either seat, is what the tangled_rope classification is measuring.
 *
 * DIRECTIONALITY LOGIC:
 *   Transgender women and their advocates sit toward the beneficiary end: the reading directly expands their recognized standing and access. Natal women in sports sit toward the target end but with constrained (not trapped) exit — they can leave competitive sport, at real personal and economic cost. Natal women in prisons and shelters sit nearest the full-target end because their exposure is involuntary and their exit option is trapped, not merely constrained — this is why they are authored as a distinct, more severe victim group rather than folded into the sports group. Institutions administering the policy are beneficiaries with arbitrage-grade exit: they can adjust policy language without bearing the category-boundary costs themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ending invasive, medicalized, dignity-destroying tests for legal recognition) remains substantially live and is corroborated outside the advocacy coalition by human rights and clinical bodies — this blocks a simple 'the mandate is dead, this is now pure extraction' reading. But the founding problem has been extended, largely without separate justification, from documentation and general public life (low-collision domains) into fixed-capacity, safety- and performance-conditioned domains (sports, carceral housing) where a different empirical question (average sex-linked physical differentials, single-sex vulnerability) is doing real work that identity alone does not resolve. Classifying this as tangled_rope rather than snare or rope preserves both facts: genuine coordination function for the low-collision core, genuine asymmetric extraction at the high-collision margins, both riding the same definitional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_for_fixed_capacity_domains,
    'Should a single category-membership standard govern both low-collision domains (documents, general public accommodation) and high-collision, fixed-capacity domains (sports categories, single-sex shelters, carceral housing), or does the coordination/extraction balance require domain-specific tests?',
    'Compare outcomes across jurisdictions that have adopted domain-differentiated policy (identity-based for documents, sex-based or hybrid for sports/housing) against jurisdictions using a uniform identity standard, on measures of both transgender-recognition harm and natal-women displacement/safety incidents.',
    'If domain-differentiated policy achieves comparable recognition benefits with lower displacement/safety cost, that would support decomposing this single reading into domain-specific constraints rather than treating ''woman category membership'' as one policy question; if not, it would support the uniform reading''s structural claim that any carve-out reintroduces the harm the standard was built to end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_for_fixed_capacity_domains, empirical, 'Whether a uniform identity-based standard or a domain-differentiated standard better serves the coordination function without the extractive collision.').

omega_variable(
    kernel_contest_adjudication_authority,
    'Which reading of the woman_category kernel should courts and regulators treat as authoritative when the three readings (gender identity, sex biology, intersex accommodation) yield incompatible eligibility results for the same fixed-capacity good?',
    'Track appellate and legislative resolution patterns across jurisdictions; note whether resolution proceeds by domain-specific carve-out, by strict hierarchy of one reading over the others, or by continued unresolved fragmentation.',
    'A domain-specific carve-out resolution would validate treating this as a family of related-but-distinct constraints rather than a single global policy; a strict-hierarchy resolution would elevate one reading''s ε and victim set as the operative legal fact within that jurisdiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_adjudication_authority, conceptual, 'Where formal legal authority will ultimately locate itself among the three sibling readings of the kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression against dissenting voices (detransitioners, gender-critical feminists, some natal-women''s sports advocates) primarily structural (institutional exclusion from consultation, employment consequences) or partly internalized (self-censorship from anticipated social cost)?',
    'Survey dissenting stakeholders on whether they would resume public advocacy if institutional consequences (deplatforming, employment risk) were removed; persistence of silence after removal would indicate an internalized component.',
    'If substantially internalized, the effective suppression experienced by excluded voices is higher than the structural measure alone suggests, and would not fully reverse even if institutional policy relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of dissenting voices is structural, internalized, or mixed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(woma_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(woma_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(woma_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(woma_tr_t16, woman_category__gender_identity_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(woma_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(woma_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(woma_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(woma_be_t16, woman_category__gender_identity_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(woma_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(woma_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(woma_su_t16, woman_category__gender_identity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__gender_identity_reading, 0.1).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% Part of the woman_category constraint family (3 readings of one contested kernel). This story (gender_identity_reading) shares no ε value with its siblings by design — each reading authors its own beneficiary/victim structure and its own extraction profile per the ε-invariance principle. The sex_biology_reading inverts the victim set (natal women become the protected class; transgender women become the excluded party). The intersex_accommodation_reading rejects both binary premises and authors a third, distinct victim set (intersex people mis-sorted by either binary test). All three link to each other via affects_constraints to preserve the family structure for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
