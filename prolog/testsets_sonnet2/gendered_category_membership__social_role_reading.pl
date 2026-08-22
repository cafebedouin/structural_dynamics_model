% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Category Membership via Sustained Social Performance and Recognition
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates one reading within the contested kernel of
 *   gendered category membership: membership grounded in sustained social
 *   performance and recognition by others, rather than in birth anatomy (the
 *   biological_sex_reading) or in self-declared identity (the
 *   gender_identity_reading). Under this reading, whether a person counts as
 *   a woman in a given social space is settled continuously, interaction by
 *   interaction, by whether other people extend recognition — which depends
 *   on presentation, voice, history, and context rather than on a fixed
 *   criterion available at a single point of adjudication. This produces a
 *   distinctive structural signature: conditional inclusion for trans women
 *   who pass, distributed (rather than centralized) gatekeeping performed by
 *   ordinary social actors and institutional administrators alike, and a
 *   victim class that is NOT limited to trans individuals —
 *   gender-nonconforming cis women are policed by the identical mechanism,
 *   since the recognition standard cannot distinguish 'insufficiently passing
 *   trans woman' from 'insufficiently feminine cis woman' without smuggling
 *   in exactly the biological or identity criteria this reading sets aside.
 *
 * KEY AGENTS:
 *   - socially_recognized_passing_individuals: primary conditional beneficiary (moderate/constrained) — gains access through successful performance, precariously
 *   - non_passing_trans_women: primary target (powerless/trapped) — excluded by audience judgment regardless of sincerity
 *   - gender_nonconforming_cis_women: secondary target (powerless/constrained) — swept into a standard built for a different population
 *   - gatekeeping_incumbent_women and institutional administrators: agenda-setters (organized-institutional/arbitrage-constrained) — hold discretionary boundary-setting power
 *   - biological_sex_reading_advocates and gender_identity_reading_advocates: excluded — hold the sibling framings this story does not adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.38).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.42).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Category Membership via Sustained Social Performance and Recognition").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'abc3fbde-879f-40e0-994b-71d255e8ed6a').
narrative_ontology:cs_kernel_codification('abc3fbde-879f-40e0-994b-71d255e8ed6a', distributed).
narrative_ontology:cs_authority_grounding('abc3fbde-879f-40e0-994b-71d255e8ed6a', distributed).
narrative_ontology:cs_reading_relation('abc3fbde-879f-40e0-994b-71d255e8ed6a', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('abc3fbde-879f-40e0-994b-71d255e8ed6a', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('abc3fbde-879f-40e0-994b-71d255e8ed6a', foundational, membership_is_constituted_by_intersubjective_recognition).
narrative_ontology:cs_axiom_status(membership_is_constituted_by_intersubjective_recognition, holdable).
narrative_ontology:cs_axiom_grounding('abc3fbde-879f-40e0-994b-71d255e8ed6a', membership_is_constituted_by_intersubjective_recognition, conventional).
narrative_ontology:cs_axiom('abc3fbde-879f-40e0-994b-71d255e8ed6a', secondary, sustained_performance_generates_defeasible_standing).
narrative_ontology:cs_axiom_status(sustained_performance_generates_defeasible_standing, holdable).
narrative_ontology:cs_axiom_grounding('abc3fbde-879f-40e0-994b-71d255e8ed6a', sustained_performance_generates_defeasible_standing, conventional).
narrative_ontology:cs_reference_frame('abc3fbde-879f-40e0-994b-71d255e8ed6a', everyday_face_to_face_social_sorting).
narrative_ontology:cs_drift_state('abc3fbde-879f-40e0-994b-71d255e8ed6a', contemporary_institutional_formalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abc3fbde-879f-40e0-994b-71d255e8ed6a', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, gatekeeping_incumbent_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, socially_recognized_passing_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, non_passing_trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, individuals_in_transition).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, category_membership_is_relational_not_essential).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trans women whose presentation, voice, mannerisms, and social history are read as consistent with the category by most interlocutors in most settings. They gain access to gendered spaces, pronouns, and social treatment on the strength of continuous successful performance, but that access is never secured once and for all — it is re-earned in every new interaction and can be revoked by a single unconvinced observer.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, socially_recognized_passing_individuals, beneficiary,
    moderate, biographical, constrained, local).

% Trans women whose voice, height, bone structure, or transition timeline make sustained recognition difficult regardless of effort or sincerity of identity. They are excluded from the category not by any failure of self-identification but by an audience's refusal to extend recognition, and have no recourse against a standard that is applied by ad hoc social judgment rather than fixed criteria.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, non_passing_trans_women, payer,
    powerless, biographical, trapped, local).

% Cisgender women whose appearance, voice, or manner reads as insufficiently feminine face the same recognition-based scrutiny — being challenged, misgendered, or excluded from women's spaces because the performance standard used to admit trans women into the category is the same standard that polices all women's conformity to it. Butch women, tall women, women with androgynous features bear this cost even though their birth-sex membership is not in question under other readings.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    powerless, biographical, constrained, local).

% Early- or mid-transition individuals occupy a liminal zone where recognition is inconsistent by design — the same person may be read as their affirmed gender in one venue and denied it in the next. Their category membership fluctuates hour to hour based on lighting, clothing, and the mood of strangers, with no stable status to appeal to.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, individuals_in_transition, payer,
    powerless, biographical, trapped, local).

% Cisgender women (and women's-space administrators) who informally and formally set and enforce the performance/recognition threshold — in bathrooms, changing rooms, shelters, sports leagues, social circles — deciding case by case whether a given trans woman 'counts.' They benefit from retaining discretionary control over category boundaries and from a standard that lets them admit some trans women while excluding others without needing a formal rule, but they also carry real safety and comfort interests the standard is invoked to protect.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gatekeeping_incumbent_women, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, gatekeeping_incumbent_women, beneficiary).

% Institutions running sex-segregated leagues, shelters, and facilities must operationalize 'passing/recognition' into administrable criteria (hormone levels, years since transition, appearance panels), converting an informal social standard into formal gatekeeping apparatus that then gets challenged from both directions — as too permissive by some cis-women advocates and as arbitrarily exclusionary by trans advocates.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, womens_sports_and_space_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Hold that membership should track birth anatomy regardless of performance or recognition; they view the social-role standard as an unstable compromise that neither protects sex-based interests nor honors trans identity, and are not accommodated within this reading's framework at all — their objection is structural, not a matter of degree.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, biological_sex_reading_advocates, excluded,
    organized, generational, mobile, national).

% Hold that self-declared identity alone should ground membership, with no performance or recognition threshold. Under this reading they are excluded from setting terms: a sincere self-identification that fails to secure social recognition confers no standing here, which they regard as making membership hostage to others' perceptions rather than to the person's own claim.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_identity_reading_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, non-bureaucratic way for ordinary social contexts (bathrooms, changing rooms, social circles, sports) to sort category membership without requiring appeal to contested biological facts or unverifiable inner states — recognition by others is the only currency available in everyday face-to-face interaction, so the standard coordinates expectations using the information actually present in the room.
% TRANSFER_FUNCTION: Moves social standing, physical access, and safety-relevant trust from those who fail the recognition threshold (non-passing trans women, gender-nonconforming cis women, mid-transition individuals) to those positioned to judge it (incumbent women and institutional gatekeepers) and to those who successfully clear it (passing individuals) — access is granted or withheld interaction by interaction rather than fixed by rule.
% ABSENT_VOICES: Both sibling readings' advocates are structurally absent from this standard's operation: biological-sex advocates would object that performance can be counterfeited and shouldn't ground access to sex-segregated space at all; gender-identity advocates would object that the standard makes a person's own testimony about themselves subordinate to a stranger's snap judgment. Neither objection is processed within the social-role framework — both are treated as external critique.
% DISAPPEARANCE_RATIONALE: If sustained-performance/recognition ceased to function as the operative criterion overnight, passing trans women would lose their only current pathway to consistent recognition (a real loss), gender-nonconforming cis women would be freed from a policing mechanism that currently targets them too (a real gain), and institutions would have to adopt either a bright-line biological rule or a self-declaration rule — either of which redistributes who is included and who administers the boundary. Whether the world 'rearranges' or is 'unchanged' depends entirely on which replacement standard fills the vacuum, which is exactly the kernel contest this story is one reading of.
% FOUNDING_PROBLEM: Ordinary social spaces need a workable sorting mechanism in the absence of either verified biological data or verified inner identity — something observers can actually use in the moment of a bathroom door or a locker room, given that neither chromosomes nor subjective self-concept are legible on sight.
% FOUNDING_PROBLEM_CORROBORATION: Facility administrators and legal scholars outside any advocacy camp attest that face-to-face social sorting by observable performance is in fact how membership gets adjudicated in practice regardless of which formal rule a jurisdiction claims to follow — this is corroborated by sociological research on 'passing' and by litigation records showing informal recognition disputes precede and outlast formal policy changes. No party benefiting from discretionary gatekeeping power has an interest in surfacing how much power the standard grants them, so this corroboration is explicitly sought from disinterested facility-design and sociolegal sources rather than from gatekeepers themselves.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.38 at interval end) because the standard's costs are primarily performance costs and recognition-uncertainty costs rather than direct material extraction — there is no rent collected in the ordinary sense, but there is a real and rising cost borne by those who cannot secure recognition regardless of effort. Suppression is moderate (0.42) and rising: the standard's persistence depends on continued informal enforcement (challenges, exclusions, appearance-based scrutiny) rather than on formal coercive apparatus, and that informal enforcement has hardened somewhat as the underlying kernel contest has become more politically salient and institutions have felt pressure to formalize what was once purely informal judgment. Theater ratio (0.28) reflects that some of the apparatus — appearance panels, hormone-level thresholds used as recognition proxies — substitutes a legible administrative proxy for the actually operative social judgment, without eliminating the underlying discretionary standard.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent-gatekeeper seat, this looks like sensible, humane case-by-case judgment responsive to real safety and comfort interests — a rope. From the non-passing trans woman's seat, or the gender-nonconforming cis woman's seat, the identical mechanism looks like an unaccountable, ad hoc extraction of social standing administered by whoever happens to be present, with no criteria to contest — closer to a snare in local operation even though it carries a genuine coordination function at the aggregate level. This divergence is why the constraint is authored as tangled_rope: both the coordination function (a workable sorting standard for face-to-face contexts) and the asymmetric extraction (concentrated cost borne by those who fail an unaccountable, informally-enforced threshold) are structurally present simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Socially-recognized passing individuals sit near the beneficiary end of directionality because the standard, when it works for them, grants real access — but their exit options are only 'constrained,' not 'mobile' or 'arbitrage,' because that access is never secured and must be continuously re-earned. Non-passing trans women and individuals in transition sit near the full-target end: trapped exit options, no recourse against a standard with no fixed criteria to appeal to. Gender-nonconforming cis women are a distinctive feature of this specific reading: they are victims of a standard that was not built to exclude them but does so as a structural byproduct, which is why the victim structure is 'ambiguous' rather than clean — the same mechanism that includes some trans women also excludes some cis women, and the same mechanism that could exclude trans women protects cis women's presumptive category security only when their presentation is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — spaces need SOME sorting mechanism when neither biological data nor subjective identity is legible on sight — remains live; this is not a vestigial arrangement propped up by inertia. What prevents mislabeling this as pure extraction is the genuine absence of a superior alternative available to ordinary face-to-face interaction: a bathroom door does not have access to chromosome tests or a verified internal identity statement, so some sorting-by-observable-performance is doing real coordination work. What prevents mislabeling it as pure coordination is that the standard's discretionary, unappealable character concentrates real and rising costs on specific non-consenting parties (non-passing trans women, gender-nonconforming cis women) who have no path to contest an adverse judgment — this is the asymmetric extraction gate that keeps the classification at tangled_rope rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_threshold_administrability,
    'Can ''sustained social performance and recognition'' ever be converted into a stable, appealable administrative criterion, or is it irreducibly a matter of ad hoc social judgment that resists codification without collapsing into either the biological_sex_reading or the gender_identity_reading?',
    'Track institutional attempts to formalize the standard (sports-league appearance panels, hormone-threshold policies, shelter intake criteria) and observe whether they converge on stable, litigation-surviving criteria or continue to generate case-by-case disputes that reveal the underlying judgment as irreducibly informal.',
    'If the standard proves administrable, this reading moves toward scaffold (transitional formalization with an eventual stable rule) or rope (workable coordination with acceptable overhead); if it proves irreducibly informal, the tangled_rope classification is reinforced and the suppression/extraction trend lines should be expected to keep rising as more institutions attempt and fail at formalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_threshold_administrability, empirical, 'Whether the social-role standard can be stabilized into administrable criteria or remains inherently discretionary.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three sibling readings of gendered_category_membership disagree — is it about WHAT grounds membership (metaphysical disagreement about the nature of the category), or about WHO gets to adjudicate membership in practice (a disagreement about authority and process that could survive metaphysical agreement)?',
    'This is the committer-structure content required by Rule 2: examine whether a party could hold the social_role_reading''s practical adjudication process while accepting either sibling''s metaphysical grounding — e.g., someone who believes biological sex is the true ground of category membership might still accept that in practice, ordinary social spaces must sort by observable performance because biological verification is unavailable at the point of interaction. If so, the readings are not purely rival metaphysical claims but partially rival PROCESS claims layered over metaphysical disagreement.',
    'If the disagreement is primarily about process rather than metaphysics, the three readings could in principle share procedural common ground even while their formal grounding claims remain in tension — this would soften the forecloses/coexists_with distinction and suggest more of the kernel''s structure lives in cs_structure.axioms (process-level claims) than in the metaphysical headline claims each reading advertises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel''s three readings disagree about the metaphysics of the category or about who adjudicates it in practice.').

omega_variable(
    cis_woman_collateral_victim_status,
    'Is the inclusion of gender-nonconforming cis women in this reading''s victim set a genuine structural feature of the social_role_reading specifically, or an artifact that would appear under any recognition-based sorting mechanism regardless of which kernel reading nominally governs it?',
    'Compare enforcement patterns in jurisdictions/institutions that formally adopt the biological_sex_reading or gender_identity_reading as policy but where front-line social sorting still operates on observable performance in practice (per the founding_problem_corroboration) — if gender-nonconforming cis women are policed similarly under nominally different formal regimes, the collateral-victim structure is a feature of face-to-face recognition sorting as such, not of this reading specifically.',
    'If the collateral-victim pattern is universal to recognition-based sorting regardless of formal policy, this reading''s distinctive ''ambiguous victim structure'' claim weakens — the ambiguity would belong to the underlying sorting mechanism, not to the social_role_reading''s specific normative content, and the constraint family''s ε-invariance would need a shared upstream story capturing the sorting-mechanism-as-such.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_woman_collateral_victim_status, conceptual, 'Whether cis-woman collateral exclusion is specific to this reading or a general feature of recognition-based sorting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__social_role_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__social_role_reading, theater_ratio, 8, 0.23).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__social_role_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__social_role_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__social_role_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__social_role_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__social_role_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__social_role_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__social_role_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__social_role_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__social_role_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__social_role_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__social_role_reading, 0.1).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the gendered_category_membership kernel, decomposed per the epsilon-invariance principle because the three grounding claims (biological markers, self-declared identity, social performance/recognition) produce structurally distinct beneficiary/victim sets, distinct enforcement mechanisms, and distinct ε values that cannot be averaged or blended into a single constraint without violating epsilon-invariance. The social_role_reading is distinguished from its siblings by: (1) conditional rather than categorical inclusion of trans women (contingent on passing/recognition rather than fixed at birth or self-declared), (2) distributed rather than centralized gatekeeping (every social interaction is a potential adjudication point rather than a single formal criterion), and (3) an ambiguous, cross-cutting victim structure that includes gender-nonconforming cis women alongside non-passing trans individuals — a victim pattern neither sibling reading produces in the same shape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
