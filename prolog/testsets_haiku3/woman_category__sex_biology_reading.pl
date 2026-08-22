% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Woman Category: Sex/Biology Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the sex-biology reading of the
 *   contested 'woman' category. It defines category membership on the basis
 *   of chromosomal (XX), anatomical (female reproductive organs), and
 *   reproductive characteristics (capacity for pregnancy and female-pattern
 *   sexual development). The reading is operationalized in specific
 *   high-stakes domains: sex-segregated violence protections (shelters,
 *   restraining orders), sports eligibility (testosterone limits, chromosome
 *   verification), reproductive rights frameworks (contraception, abortion,
 *   maternal healthcare), and sex-based epidemiology (data collection on
 *   sexual violence, reproductive health). The core extractive claim is that
 *   this reading excludes people outside its boundary (transgender women,
 *   many intersex people) from protections and spaces that would be available
 *   under alternative readings (gender-identity or intersex-accommodating
 *   definitions). The reading is simultaneously presented as natural law
 *   (biological sex is a fixed fact) and as an engineered boundary
 *   (operationalized through policy, medical screening, and enforcement).
 *   This is precisely the false-summit candidate structure: beneficiaries
 *   within the sex-based-rights movement benefit from treating the reading as
 *   natural and inevitable; the constraint's persistence depends on
 *   suppressing alternative readings and enforcing bodily/medical disclosure
 *   from edge cases. The measured extraction is moderate-to-high (0.68 at
 *   interval end) because the reading redistributes recognition and access in
 *   ways that concentrate benefits for the beneficiary set and extract costs
 *   from the victim sets. Over the 1970-2026 interval, suppression has
 *   increased (from 0.25 to 0.45) as the reading faces mounting resistance
 *   from gender-identity and intersex advocates, requiring active policy
 *   enforcement to maintain the boundary.
 *
 * KEY AGENTS:
 *   - Female-biology people: those with XX chromosomes and female reproductive anatomy; receive sex-segregated protections and benefits from category membership stability.
 *   - Transgender women: excluded from this reading's category membership; experience the boundary as denial of recognition and exclusion from protections indexed to 'woman'.
 *   - Intersex people: ambiguously positioned; some fit the boundary cleanly, others are excluded or forced to disclose medical history to access protections.
 *   - Sex-based rights advocates: agenda-setters who operationalize and defend the reading through policy, litigation, and institutional maintenance.
 *   - Gender-identity advocates: excluded from authority but actively contesting the reading's legitimacy and operationalization.
 *   - Legislators and policy-makers: operationalize the reading in specific domains (sports, shelters, prisons, healthcare) and face pressure from multiple stakeholder camps.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.45).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Woman Category: Sex/Biology Reading").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, 'c7985c97-731b-4cc8-a632-e9ca717cc7bd').
narrative_ontology:cs_kernel_codification('c7985c97-731b-4cc8-a632-e9ca717cc7bd', distributed).
narrative_ontology:cs_authority_grounding('c7985c97-731b-4cc8-a632-e9ca717cc7bd', extraction).
narrative_ontology:cs_interpretation_layer_present('c7985c97-731b-4cc8-a632-e9ca717cc7bd').
narrative_ontology:cs_reading_relation('c7985c97-731b-4cc8-a632-e9ca717cc7bd', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7985c97-731b-4cc8-a632-e9ca717cc7bd', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('c7985c97-731b-4cc8-a632-e9ca717cc7bd', foundational, biological_sex_material_basis).
narrative_ontology:cs_axiom_status(biological_sex_material_basis, holdable).
narrative_ontology:cs_axiom_grounding('c7985c97-731b-4cc8-a632-e9ca717cc7bd', biological_sex_material_basis, empirically_contingent).
narrative_ontology:cs_axiom('c7985c97-731b-4cc8-a632-e9ca717cc7bd', foundational, reproductive_anatomy_predicts_vulnerability).
narrative_ontology:cs_axiom_status(reproductive_anatomy_predicts_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('c7985c97-731b-4cc8-a632-e9ca717cc7bd', reproductive_anatomy_predicts_vulnerability, empirically_contingent).
narrative_ontology:cs_reference_frame('c7985c97-731b-4cc8-a632-e9ca717cc7bd', biological_sex_materialist_framework).
narrative_ontology:cs_drift_state('c7985c97-731b-4cc8-a632-e9ca717cc7bd', contemporary_identity_political_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c7985c97-731b-4cc8-a632-e9ca717cc7bd', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_biology_people).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, sex_based_rights_advocates).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people_excluded).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, sex_based_data_collection_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, intersex_people_excluded).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, violence_research_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive sex-segregated protections (domestic violence shelters, women's prisons, sports categories) defined around biological female reproductive capacity and anatomy. These policies collect data, set boundaries, and allocate resources on the premise that biological sex is the relevant difference for understanding vulnerability to certain material harms (sexual violence, reproductive coercion, athletic performance advantage). Maintain this framing against competing identity-based definitions.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, female_biology_people, beneficiary,
    organized, generational, constrained, global).

% Excluded from many sex-segregated spaces, protections, and categories by this reading's boundary (which does not recognize gender identity as constitutive of category membership). They experience this exclusion as a denial of social recognition of their category membership and practical exclusion from protections and spaces intended for women. Their exit from the constraint is incomplete because the constraint defines access to social infrastructure.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women, payer,
    organized, biographical, constrained, global).

% Face ambiguous status under this reading: biological traits may be atypical (DSD, androgen insensitivity, etc.), and the reading's boundary (XX chromosomes + female anatomy) may not cleanly apply. Some intersex people are excluded from sex-segregated spaces and protections; others are ambiguously included. The constraint forces them to declare or be tested on biological characteristics as the condition of category membership, which many find invasive. They pay extraction through forced bodily/medical disclosure to access the protection.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people_excluded, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, intersex_people_excluded, beneficiary).

% Defend and enforce the sex-biology reading through policy work, litigation, legislative testimony, and institutional maintenance. They argue that biological sex is an irreducible axis of material harm (sexual violence, reproductive coercion, athletic performance advantage), that sex-segregated protections are necessary to address harms distributed by sex, and that gender identity is a distinct category that should not override sex-based boundary maintenance. They set the agenda for how the constraint is operationalized in specific domains (sports, shelters, prisons, data collection).
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sex_based_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Excluded from this constraint's framing and authority structure; they would argue that gender identity is constitutive of category membership, that transgender women are women and belong in women's spaces and protections, and that sex-biology exclusion causes material harm (denial of safety, social erasure, exclusion from care). They actively contest this reading's legitimacy and authority.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, global).

% Argue that biological sex is a spectrum (DSDs, reproductive variation), that the reading's boundary is both over-inclusive and under-inclusive in ways that force medical disclosure and create ambiguous membership for people whose biology does not cleanly fit binary categories, and that the reading reinforces medical gatekeeping and bodily non-autonomy for intersex people.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_advocates, excluded,
    moderate, generational, mobile, global).

% Draft and enforce laws and policies (sports regulations, shelter access rules, prison classification, healthcare protocols, data collection) that operationalize a sex-biology boundary. They face pressure from multiple stakeholders (sex-based advocates, gender identity advocates, intersex advocates) to define the category in different ways, and their choices determine who is admitted to or excluded from specific protections and spaces.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, legislators_policymakers, agenda_setter,
    institutional, generational, mobile, national).

% Operationalize the sex-biology reading in athletics policy, where the reading's premise (reproductive/anatomical performance advantage) is most contentious and empirically measurable. They mandate testosterone testing, chromosome verification, or reproductive capacity documentation as conditions of category membership for sex-segregated competition — extracting bodily disclosure from those competing in the category.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    organized, biographical, constrained, global).

% Argue that collecting data indexed to biological sex (not gender identity) is necessary to understand and address sexual violence and reproductive coercion, because these harms are materially distributed by reproductive capacity and anatomy. They use sex-based data collection to describe patterns (e.g., sexual assault perpetrated disproportionately by those with male reproductive anatomy against those with female reproductive anatomy) and design targeted protections.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, violence_research_advocates, beneficiary,
    organized, generational, mobile, global).

% Analyze the constraint from outside its enforcement logic, examining the trade-offs between using sex as a sorting criterion for protections (precision gains for majority cases, harms to edge cases like intersex and transgender people) versus alternative approaches (gender identity, self-identification, individualized assessment).
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, observer_bioethicists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, sex_based_rights_advocates).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a stable boundary for sex-segregated protections and data collection: domestic violence prevention (shelters, restraining orders), reproductive rights (contraception, abortion, maternal healthcare), sports fairness (single-sex competition with comparable physical baselines), and epidemiological research on sexual violence (data indexed to reproductive capacity to describe patterns of perpetration and victimization).
% TRANSFER_FUNCTION: Allocates social recognition, legal status, and access to sex-segregated spaces and protections on the basis of chromosomal and anatomical sex characteristics. Those who fit the reading's boundary (XX chromosomes, female reproductive anatomy) are allocated to the 'woman' category and gain access to the protections that category carries. Those outside the boundary (transgender women, many intersex people) are reallocated away from the category and, consequently, away from protections indexed to 'woman'.
% ABSENT_VOICES: Gender-identity advocates and many intersex advocates are structurally excluded from the authority structure that maintains this reading. They would testify that the sex-biology boundary causes material harm through denial of recognition, exclusion from protections, forced medical disclosure, and misalignment of protections to actual vulnerability (transgender women face sexual violence too; intersex people may not fit a binary boundary; gender identity is relevant to some vulnerabilities). These voices appear in litigation and legislative testimony but do not control policy in many jurisdictions where the sex-biology reading dominates.
% DISAPPEARANCE_RATIONALE: Sex-based advocates argue: if this reading disappeared, sex-segregated protections would become incoherent; data collection on sexual violence would lose the ability to describe perpetration and victimization patterns by reproductive sex; athletic fairness would require alternative mechanisms to separate bodies with different performance advantages. The world would not rearrange toward solution of sex-based harms; it would lose the tools for addressing them. Gender-identity advocates argue: if this reading disappeared and was replaced by gender-identity recognition, protections would either universalize (not sex-segregated) or would become more precise (individualized assessment of vulnerability rather than categorical sex). The world would rearrange toward more inclusive category membership and would not lose its ability to protect people from violence — it would just protect them based on actual vulnerability rather than assumed category.
% FOUNDING_PROBLEM: Biological reproductive capacity and anatomy create material vulnerabilities: sexual violence is perpetrated disproportionately by those with male anatomy against those with female anatomy; pregnancy-related medical risks are indexed to reproductive capacity; some athletic performance advantages are associated with typical male physiology. A stable category boundary indexed to these characteristics enables targeted protection of those at risk of these harms.
% FOUNDING_PROBLEM_CORROBORATION: Sex-based violence researchers and women's health researchers testify that reproductive-anatomy-indexed analysis reveals epidemiological patterns necessary for prevention: sexual assault is perpetrated at different rates by reproductive sex; maternal mortality is a sex-specific risk; testosterone-dependent performance advantages are documented in athletics. They argue these material facts justify the boundary. Gender-identity advocates and intersex scientists testify that the founding problem is more complex: transgender women face sexual violence at rates closer to cisgender women than to cisgender men; intersex people do not fit a binary boundary cleanly; the actual vulnerabilities matter more than reproductive category. Medical evidence on transgender health shows that vulnerability to some harms persists post-transition while others are altered. This corroboration is contested — no source outside the benefiting parties has produced a consensus judgment on whether the founding problem is solved, persists, or has been reframed.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, contested).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68) because the reading concentrates recognition and access around a specific biological definition while systematically excluding others from the same protections. The exclusion is not incidental but essential to the reading's logic: the boundary that defines who gets 'woman' protections also defines who does not. Suppression is moderate (0.45) because the reading must actively be enforced against rising resistance: sports bodies mandate chromosome testing, legislatures pass bathroom bills, healthcare systems set policies on how to classify transgender and intersex patients. The constraint does not emerge naturally or persist by consensus; it persists because institutional actors with power (legislatures, sports bodies, healthcare systems) make enforcing the boundary a priority. Theater is low-moderate (0.28) because the reading has a real functional core (biological reproductive capacity does predict some vulnerabilities to sexual violence; athletic performance advantage does correlate with sex-typical anatomy), but an increasing share of enforcement activity is theatrical: defenders of the reading spend more effort excluding edge cases and alternative definitions than addressing the founding problems the reading was meant to solve. Accessibility collapse is high (0.72) because once the boundary is understood (XX + female anatomy = woman, all others = not), alternatives are not immediately visible: the reading presents itself as natural law, making it hard to imagine the category drawn differently. Resistance is high (0.81) because the reading faces sustained, organized opposition from transgender advocates, intersex advocates, and gender-identity proponents who argue the boundary is both under-inclusive and over-inclusive and causes material harm. The measured resistance reflects the actual contention in legislatures, courts, athletic bodies, and medical institutions.
 *
 * PERSPECTIVAL GAP:
 *   From the sex-based-advocate seat, the reading is Rope (genuine coordination problem, stable boundary enables targeted help). From the transgender-women seat, it is Snare (the coordination story is cover; exclusion is the primary function; exit is suppressed). From the legislator seat, it is Tangled Rope (real coordination function, but increasingly asymmetric extraction from edge cases, requiring active enforcement). From the intersex seat, it is Tangled Rope with identity-lock (coordination for those who fit the boundary cleanly, extraction for those who are ambiguous). These divergent classifications from the same structural facts are exactly what the per-seat computation produces — and the claimed_type (Tangled Rope) reflects the analyst's judgment that the reading contains both a coordination function (addressing sex-based vulnerability) and asymmetric extraction (from those excluded by the boundary).
 *
 * DIRECTIONALITY LOGIC:
 *   Female-biology people are the structural beneficiaries: they receive sex-segregated protections, their category membership is recognized and stable, and they benefit from data collection and policy frameworks indexed to their position (d ≈ 0.1–0.3, low extraction). Sex-based-rights advocates are agenda-setters who maintain the boundary (d ≈ 0.2–0.4, moderate beneficiary position). Transgender women are victims of the reading: they are excluded from the 'woman' category and from many protections, and the constraint's persistence depends on suppressing their claims to membership (d ≈ 0.75–0.95, high extraction). Intersex people face ambiguous extraction depending on how cleanly their biology fits the boundary; many are forced to disclose medical history and face uncertainty about which protections apply to them (d ≈ 0.6–0.85, moderate-to-high extraction). Gender-identity advocates are excluded from authority but not passive: they actively resist and contest the reading, which raises their organizational power and lowers their formal exit (trapped in contention; d ≈ 0.5–0.7, symmetric-to-payer). Legislators face structural pressure from multiple seats and must enforce the boundary against resistance, which increases their suppression requirement (d ≈ 0.4–0.6, symmetric). The directionality derivation maps cleanly from beneficiary/victim declarations + power + exit: no overrides needed for this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is multi-part: (1) sexual violence is perpetrated disproportionately by those with male reproductive anatomy against those with female reproductive anatomy, (2) pregnancy-related medical risks are indexed to female reproductive capacity, (3) some athletic performance advantages are indexed to reproductive anatomy. This reading was operationalized to address these harms through targeted protections: domestic violence shelters, reproductive rights, sex-segregated sports. The mandatrophy question is whether the founding problem is still live or whether the reading has become a zombie extracting recognition and access from edge cases after the core problem is solved. The measurement supports the mandatrophy thesis: suppression has risen sharply (from 0.25 to 0.45) while theater has also risen (from 0.12 to 0.28), suggesting the reading is increasingly maintained through active enforcement rather than by solving the problem it was built for. Over the interval, sex-based violence research has become more sophisticated and increasingly indexes risk to specific factors (age, relationship to perpetrator, physical vulnerability) rather than to categorical sex, undermining the reading's assumption that sex is the right categorical variable for understanding and preventing violence. Sports policy has become more granular (testosterone limits, individualized assessment) rather than relying on categorical sex. This suggests the founding problem is being solved through more precise mechanisms, leaving the constraint to function as a category-enforcement mechanism rather than a problem-solver. Whether this is true mandatrophy (the founding problem has died but the constraint persists) or correct response (categorical sex is still necessary for some purposes, and the constraint serves to maintain clarity) is contested — sex-based advocates argue the founding problem is still live, gender-identity advocates argue it is effectively solved and the reading now extracts without coordinating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_definition_ambiguity,
    'Is the ''woman'' category a commitment-system kernel (a fixed text or inherited tradition that all readings interpret), or is it a contested concept where readings disagree about what the category even refers to?',
    'Examine whether all readings (sex-biology, gender-identity, intersex-accommodation) agree on a shared kernel text or founding premise, or whether they contest the kernel itself. If no shared kernel, readings are not interpretations of the same kernel but rather competing definitions of the category.',
    'If shared kernel: readings are coexistent interpretations of a stable referent, and the constraint-family structure is valid. If no shared kernel: the three ''readings'' are actually three distinct constraints that happen to target the same category label, and the framing as readings may be misleading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_definition_ambiguity, conceptual, 'Whether ''woman'' is a fixed kernel with multiple interpretations or a contested concept with no shared kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t1970, woman_category__sex_biology_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(woma_tr_t1990, woman_category__sex_biology_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(woma_tr_t2005, woman_category__sex_biology_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(woma_tr_t2015, woman_category__sex_biology_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(woma_tr_t2020, woman_category__sex_biology_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(woma_tr_t2026, woman_category__sex_biology_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t1970, woman_category__sex_biology_reading, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(woma_be_t1990, woman_category__sex_biology_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(woma_be_t2005, woman_category__sex_biology_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(woma_be_t2015, woman_category__sex_biology_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(woma_be_t2020, woman_category__sex_biology_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(woma_be_t2026, woman_category__sex_biology_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t1970, woman_category__sex_biology_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(woma_su_t1990, woman_category__sex_biology_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(woma_su_t2005, woman_category__sex_biology_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(woma_su_t2015, woman_category__sex_biology_reading, suppression_requirement, 2015, 0.43).
narrative_ontology:measurement(woma_su_t2020, woman_category__sex_biology_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(woma_su_t2026, woman_category__sex_biology_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% The 'woman' category is a contested kernel with multiple readings. This story instantiates the sex-biology reading (category membership determined by XX chromosomes and female reproductive anatomy). Sibling readings (gender-identity and intersex-accommodation) are separate constraint stories, each with their own ε, beneficiary/victim structure, and type. All three readings compete for institutional authority in the same domains (sports, healthcare, violence prevention, legal protection). They are linked as a constraint family; the family's internal structure is that each reading influences the others' institutional operationalization and legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
