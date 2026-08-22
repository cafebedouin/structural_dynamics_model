% ============================================================================
% CONSTRAINT STORY: woman_category__intersex_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__intersex_accommodation_reading, []).

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
 *   constraint_id: woman_category__intersex_accommodation_reading
 *   human_readable: Woman Category: Intersex Accommodation Reading
 *   domain: political_philosophy/law/social_policy/bioethics
 *
 * SUMMARY:
 *   The woman category is contested across three structurally distinct
 *   readings of a single kernel: sex_biology_reading
 *   (chromosomal/anatomical/reproductive criteria), gender_identity_reading
 *   (internal gender identity), and intersex_accommodation_reading (this one
 *   — biological sex as spectrum, including intersex variation). This story
 *   instantiates ONLY the intersex-accommodation reading as a clean,
 *   ε-invariant constraint. It is not a summary of the contest or a
 *   compromise between readings. The reading centers acknowledgment that
 *   human biological sex does not reduce to a clean binary and that people
 *   with intersex conditions represent legitimate, natural human variation
 *   that should be accommodated within the woman category without requiring
 *   conformity to typical female anatomy. The reading benefits intersex
 *   people and medical/legal advocates for spectrum recognition; it imposes
 *   boundary-policing and performance-scrutiny costs on elite female athletes
 *   with androgen variation and on intersex people in contexts where
 *   enforcement is strong (sports). Extractiveness is moderate-to-high (0.62
 *   at interval end) because enforcement of the spectrum boundary requires
 *   continuous institutional work and medical scrutiny, which falls heaviest
 *   on the athletes and intersex people the reading nominally includes.
 *   Suppression is substantial (0.71) because the reading must actively
 *   suppress resistance from sex-biology and gender-identity adherents to
 *   maintain its definition; the category boundary is contested and requires
 *   ongoing policing. Theater ratio is low-to-moderate (0.28) because much of
 *   the boundary work is functional (real medical review), but performative
 *   elements increase as enforcement expands (documented Semenya case
 *   pattern).
 *
 * KEY AGENTS:
 *   - intersex_people_recognizing_as_women — powerless, identity-locked beneficiaries whose category membership the reading includes but whose status remains contingent on boundary policing
 *   - elite_female_athletes_with_androgen_sensitivity — powerful athletes bearing the cost of performance-based boundary enforcement (hormone suppression protocols, eligibility scrutiny)
 *   - medical_and_legal_advocates — organized, mobile beneficiaries advancing spectrum recognition as institutional doctrine
 *   - intersex_people_excluded_by_binary_enforcement — powerless, identity-locked payers subject to surveillance and eligibility determination
 *   - international_sports_governance — institutional agenda-setter bearing enforcement burden
 *   - sex_biology_reading_adherents — excluded, constrained resisters holding binary-category model
 *   - gender_identity_reading_adherents — excluded, constrained resisters viewing the reading as inadequate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, 0.62).
domain_priors:suppression_score(woman_category__intersex_accommodation_reading, 0.71).
domain_priors:theater_ratio(woman_category__intersex_accommodation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(woman_category__intersex_accommodation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__intersex_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__intersex_accommodation_reading, "Woman Category: Intersex Accommodation Reading").
narrative_ontology:topic_domain(woman_category__intersex_accommodation_reading, "political_philosophy/law/social_policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__intersex_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__intersex_accommodation_reading, '7af010e1-4834-4857-aebe-a8346a5c437f').
narrative_ontology:cs_kernel_codification('7af010e1-4834-4857-aebe-a8346a5c437f', distributed).
narrative_ontology:cs_authority_grounding('7af010e1-4834-4857-aebe-a8346a5c437f', distributed).
narrative_ontology:cs_reading_relation('7af010e1-4834-4857-aebe-a8346a5c437f', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('7af010e1-4834-4857-aebe-a8346a5c437f', woman_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('7af010e1-4834-4857-aebe-a8346a5c437f', foundational, biological_sex_is_spectrum_not_binary).
narrative_ontology:cs_axiom_status(biological_sex_is_spectrum_not_binary, holdable).
narrative_ontology:cs_axiom_grounding('7af010e1-4834-4857-aebe-a8346a5c437f', biological_sex_is_spectrum_not_binary, empirically_contingent).
narrative_ontology:cs_axiom('7af010e1-4834-4857-aebe-a8346a5c437f', foundational, intersex_variation_is_natural_not_pathological).
narrative_ontology:cs_axiom_status(intersex_variation_is_natural_not_pathological, holdable).
narrative_ontology:cs_axiom_grounding('7af010e1-4834-4857-aebe-a8346a5c437f', intersex_variation_is_natural_not_pathological, deontological).
narrative_ontology:cs_reference_frame('7af010e1-4834-4857-aebe-a8346a5c437f', binary_sex_category_system).
narrative_ontology:cs_drift_state('7af010e1-4834-4857-aebe-a8346a5c437f', post_intersex_medicalization_awareness_2010_2026, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('7af010e1-4834-4857-aebe-a8346a5c437f', '').
narrative_ontology:cs_kernel_id(woman_category__intersex_accommodation_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, intersex_people_recognizing_as_women).
narrative_ontology:constraint_beneficiary(woman_category__intersex_accommodation_reading, medical_and_legal_advocates_for_sex_spectrum_recognition).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, elite_female_athletes_with_androgen_sensitivity).
narrative_ontology:constraint_victim(woman_category__intersex_accommodation_reading, intersex_people_excluded_by_binary_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% People with intersex conditions (androgen insensitivity syndrome, Müllerian agenesis, mixed gonadal dysgenesis, 5-alpha reductase deficiency, and others) who identify as and live as women. The intersex-accommodation reading includes them in the woman category without requiring proof of chromosomal or reproductive conformity. This reading benefits them by acknowledging their lived identity and biological reality as legitimate members of the category without forcing them to pretend to be chromosomally typical. Exit would mean abandoning their self-conception and social participation as women, or reverting to the sex-biology reading and losing legal recognition.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_people_recognizing_as_women, beneficiary,
    powerless, biographical, identity_locked, global).

% Athletes like Caster Semenya, Dutee Chand, and others who were assigned female at birth, raised as girls, have female-typical anatomy and social identity, but carry variation in androgen processing, production, or sensitivity that may confer competitive advantage in endurance sports. Under the intersex-accommodation reading, they are recognized as women but subject to performance-based boundary enforcement (testosterone suppression protocols, eligibility review) that the binary sex-biology reading does not impose on typical-female athletes. They bear the cost of boundary policing through medical intervention while competitors without androgen variation do not. Their exit options are limited: changing sports (career-ending for elite athletes), accepting hormone suppression (with health and performance consequences), or litigating (expensive, uncertain). They cannot exit their biology or the reading's application to them.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, elite_female_athletes_with_androgen_sensitivity, payer,
    powerful, biographical, constrained, global).

% Medical societies (endocrinology, reproductive health), human rights organizations (UN bodies, Amnesty International), and legal scholars advocating for recognition of intersex variation as a legitimate, natural, non-pathological form of human biological difference. They benefit from institutional adoption of the intersex-accommodation reading because it advances their core mission of destigmatizing intersex conditions, generates professional legitimacy, funding for intersex research and care, and policy influence at international and national levels. Their mobility is high: if this reading is rejected, they redirect their advocacy to other policy domains (healthcare access, antidiscrimination law, freedom from non-consensual medical intervention). They incur small costs—institutional pushback, resistance from sex-biology adherents—but these are manageable within their organizational scope.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, medical_and_legal_advocates_for_sex_spectrum_recognition, beneficiary,
    organized, generational, mobile, global).

% Intersex people who do not fit neatly into either binary category or who have conditions making category assignment ambiguous under the sex-biology reading. The intersex-accommodation reading was designed to include them and reduce coercion to choose, but enforcement of that inclusion—especially in elite sports contexts and in high-scrutiny legal/medical settings—requires continuous documentation, medical evaluation, and eligibility determination. They bear the cost of being subjects of surveillance, medical attention, and institutional categorization as the price of inclusion. Their exit is impossible: they cannot exit their biology, their need for legal recognition, or the institutional machinery that now follows them.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, intersex_people_excluded_by_binary_enforcement, payer,
    powerless, biographical, identity_locked, global).

% Organizations like the International Olympic Committee, World Athletics, and other international sports federations set the rules for sex category eligibility in competition. The intersex-accommodation reading requires them to enforce a spectrum-based boundary that is more difficult to define and police than binary biology. They must decide: which androgen levels qualify? Which anatomical features? Which chromosomal variations? Once they adopt the reading, they are locked into administering it, which involves conducting or reviewing medical tests, setting hormone limits, and making eligibility determinations on a case-by-case basis. This administrative burden falls on them; the reading benefits people it includes but imposes machinery costs and liability risk on the governance body.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, international_sports_governance, agenda_setter,
    institutional, generational, mobile, global).

% Legal systems, conservative bioethicists, some athletes, and advocacy organizations that hold the woman category should rest on chromosomal (XX), anatomical (female reproductive system), and reproductive (functional capacity) criteria. They are excluded from setting the agenda under the intersex-accommodation reading but mount substantial intellectual, legal, and political resistance to it. They cannot easily exit: they must engage with the reading in any jurisdiction that adopts it, and they lose standing to define the category in those jurisdictions. They argue the reading collapses meaningful boundaries and creates problems in sports and single-sex spaces. This is the most organized resistance base.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, sex_biology_reading_adherents, excluded,
    organized, generational, constrained, global).

% Legal systems, many human rights organizations, and many transgender people who hold the woman category should rest on gender identity: a person who identifies as a woman is a woman, regardless of assigned sex at birth or biological characteristics. They view the intersex-accommodation reading as centering the wrong factor (biology rather than identity) and potentially excluding transgender women who do not have female-typical biology. They are excluded from setting the agenda under this reading but contest its adequacy and theoretical grounding. They argue the reading perpetuates the idea that womanhood is grounded in biological difference rather than in identity and lived experience. They cannot exit: like sex-biology adherents, they must engage with the reading where it is adopted.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, gender_identity_reading_adherents, excluded,
    organized, generational, constrained, global).

% Medical licensing boards, civil rights enforcement agencies, employment regulators, and social service administrations must implement the woman category in healthcare (eligibility for gynecological care, reproductive services), in benefits (spousal benefits, maternity leave eligibility), and in antidiscrimination protection. They observe the reading contest and face contradictory pressures: pressure from gender-identity advocates to not require biological documentation; pressure from sex-biology advocates to enforce chromosomal/reproductive criteria; pressure from intersex advocates and medical professionals to accommodate variation. Their analytical role gives them influence over implementation details but no clear authority to resolve the fundamental contest. They must manage the ambiguity case-by-case and often lack clear guidance.
narrative_ontology:constraint_stakeholder(woman_category__intersex_accommodation_reading, regulatory_compliance_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the institutional problem of defining the woman category in a way that acknowledges natural human biological variation (intersex conditions) rather than enforcing a binary reproductive taxonomy. Enables legal recognition, social inclusion, and equitable access to healthcare and civil protections for people whose biology does not fit neatly into typical male or typical female categories. Coordinates between medical science (which documents intersex variation as natural and common), law (which must define eligibility for protections and benefits), and lived experience (people existing as women with non-binary bodies).
% TRANSFER_FUNCTION: Transfers membership and the rights/protections attached to it from a binary biological model to a spectrum-inclusive model. In medical contexts, transfers authority over category definition from reproductive endocrinologists (who focus on fertility) to gender-medicine and intersex-care specialists (who center lived identity and wellbeing). In legal contexts, transfers protection from explicit exclusion (intersex people formally denied woman status) to conditional inclusion (recognized as women but subject to verification processes). In elite sports, transfers the definition of fair competition from binary sex categories to spectrum-based hormone limits and case-by-case determination. The most consequential transfer: shifts the burden of boundary maintenance from intersex people themselves (who had to choose, hide, or fight for recognition) to institutional machinery (medical review, eligibility determination, documentation protocols).
% ABSENT_VOICES: Transgender women are formally excluded from this reading's focus (the reading centers biological variation, not identity). Gender-identity advocates argue the reading is incomplete and leaves them out or subordinates their concerns. Sex-biology adherents are excluded from agenda-setting and mount resistance from institutional positions they still control (some sports federations, some countries' legal systems). Intersex people in non-athletic contexts (where the reading's enforcement burden is low) would likely support it; elite athletes with androgen variation would likely oppose the performance-boundary consequences even though the reading nominally includes them. The most dangerously absent voice: intersex people in low-income countries without access to medical expertise—they are included in the reading's category but completely absent from its governance and enforcement mechanisms, which presume institutional capacity.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and pure sex-biology reading replaced it everywhere, intersex people currently living and legally recognized as women would lose category membership in many jurisdictions. Healthcare systems would reclassify patients; legal systems would deny antidiscrimination protection and benefits eligibility to intersex people previously covered; social institutions would revert to binary enforcement. The institutional scaffolding constructed to implement the reading (intersex-inclusive medical protocols, non-binary category recognition in law, administrative procedures for athletes with androgen variation) would collapse. People would not disappear or cease to exist—intersex people would continue to exist—but their legal status, institutional recognition, and protection would dissolve or be reclassified.
% FOUNDING_PROBLEM: Medical science documented natural intersex variation (affecting an estimated 1–2% of the population); existing law and policy enforced binary sex categories (male/female) without accommodation for people whose biology, anatomy, or physiology did not fit the binary. Intersex people were forced to choose a binary category that did not match their biology, hide their conditions, or face legal/institutional exclusion. Medical gatekeeping required intersex people to undergo normalization surgeries to fit the binary. The problem: a category system (woman) grounded in binary reproductive anatomy excluded or pathologized natural human variation.
% FOUNDING_PROBLEM_CORROBORATION: Medical endocrinologists (American Endocrine Society, European Society of Human Reproduction and Embryology) and human rights organizations (UN Office of the High Commissioner, Amnesty International) attest the founding problem is live and serious—intersex people continue to face category exclusion, medical coercion, and institutional ambiguity. Medical anthropologists and bioethicists document ongoing intersex experiences of non-recognition and coercion. Sex-biology reading adherents and some conservative bioethicists contest whether the problem warrants category-boundary revision: they argue intersex people represent rare edge cases that should be handled through exception procedures rather than category redefinition. Gender-identity advocates attest the founding problem is incompletely solved because the reading still centers biology rather than identity as the category grounding, leaving gender-nonconforming people and transgender women outside its scope. Elite sports governance acknowledges the boundary-definition problem but disputes whether the intersex-accommodation reading solves it (many argue it creates new boundary problems in performance contexts). The reading is corroborated by medical science and human rights documentation, but contested on theoretical grounds by both other readings.
narrative_ontology:disappearance_verdict(woman_category__intersex_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__intersex_accommodation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__intersex_accommodation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_category__intersex_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__intersex_accommodation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__intersex_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__intersex_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__intersex_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.62 over the interval as the reading diffuses from medical/academic contexts into sports regulation and civil law. Early institutional adoption (t=0–8) focuses on medical recognition and antidiscrimination protection, carrying low extraction. By t=16–25, the reading encounters elite sports contexts where boundary definition becomes high-stakes (performance advantage, fairness claims); enforcement machinery (testosterone limits, eligibility procedures) intensifies and lands most heavily on intersex athletes and intersex people seeking category confirmation. Plateau at t=35+ reflects stabilization: the reading is now established in law and medicine but remains contested in elite sports; further extraction is constrained by the magnitude of resistance and by legal/medical profession limits on how much surveillance the reading's institutional beneficiaries can impose without triggering rights-based pushback. Suppression follows the same trajectory because the reading must continuously suppress resistance from both other readings (sex-biology and gender-identity) to maintain its definition. Theater ratio stays low because much of the work is substantive (medical review, legal redefinition) rather than performative, but increases modestly as enforcement machinery becomes more visible and contested (t=25+). Accessibility collapse at 0.48 reflects that the spectrum-based boundary is actually harder to collapse—it is more open, more permeable, more accepting of variation—than binary alternatives, which is why it faces such organized resistance. Resistance at 0.72 is high and sustained because all three readings contest it: sex-biology adherents see it as category collapse, gender-identity adherents see it as inadequate, and elite-sports bodies see it as creating new boundary problems. The reading is extractive not because it harms anyone directly but because it concentrates enforcement burden and medical scrutiny on the very people it nominally benefits (intersex people) while benefiting advocates and institutions whose stake is political legitimacy rather than participant welfare.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap emerges from the split in the intersex beneficiary population. Intersex people in medical contexts (receiving care under spectrum-based diagnosis) benefit clearly from the reading with low cost. Intersex people in elite sports (competing under the reading but subject to hormone suppression and eligibility review) experience the reading as extractive enforcement rather than beneficial inclusion. The reading's architects and advocates (organized, mobile) see it as inclusion and recognition; the elite athletes subject to performance scrutiny see it as a new form of control. This is not a disagreement about facts—all parties can agree on biological variation—but a fundamental disagreement about whether spectrum-based category definition in high-stakes contexts serves the people it nominally includes. The engine's per-seat classification should reflect this: beneficiary and observer seats should compute the reading as coordinating rope; payer seats (especially in elite sports) should compute it as extractive snare or tangled_rope with high χ for constrained, identity-locked agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (medical/legal advocates): high exit mobility, organized power, professional benefit from institutional adoption. d ≈ 0.2. Victims in sports (elite athletes with androgen variation): powerful but constrained to their sport, identity-locked in gender identity, bear direct enforcement cost (hormone suppression, eligibility review). d ≈ 0.78. Victims in documentation/surveillance (intersex people subject to ongoing category determination): powerless, identity-locked (cannot exit their biology or their need for legal recognition), bear surveillance and scrutiny cost. d ≈ 0.82. Intersex beneficiaries (those who gain legal recognition without high enforcement burden): moderate power through organized advocacy, identity-locked (cannot exit being intersex or woman-identifying), benefit from the reading but gain little rent—mixed position. d ≈ 0.35. Excluded resisters (sex-biology and gender-identity adherents): organized, moderately powerful, constrained by law where the reading is adopted, face pressure to accommodate it. d ≈ 0.58 (displaced but not targeted; opposition is coordinate, not individual).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intersex people lack legal recognition and face binary exclusion) was live at t=0 and remains contested at t=50. The reading's mandate is to include intersex variation in the woman category to solve that problem. However, the manner of solving it—through spectrum-based boundary definition in contexts like elite sports—has introduced new institutional problems: continuous policing, surveillance, and medical scrutiny of intersex people. The Semenya case exemplifies this: the reading nominally includes her (she is a woman with intersex condition), but enforcement of the spectrum boundary requires hormone suppression and eligibility review, which many parties (Semenya herself, human rights advocates, gender-identity reading adherents) see as violating rather than protecting her. This is mandatrophy in the functional sense: the mandate (include intersex variation) has outlived its connection to the actual benefit (providing recognition without surveillance). At t=50, the reading persists because institutional machinery has been built around it and advocacy organizations have invested in it; but the actual function—benefiting intersex people without imposing new forms of control—has degraded. The measurement series capture this: extractiveness rises even though the founding problem remains contested (because the reading solves it through increasing institutional burden, not through genuine accommodation). Theater ratio stays low (the machinery is real, not purely performative) but the gap between the reading's stated function (include) and actual operation (police) widens. A true tangled_rope would have the theater ratio and extractiveness aligned with the coordination function; here, they diverge slightly, signaling that the coordination benefit is real but the extraction mechanism (boundary policing) is growing faster than the benefit distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spectrum_boundary_definition_contested,
    'Where exactly on the biological sex spectrum does the woman category boundary lie, and who has legitimate authority to define it?',
    'Comparative analysis of how different legal systems, medical bodies, and sports organizations define the boundary (which androgen levels, which chromosomal variations, which anatomical features count as female-spectrum); post-adoption empirical study of how intersex people experience the boundary definition in practice.',
    'If the boundary is inherently contestable and cannot be drawn at any single point without excluding some people the reading nominally includes, the reading collapses into a de facto administrative snare (every person gets individualized determination, no clear rule). If a stable boundary can be drawn with intersex people''s consent, the reading stabilizes as tangled_rope. If intersex people experience any boundary definition as exclusionary, the reading is revealed as extractive regardless of stated intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spectrum_boundary_definition_contested, conceptual, 'Whether a spectrum-based category boundary can be drawn and defended without internal contradiction.').

omega_variable(
    performance_advantage_separability,
    'Can the reading''s medical/legal inclusion function be separated from its sports-performance boundary function, or are they structurally entangled?',
    'Jurisdictional separation experiment: some legal systems adopt the reading for civil rights and medical recognition but use sex-biology criteria for elite sports; compare outcomes for intersex people''s legal inclusion and elite athlete participation across systems.',
    'If the functions can be separated without loss to either, the reading''s extraction is compartmentalizable and remediable (change sports rules without retracting the reading''s inclusion function). If entangled, the reading is fundamentally compromised: including intersex people legally requires mechanisms that, in sports contexts, function as eligibility gates and surveillance. This would mean the reading cannot achieve its mandate (genuine inclusion without conditions) in all domains simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_advantage_separability, empirical, 'Whether the reading''s inclusion and enforcement functions can be decoupled across policy domains.').

omega_variable(
    agenda_setter_capture_by_medical_episteme,
    'Is the reading''s institutional persistence driven by its genuine benefit to intersex people, or by the medical and legal professions'' interest in maintaining a spectrum-based category that requires their expertise and authority?',
    'Stakeholder surveys and deliberative processes with intersex people themselves (not their advocates) on whether they prefer the intersex-accommodation reading or alternatives; analysis of professional societies'' investment in spectrum-based frameworks vs. their public rhetoric.',
    'If intersex people would prefer simpler legal recognition without medicalized boundary definition, and medical societies continue promoting spectrum-based approaches, the reading is revealed as a false summit: it benefits the professional beneficiaries more than the nominal intersex beneficiaries. Extractiveness would be re-measured as higher, and classified as snare rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agenda_setter_capture_by_medical_episteme, empirical, 'Whether the reading''s persistence serves intersex people''s stated preferences or professional beneficiaries'' institutional interests.').

omega_variable(
    committer_contest_over_kernel_grounding,
    'Which reading (sex_biology, gender_identity, or intersex_accommodation) is the legitimate interpretation of what makes someone a woman, and can a single framework hold multiple readings without contradiction?',
    'This is a committer-axis question, not an observational one. It reflects the irreducible disagreement about whether the woman category should ground in chromosomes, identity, or biological variation. Resolution depends on which normative framework one adopts (medical, political, philosophical), not on empirical discovery.',
    'If no single framework can hold all three readings coherently, then institutional context matters absolutely: medical contexts use intersex-accommodation, legal contexts use gender-identity, sports contexts revert to sex-biology. The reading is locally valid but not universally grounded. If one reading does achieve universal grounding (e.g., through constitutional amendment or international treaty), the others are foreclosed—but this is political, not analytical resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_contest_over_kernel_grounding, conceptual, 'The fundamental committer disagreement about category grounding, unresolvable by empirical means alone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__intersex_accommodation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__intersex_accommodation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(woma_tr_t8, woman_category__intersex_accommodation_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(woma_tr_t16, woman_category__intersex_accommodation_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(woma_tr_t25, woman_category__intersex_accommodation_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(woma_tr_t35, woman_category__intersex_accommodation_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(woma_tr_t50, woman_category__intersex_accommodation_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__intersex_accommodation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(woma_be_t8, woman_category__intersex_accommodation_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(woma_be_t16, woman_category__intersex_accommodation_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(woma_be_t25, woman_category__intersex_accommodation_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(woma_be_t35, woman_category__intersex_accommodation_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement(woma_be_t50, woman_category__intersex_accommodation_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__intersex_accommodation_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(woma_su_t8, woman_category__intersex_accommodation_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(woma_su_t16, woman_category__intersex_accommodation_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(woma_su_t25, woman_category__intersex_accommodation_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(woma_su_t35, woman_category__intersex_accommodation_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(woma_su_t50, woman_category__intersex_accommodation_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__intersex_accommodation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_category__intersex_accommodation_reading, 0.12).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__intersex_accommodation_reading, woman_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% The woman_category kernel decomposes into three structurally distinct constraints, one per reading. Each reading instantiates a different ε, beneficiary/victim structure, and enforcement mechanism. All three are linked: sex_biology_reading is the baseline binary reading; gender_identity_reading centers identity rather than biology; intersex_accommodation_reading (this one) centers biological variation. The readings coexist in different institutional contexts and influence each other's enforcement: adoption of the intersex-accommodation reading in sports creates pressure on the gender_identity_reading to clarify its boundary (does it include intersex people who don't identify as women?), which creates pressure on the sex_biology_reading to clarify its scope. See docs/constraint_families/woman_category_kernel.md for the full family structure and cross-reading analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(woman_category__intersex_accommodation_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
