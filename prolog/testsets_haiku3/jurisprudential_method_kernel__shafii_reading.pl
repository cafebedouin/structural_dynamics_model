% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: al-Shafi'i's Four-Tier Jurisprudential Hierarchy
 *   domain: legal/religious/institutional
 *
 * SUMMARY:
 *   Al-Shafi'i (d. 204 AH / 820 CE) published a systematic methodology for
 *   Islamic jurisprudence organized as a strict four-tier hierarchy: Qur'an
 *   at the apex, then Hadith (with rigorous transmission authentication as
 *   the arbiter of authenticity), then Ijma (consensus of scholars), then
 *   Qiyas (analogical reasoning) as the last resort. This methodological
 *   standardization was presented as a solution to inconsistencies between
 *   earlier schools of jurisprudence (Abu Hanifah's Hanafi school, Malik ibn
 *   Anas's Maliki tradition, Ahmad ibn Hanbal's emerging Hanbali school) that
 *   operated with different implicit methodologies. The Shafi'i reading
 *   instantiates one authoritative reading of how Islamic law should be
 *   sourced and reasoned. It is one of four competing readings of the same
 *   jurisprudential kernel — the others being the Hanafi reading (which
 *   privileges qiyas and juristic reasoning), the Maliki reading (which
 *   grounds law in Medinan community practice), and the Hanbali reading
 *   (which restricts law to literal text and Companion opinions). This story
 *   describes only the Shafi'i reading's structure, beneficiaries, and
 *   extraction mechanisms.
 *
 * KEY AGENTS:
 *   - hadith_scholars: institutional beneficiaries of the hierarchy; their expertise in transmission chains becomes the definitive gate for legal authenticity
 *   - methodological_standardization_proponents: institutional beneficiaries; gain coherence and legitimacy through a published, explicit system
 *   - customary_practice_schools: powerful victims; their epistemically-grounded regional authority is demoted beneath hadith authentication
 *   - qiyas_centered_jurists: powerful victims; analogical reasoning is placed at the bottom of the hierarchy, constraining their methodological autonomy
 *   - regional_legal_traditions: organized victims; lose their distinctive voice to a uniform, empire-wide methodological framework
 *   - institutional_regulators: agenda-setters; benefit from a published system that can be cited as authoritative and appear neutral
 *   - competing_methodological_schools: observers and implicit contestants; must decide whether to adopt, resist, or selectively incorporate the hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.71).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.77).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "al-Shafi'i's Four-Tier Jurisprudential Hierarchy").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "legal/religious/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, '8e54f7e0-0325-40a1-8197-6e35cfdc86c6').
narrative_ontology:cs_kernel_codification('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', fixed_text).
narrative_ontology:cs_authority_grounding('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', lineage).
narrative_ontology:cs_interpretation_layer_present('8e54f7e0-0325-40a1-8197-6e35cfdc86c6').
narrative_ontology:cs_reading_relation('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', foundational, hadith_transmission_is_epistemic_arbiter).
narrative_ontology:cs_axiom_status(hadith_transmission_is_epistemic_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', hadith_transmission_is_epistemic_arbiter, deontological).
narrative_ontology:cs_axiom('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', foundational, methodological_hierarchy_resolves_school_inconsistency).
narrative_ontology:cs_axiom_status(methodological_hierarchy_resolves_school_inconsistency, holdable).
narrative_ontology:cs_axiom_grounding('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', methodological_hierarchy_resolves_school_inconsistency, instrumental).
narrative_ontology:cs_reference_frame('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', explicit_four_tier_hierarchy_with_hadith_authority).
narrative_ontology:cs_drift_state('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', post_hanafi_institutional_competition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e54f7e0-0325-40a1-8197-6e35cfdc86c6', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, methodological_standardization_proponents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_schools).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, qiyas_centered_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, regional_legal_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, transmission_skeptics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hadith transmission becomes the definitive arbiter of legal authenticity under Shafi'i's hierarchy. Scholars with expertise in isnad (transmission chains) and hadith authentication gain institutional authority to determine law's foundational claims. Their career paths and scholarly prestige depend on mastering hadith methodology; the standardization makes their training essential to legitimate jurisprudence across all schools that adopt the framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__shafii_reading, hadith_scholars, agenda_setter).

% Jurists and institutional authorities seeking coherence across regional schools benefit from a published, explicit methodology that claims to resolve contradictions between schools through systematic hierarchy rather than power struggles. The standardization provides institutional legitimacy and reduces jurisdictional conflicts by offering a transparent rulebook for adjudicating disputes between schools.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, methodological_standardization_proponents, beneficiary,
    institutional, generational, mobile, global).

% Schools grounding law in community practice (especially 'amal ahl al-Madina in the Maliki tradition) find their epistemic source demoted beneath hadith in the hierarchy. Their legal conclusions derived from established customary practice are now subject to challenge by hadith-authenticated rulings, even where community consensus had long settled the question. Their regional authority is structurally undermined.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_schools, payer,
    powerful, generational, constrained, regional).

% Jurists (especially in the Hanafi tradition) who treat qiyas (analogical reasoning) as a primary tool for extending divine intent to novel cases find their methodological autonomy constrained. The hierarchy places qiyas at the bottom, relegating it to a last resort only after Qur'an, hadith, and ijma are exhausted. Their sophisticated reasoning frameworks are repositioned as secondary to transmission authentication.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, qiyas_centered_jurists, payer,
    powerful, generational, constrained, regional).

% Established schools with deep roots in particular regions (Egypt, Khorasan, Medina) built their authority on local interpretive communities and evolved jurisprudence. The standardized hierarchy imposes a uniform methodological framework that breaks their historical autonomy. Regional schools must either adopt the hierarchy and lose their distinctive voice, or resist and lose institutional legitimacy in a broadening empire that privileges the standardized method.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, regional_legal_traditions, payer,
    organized, generational, constrained, regional).

% Early jurists whose methodologies preceded Shafi'i (Abu Hanifah, Malik, Ahmad ibn Hanbal) are reinterpreted through the hierarchy's lens, their actual practices oversimplified into neat tiers. They cannot defend their own methodological intentions against posthumous systematization. Their schools become classified as deviations from the standard rather than as legitimate alternatives.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, early_jurists_of_varied_schools, excluded,
    institutional, biographical, trapped, regional).

% Jurists who harbored doubts about particular hadith chains or who relied more on rationally-grounded or community-sourced authority find themselves unable to challenge hadith-based rulings within the standardized framework. The hierarchy's emphasis on transmission authentication restricts their ability to propose rational or practice-based alternatives. Their skepticism becomes heretical rather than legitimate jurisprudential difference.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, transmission_skeptics, payer,
    moderate, biographical, constrained, global).

% Jurists proposing novel solutions to new problems through rational analogy or juristic preference (istihsan) find the hierarchy as a constraint on creative methodology. The hierarchy's elevation of hadith and depression of qiyas limits the tools available for addressing unprecedented situations. Their innovation is reframed as methodologically illegitimate rather than as responsive reasoning.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, jurisprudential_innovation_advocates, excluded,
    moderate, biographical, constrained, regional).

% Caliphal authorities and state institutions benefit from having a published, systematized methodology they can cite as the official framework for legal reasoning. The hierarchy provides an institutional anchor point for claiming legitimate governance: 'We follow Shafi'i's method.' It reduces the appearance of arbitrary judicial decision-making by embedding law in an explicit order of precedence.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, institutional_regulators, agenda_setter,
    institutional, generational, mobile, global).

% Hanafi, Maliki, and Hanbali schools (and emerging jurisprudential movements) observe the Shafi'i framework and either adopt it, resist it, or selectively incorporate it. They witness the standardization as an attempt to impose a single hierarchy where methodological pluralism had existed. They take positions on whether the hierarchy is binding, superior, or legitimately challengeable.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, competing_methodological_schools, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__shafii_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a transparent, publishable methodology for resolving contradictions between jurists and schools. Before Shafi'i's systematization, schools operated with implicit methodologies embedded in case-by-case reasoning; the explicit four-tier hierarchy creates a common language for comparing schools' conclusions and adjudicating disagreements by reference to a shared order of authorities.
% TRANSFER_FUNCTION: Transfers epistemic authority from regional schools and individual jurists' rational judgment to those trained in hadith transmission and authentication. Authority flows from varied sources (customary practice, analogical reasoning, juristic preference) to a narrow gate controlled by hadith scholars. The constraint extracts deference to hadith authentication as the price of methodological legitimacy across the Islamic world.
% ABSENT_VOICES: Competing schools (Hanafi, Maliki, Hanbali jurists) are not represented in authoring the Shafi'i hierarchy itself; they would contest the ranking of qiyas versus ijma, the primacy of transmission authentication, and the subordination of customary practice. Regional legal communities, customary-law practitioners, and advocates for rational juristic extension are structurally excluded from the frame that valorizes textual transmission.
% DISAPPEARANCE_RATIONALE: If Shafi'i's hierarchy were abandoned, legal methodology would fragment back into regional schools operating on different assumptions: qiyas-centered reasoning would re-emerge as a primary tool, customary practice would re-assert authority in regions where it was established, and institutional uniformity would be lost. Jurists who organized their careers and training around hadith authentication would lose their institutional position. The coherence of Islamic jurisprudence as a single methodological project would collapse.
% FOUNDING_PROBLEM: Early Islamic jurisprudential schools operated with inconsistent methodologies: some prioritized customary practice, others analogical reasoning, others textual transmission. This created contradictions, jurisdictional conflicts, and made it impossible to adjudicate disputes between schools by reference to a shared standard. al-Shafi'i's hierarchy was created to resolve these inconsistencies by establishing a published, explicit order of authorities that all schools could reference.
% FOUNDING_PROBLEM_CORROBORATION: Shafi'i himself and his followers attest the founding problem was real and the hierarchy solves it. Later historiographers and methodologists attest the standardization brought coherence where inconsistency had reigned. However, competing schools (Hanafi, Maliki, Hanbali jurists) implicitly contest this by continuing to use methodologies that prioritize qiyas, customary practice, and juristic reasoning differently — they attest the problem was differently defined in their traditions, not that it was solved by Shafi'i's hierarchy. Independent historians of Islamic law document the diversity of early jurisprudential methods and the institutional centrality the Shafi'i system achieved, supporting both the reality of inconsistency and the fact of standardization, while remaining agnostic on whether standardization was necessary or beneficial.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures at 0.68 at interval end because the hierarchy achieves its stated coordination goal (resolving inconsistencies between schools through a transparent methodology) while simultaneously constraining the epistemological autonomy of schools that operated differently. Early extractiveness is lower (0.38 at t=0) because Shafi'i's method was initially one school among competitors without enforcement power; it rises sharply (0.38→0.62) over the first 100-unit period as institutional adoption accelerates and the hierarchy becomes institutionalized as the official jurisprudential standard. It plateaus (0.62→0.68 from t=100 to t=300) at a high level because the constraint has stabilized: hadith scholarship becomes the normative pathway to legal authority, customary-practice and qiyas-centered schools are subordinated but not eliminated, and regional traditions must operate within the hierarchy's frame or lose legitimacy. Suppression is high (0.71 at interval end) because the hierarchy's persistence depends on actively maintaining hadith authentication as the arbiter against challenges from schools using qiyas, customary practice, or juristic preference. Theater ratio rises from 0.22 to 0.42 over the interval, indicating that the constraint's enforcement increasingly involves performative commitment to the hierarchy (citations to Shafi'i's method in judgments, formal adherence in fatwas) rather than substantive application of the four-tier ranking — competing schools cite the hierarchy while continuing practices that contradict it.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute as different constraint types from each other: from the hadith scholar's seat (beneficiary, low directionality), the constraint appears as coordination that solved a real problem. From the Hanafi jurist's seat (payer, high directionality), the constraint appears as enforced methodological capture that eliminated a legitimate alternative reasoning framework. The engine's per-seat computation captures this divergence; the authored claim (tangled_rope) reflects the structural asymmetry: genuine coordination function (resolving school inconsistencies) coupled with asymmetric extraction (subordinating competing methodologies).
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars and methodological standardization proponents occupy the beneficiary end of directionality (d ≈ 0.15–0.25): they benefit from the hierarchy's institutional success without bearing significant costs; their career paths and scholarly prestige are elevated by the constraint. Customary-practice schools and qiyas-centered jurists occupy the target end (d ≈ 0.75–0.85): they bear the cost of methodological subordination, their reasoning is structurally devalued, and their regional autonomy is constrained. Early jurists (excluded stakeholders) are trapped — they cannot defend their own methodological intentions against posthumous systematization. Regional legal traditions occupy a high-extraction position (d ≈ 0.70) because they must either adopt the hierarchy (losing distinctive voice) or resist and lose institutional legitimacy. The hierarchy creates a binding choice structure: adopt the method and lose regional authority, or resist and lose empire-wide legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits incipient mandatrophy: the founding problem (inconsistent methodologies between schools) is live in the sense that schools continue to disagree on methodology, but the standardization resolves it by institutional fiat rather than by addressing the underlying epistemological disagreement. Competing schools (Hanafi, Maliki, Hanbali) continue to operate with different implicit methodologies even while formally citing Shafi'i's hierarchy. The theater ratio's rise (0.22→0.42) indicates increasing performative conformity to the hierarchy while substantive practice remains methodologically diverse. This is a hallmark of mandatrophy: the constraint's explicit function (resolve inconsistencies) is decoupled from its actual operation (institutionalize one school's methodology while suppressing others). The measuring point lies in the gap between the founding problem's stated status (live: schools still disagree on methodology) and the constraint's measured operation (theater rising, suggesting the hierarchy no longer resolves disagreement but merely enforces institutional conformity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authenticity_epistemology,
    'What counts as an authentically-transmitted hadith? Is hadith authentication a discovery of pre-existing transmission chains or a construction of scholarly consensus about which chains are acceptable?',
    'Comparative analysis of hadith authentication standards across schools: if standards are uniform and grounded in external criteria (isnad strength, transmitter reliability), authentication is discovery; if standards vary with school affiliation or change over time, it is construction.',
    'If hadith authentication is construction rather than discovery, the constraint''s claim to objectivity (the hierarchy resolves inconsistency through neutral ranking of authorities) is weakened. Hadith scholars become gatekeepers whose authority rests on institutional power rather than on transparent methodology. This would reframe the constraint from tangled_rope (genuine coordination + asymmetric extraction) to snare (extraction disguised as methodology).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authenticity_epistemology, conceptual, 'Whether hadith authentication is epistemic discovery or institutional construction.').

omega_variable(
    kernel_reading_vs_empirical_fact,
    'Is the four-tier hierarchy Shafi''i''s reading of how Islamic law should work, or Shafi''i''s claim about how law actually was grounded in earlier Islamic practice?',
    'Historiographical analysis of pre-Shafi''i jurisprudence: if early schools operated with implicit hierarchies that match Shafi''i''s, it is a descriptive reading of practice; if early schools explicitly used different methods (qiyas-first, customary-practice-first), it is a normative proposal for reform.',
    'If the hierarchy is a reading (normative proposal), the constraint''s founding problem is contestable — earlier schools might reasonably have seen their own methodologies as coherent rather than inconsistent. If it is a factual claim about practice, the constraint is a true coordination solution. This determines whether the constraint is correctly classified as tangled_rope (coordination + extraction) or as snare (extraction disguised as methodology).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_empirical_fact, empirical, 'Whether the hierarchy describes earlier practice or proposes reform.').

omega_variable(
    suppression_mechanism_in_jurisprudence,
    'Is the suppression of qiyas and customary practice structural (the hierarchy makes competing methods logically inaccessible) or internalized (jurists adopt the hierarchy''s frame and come to believe competing methods are illegitimate)?',
    'Textual analysis of post-Shafi''i jurisprudence: if jurists explicitly acknowledge qiyas as valid but subordinate it to the hierarchy, suppression is internalized (institutional authority structure, not logical foreclosure). If jurists actively deny qiyas legitimacy, suppression is structural.',
    'If suppression is internalized, the constraint''s effective extraction is higher than the structural measure suggests — payer seats carry the internalization with them even if the hierarchy''s enforcement relaxed. If suppression is structural, the extraction persists only through active institutional maintenance of the hierarchy. This affects whether T17 (enforcement decay) would progressively reclassify the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_jurisprudence, empirical, 'Whether suppression of competing methods is structural or internalized in jurists'' frameworks.').

omega_variable(
    kernel_foreclosure_among_readings,
    'Does the Shafi''i reading logically foreclose the Hanafi or Maliki readings, or do they coexist as live alternatives held by different institutional communities?',
    'Jurisprudential analysis: does adopting the Shafi''i hierarchy require denying the Hanafi or Maliki premises as false, or merely as lower in a shared ranking? If the premises can coexist (one school uses the hierarchy, another uses qiyas-first reasoning, both can cite Islamic sources), the readings coexist; if one reading''s core premise directly contradicts another''s, it forecloses.',
    'If readings coexist, the constraint is a real institutional competition between schools, not a resolution of the kernel. If the Shafi''i reading forecloses the Hanafi reading, the constraint is a decisive closure of what were previously live alternatives. This affects classification of network relationships between reading-constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_among_readings, conceptual, 'Whether the Shafi''i reading forecloses or coexists with sibling readings.').

omega_variable(
    beneficiary_intentionality,
    'Did hadith scholars and Shafi''i himself intend to benefit their own institutional position by ranking hadith authentication at the apex, or did they genuinely believe this ranking was the epistemic correct method?',
    'Biographical and textual analysis: do Shafi''i''s writings show awareness of how the hierarchy benefits hadith scholars, or is the hierarchy presented as discovered truth? Do hadith scholars'' subsequent writings show strategic deployment of the hierarchy, or consistent principled commitment?',
    'If the hierarchy was intentionally designed to capture institutional power, the constraint is snare (extraction disguised as methodology), not tangled_rope (genuine coordination with side extraction). If it was genuinely believed as correct methodology, the constraint is tangled_rope with some side extraction. This affects whether the constraint''s claim and classification align.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intentionality, preference, 'Whether the hierarchy was intended as institutional capture or principled methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__shafii_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(juri_tr_t50, observed).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__shafii_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement_basis(juri_tr_t100, observed).
narrative_ontology:measurement(juri_tr_t150, jurisprudential_method_kernel__shafii_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement_basis(juri_tr_t150, observed).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t250, jurisprudential_method_kernel__shafii_reading, theater_ratio, 250, 0.42).
narrative_ontology:measurement_basis(juri_tr_t250, observed).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__shafii_reading, theater_ratio, 300, 0.42).
narrative_ontology:measurement_basis(juri_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(juri_be_t50, observed).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement_basis(juri_be_t100, observed).
narrative_ontology:measurement(juri_be_t150, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 150, 0.67).
narrative_ontology:measurement_basis(juri_be_t150, observed).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 200, 0.68).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t250, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 250, 0.68).
narrative_ontology:measurement_basis(juri_be_t250, observed).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 300, 0.68).
narrative_ontology:measurement_basis(juri_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(juri_su_t50, observed).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 100, 0.64).
narrative_ontology:measurement_basis(juri_su_t100, observed).
narrative_ontology:measurement(juri_su_t150, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 150, 0.69).
narrative_ontology:measurement_basis(juri_su_t150, observed).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t250, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 250, 0.71).
narrative_ontology:measurement_basis(juri_su_t250, observed).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 300, 0.71).
narrative_ontology:measurement_basis(juri_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__shafii_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% The jurisprudential_method_kernel decomposes into four constraint stories, one per reading. Each reading instantiates a different constraint with a different ε, different beneficiary/victim structure, and different resistance profile. The Shafi'i reading (this story) authors medium-high ε measuring extraction from schools whose methodologies are subordinated beneath hadith authentication. The Hanafi reading would author lower ε (reasoning-based methodology less extractive than hadith gating). The Maliki reading would author extraction from those whose customary-practice authority is demoted. The Hanbali reading would author higher ε (literal-text constraint more suppressive of innovation). All four readings are linked via this affects_constraints network: each reading-constraint structurally influences the others by competing for institutional authority, changing legitimacy conditions, and creating pressure to adopt or resist the hierarchy. The readings coexist across different institutional communities (different schools, different regions, different eras of Islamic jurisprudence) without logical foreclosure: a jurist can remain Hanafi while acknowledging Shafi'i's hierarchy as a valid ranking within different premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__shafii_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
