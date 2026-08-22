% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Practice and Public Interest as Legal Sources
 *   domain: jurisprudence/legal_theory/islamic_law
 *
 * SUMMARY:
 *   The Maliki jurisprudential reading instantiates a specific methodological
 *   commitment within classical Islamic legal theory: that Medinan practice
 *   ('amal ahl al-Madina), public interest reasoning (maslaha mursala), and
 *   regional custom ('urf) are legitimate sources of law alongside and
 *   sometimes correcting textual sources (Quran and hadith). This reading
 *   dominates Maliki jurisprudence from its consolidation through the
 *   medieval and early modern periods and remains authoritative in North
 *   African and West African legal contexts. The reading is one of four major
 *   jurisprudential schools, each instantiating a different reading of the
 *   underlying kernel: the question of what sources and methods are valid for
 *   deriving Islamic law. The Maliki reading privileges practice-based and
 *   context-responsive reasoning; the Hanbali reading privileges textual
 *   restriction; the Hanafi reading privileges analogical reasoning and
 *   juristic discretion; the Shafi'i reading systematizes hadith
 *   authentication as the prerequisite gate. The constraint story models the
 *   Maliki reading as a structured arrangement: it benefits those embedded in
 *   regional legal traditions and custom, extracts cost from those committed
 *   to universalist textualism, and persists through institutional
 *   enforcement of the Maliki school's authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.62).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.48).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Jurisprudential Method: Medinan Practice and Public Interest as Legal Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "jurisprudence/legal_theory/islamic_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '81ff9f05-24fd-4280-9f4c-efdc55910900').
narrative_ontology:cs_kernel_codification('81ff9f05-24fd-4280-9f4c-efdc55910900', formalized).
narrative_ontology:cs_authority_grounding('81ff9f05-24fd-4280-9f4c-efdc55910900', lineage).
narrative_ontology:cs_interpretation_layer_present('81ff9f05-24fd-4280-9f4c-efdc55910900').
narrative_ontology:cs_reading_relation('81ff9f05-24fd-4280-9f4c-efdc55910900', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('81ff9f05-24fd-4280-9f4c-efdc55910900', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('81ff9f05-24fd-4280-9f4c-efdc55910900', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('81ff9f05-24fd-4280-9f4c-efdc55910900', foundational, medinan_practice_independent_source).
narrative_ontology:cs_axiom_status(medinan_practice_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('81ff9f05-24fd-4280-9f4c-efdc55910900', medinan_practice_independent_source, conventional).
narrative_ontology:cs_axiom('81ff9f05-24fd-4280-9f4c-efdc55910900', foundational, maslaha_mursala_valid_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('81ff9f05-24fd-4280-9f4c-efdc55910900', maslaha_mursala_valid_source, empirically_contingent).
narrative_ontology:cs_axiom('81ff9f05-24fd-4280-9f4c-efdc55910900', secondary, custom_integration_legitimacy).
narrative_ontology:cs_axiom_status(custom_integration_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('81ff9f05-24fd-4280-9f4c-efdc55910900', custom_integration_legitimacy, conventional).
narrative_ontology:cs_reference_frame('81ff9f05-24fd-4280-9f4c-efdc55910900', integrated_textual_and_practice_based_jurisprudence).
narrative_ontology:cs_drift_state('81ff9f05-24fd-4280-9f4c-efdc55910900', literalist_reformist_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('81ff9f05-24fd-4280-9f4c-efdc55910900', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_authorities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_legal_traditions).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualist_schools).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, hadith_literalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Maliki school's founding generation in Medina establishes the methodological framework that privileges local practice and public interest reasoning alongside textual sources. They justify this approach as grounding jurisprudence in the actual functioning legal community of the Prophet's city, where practice embodies communal consent. Their interpretive authority depends on the continuity of regional customary application and on resistance to universalist textualism.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_jurists, agenda_setter,
    institutional, generational, constrained, regional).

% Local judges, qadis, and community elders benefit from having their customary practices ('urf) recognized as legitimate legal sources when they do not contradict explicit textual rulings. They gain doctrinal authority to continue local norms without needing to justify them through textual derivation. Their structural position depends on the Maliki framework remaining authoritative in their region and on their practices not being overridden by universalist readings that privilege hadith over custom.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_authorities, beneficiary,
    moderate, generational, constrained, regional).

% The Maliki method vindicates the legitimacy of regional variation and locally-evolved jurisprudential traditions. Non-agent placeholder representing the institutional authority of Maliki jurisprudence as a whole in North African and West African legal domains.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_legal_traditions, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__maliki_reading, local_legal_traditions).

% Competing jurisprudential schools (primarily Hanbali and literalist reformist movements) that prioritize Quranic text and authenticated hadith as nearly exhaustive legal sources bear the cost of the Maliki framework's expansion of source-base beyond text. Where the Maliki method admits custom or public interest reasoning not supported by explicit hadith, this constrains the scope of application for universalist methods and reduces their interpretive authority. Their exit option—rejecting Maliki jurisprudence entirely—is blocked by geographic distribution and institutional embedding in regions where Maliki authority is established.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualist_schools, payer,
    institutional, generational, constrained, continental).

% Scholars and movements committed to hadith-centric jurisprudence (including Hanbali rigorists and later-period literalist reformists) experience the Maliki framework as diluting textual authority by permitting maslaha mursala and 'urf to override or supplement hadith-based derivation. They can exit by repositioning their scholarship toward non-Maliki regions or by arguing against the legitimacy of the Maliki method itself—which generates the resistance measured in the constraint.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hadith_literalists, payer,
    powerful, generational, mobile, continental).

% Later Maliki jurists and commentators (muttaqadimmun and mutaghalliqun eras) who systematize, defend, and refine the school's methodological commitments. They take no structural position but interpret how the framework's constraints operate and whether its core commitments remain coherent and authoritative.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_school_interpreters, observer,
    institutional, generational, analytical, regional).

% The Hanafi, Shafi'i, and Hanbali schools are not in conversation within this reading about the validity of the Maliki framework—they are excluded from shaping its development. They would dispute the status of 'amal ahl al-Madina and maslaha mursala as independent sources, but that dispute is carried out through separate methodological frameworks rather than through dialogue within the Maliki system itself.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, competing_jurisprudential_schools, excluded,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, medinan_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified methodological framework for Maliki jurisprudence that coordinates regional judges, jurists, and communities around a shared source-hierarchy: textual sources (Quran, authenticated hadith) are primary, but Medinan practice, public interest (maslaha mursala), and regional custom ('urf) provide legitimate supplementary and corrective sources. This solves the coordination problem of how local legal authority can operate with doctrinal legitimacy in situations where explicit textual guidance is absent or textual derivation would conflict with established regional practice.
% TRANSFER_FUNCTION: Transfers interpretive authority from those who would ground law in textual sources alone (hadith literalists, universalist scholars) to those who embed jurisprudence in communal practice and regional customary norms (Medinan jurists, local qadis, regional tradition-bearers). Authority flows from text-centered to context-centered reasoning, from universal hadith-based standards to locally-instantiated custom and public-interest judgments.
% ABSENT_VOICES: Hanbali and literalist reformist voices that would restrict jurisprudence to authenticated textual sources are excluded from this reading's internal dialogue—their objections to maslaha mursala and 'urf are not engaged as co-participants in the Maliki method's development, but rather as external competitors. Similarly, those who would reject regional variation entirely in favor of cosmopolitan uniformity based on hadith are outside the Maliki reading's consensus.
% DISAPPEARANCE_RATIONALE: If the Maliki methodological framework—specifically the authorization of Medinan practice, maslaha mursala, and 'urf as legitimate sources—ceased to function, North African and West African jurisprudence would reorganize. Regional legal traditions that depend on Maliki-framework legitimacy would either be forced to justify themselves through strict textual derivation (adopting Hanbali or Hanafi methods) or would lose doctrinal authority. The communal practice that the framework vindicates would either be suppressed or would require complete reformulation through alternative legal theory.
% FOUNDING_PROBLEM: Early Islamic jurisprudence required a method for handling legal questions arising in the Medinan community that textual sources (Quran and early hadith) did not explicitly address, while also accounting for the fact that established Medinan customary practice carried evidential weight as the accumulated consensus of the city where the Prophet established law. The problem: how to systematize jurisprudence in a way that honors both textual guidance and communal practice without collapsing into pure opinion or custom, but also without subjugating lived legal reality to textual formalism.
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists and later Maliki school historians (from Ibn 'Abd al-Barr onward) attest that the founding problem remained live throughout the classical period: reconciling textual authority with the evolution of regional law. Modern Islamic legal scholars outside the Maliki school (Hanbali, Hanafi, and comparative law scholars) contest whether this problem required the expansion of source-base to maslaha mursala and unrestricted 'urf, or whether strict textual derivation would have sufficed. Literalist-reformist movements (18th century onward) explicitly attest that the founding problem should have been solved by restricting jurisprudence to authenticated hadith, suggesting the Maliki solution is no longer necessary or legitimate.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the Maliki framework systematically privileges regional practice over universalist hadith application, which concentrates legal authority in the hands of those embedded in established custom. The framework imposes a cost on those who would ground law strictly in text: their interpretive reach is constrained where custom or maslaha supports a different outcome. Suppression is moderate (0.48) because the Maliki framework does not rely primarily on coercive enforcement to persist—it operates through institutional continuity and doctrinal acceptance. The framework is actively taught, defended, and refined, but not through suppression of alternatives so much as through competitive assertion of methodological validity. Theater is low (0.22) because the Maliki method's primary function—coordinating regional jurisprudence around acknowledged sources—remains operative throughout the interval. The rising trajectory of both extractiveness and theater reflects the gradual shift from early-period competitive assertion of the method toward later-period consolidation and defensive refinement (muttaqadimmun to mutaghalliqun era), where more energy goes into defending established positions against Hanbali and literalist critique. The measurements follow one shared time grid at each interval point.
 *
 * PERSPECTIVAL GAP:
 *   From the Medinan jurists' and regional authorities' position, the framework is genuine coordination: it solves the problem of how to maintain legitimate legal authority while respecting communal practice and evolving circumstances. From the hadith literalists' position, the same framework operates as enforced extraction: it subordinates textual authority to cultural relativism and allows local interests to override divine guidance. The engine computes this divergence from the structural data. The agenda-setter seat (Maliki institutional authority) should compute the type differently from the payer seats (textualists constrained in their interpretive application). The agenda-setter's control over source-hierarchy definition and the payers' inability to apply textual reasoning when custom or maslaha blocks it drives the perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The Maliki jurists and regional customary authorities are structural beneficiaries: they hold interpretive authority by virtue of their embeddedness in established practice, and the framework legitimizes their continued role. The constraint supplies them with doctrinal standing they would not have under purely textualist methods. Universalist textualists and hadith literalists are targets: their interpretive reach is constrained where the Maliki framework permits custom or maslaha to override or supplement textual reasoning. Their exit—repositioning toward Hanbali jurisprudence or launching literalist reform movements—comes with real institutional cost, hence the constrained exit_options. The directionality derivation is straightforward: beneficiaries have low d (subsidized interpretive authority), payers have high d (costs imposed through restriction of alternative methods). No directionality overrides are required.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy through sustained evidence that the founding problem—integrating textual authority with communal legal practice—remains live. As long as regional legal systems require justification that accounts for both textual sources and evolved custom, the Maliki framework coordinates a real solution. However, the contestation omega (founding_problem_status = contested) documents the literalist-reformist challenge: later movements argue the problem should have been solved differently, by restricting jurisprudence to authenticated hadith alone. If literalist movements gain institutional authority, the founding problem's vitality could shift from live to dead, triggering mandatrophy. The steady-state extractiveness (plateauing at 0.62 after t=40) and low theater ratio (0.22) suggest the constraint is not currently performative, but the rising suppression requirement trajectory indicates increasing effort required to defend the framework against textualist challenges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maslaha_mursala_scope_ambiguity,
    'What is the legitimate scope of maslaha mursala (unrestricted public interest reasoning)? Does it extend to situations where it contradicts established hadith, or only where textual sources are entirely silent?',
    'Genealogy of Maliki interpretations from foundational texts (Malik''s Muwatta, Ibn ''Abd al-Barr''s commentaries) through later school development (al-Qarafī, al-Shāṭibī) to determine whether the method consistently restricts maslaha to textual silence or permits it to override authenticated hadith. Comparison with documented jurisprudential disputes within the school.',
    'If maslaha scope is narrower (applies only where text is silent), the constraint''s extractiveness would be lower because textualists retain greater authority over cases with hadith support. If broader (permits override of hadith), the constraint''s extractiveness is confirmed, because regional judges gain authority to depart from authenticated sources on public-interest grounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_scope_ambiguity, empirical, 'Whether maslaha mursala operates as supplement to textual silence or as corrective override of text.').

omega_variable(
    medinan_practice_codification,
    'How is ''amal ahl al-Madina (Medinan practice) itself established and adjudicated? What makes a claimed practice count as established Medinan custom rather than individual judgment?',
    'Analysis of how Malik and later Maliki jurists identify which practices qualify as ''amal ahl al-Madina: what threshold of transmission, continuity, or consensus must a practice meet? Comparison with how practices are established in competing schools'' methodologies.',
    'If the standard for establishing Medinan practice is stringent (multiple reliable transmissions, documented historical continuity), the constraint operates with meaningful textual-equivalent authority and extractiveness is moderate. If the standard is flexible (single authoritative report, contemporary consensus of Medinan judges), the framework permits substantial local discretion, raising extractiveness and reducing institutional constraint on regional judges'' authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_codification, empirical, 'Whether Medinan practice is rigorously established or flexibly ascertained.').

omega_variable(
    custom_contradiction_boundary,
    'The framework permits ''urf (custom) except where it contradicts textual sources. How is this contradiction determined? Does explicit textual prohibition suffice, or must textual contradiction be clear and unambiguous?',
    'Genealogy of Maliki jurisprudential disputes involving custom and textual sources: cases where local practice appears to contradict hadith or Quranic ruling, and how Maliki jurists resolved the conflict. Comparison of strict vs. lenient interpretations of what counts as genuine contradiction.',
    'Strict interpretation (explicit textual contradiction required) permits custom to override ambiguous or weak hadith, raising extractiveness and limiting textualist authority. Lenient interpretation (any tension with text blocks custom) preserves greater textualist constraint, lowering extractiveness. The boundary determines how much doctrinal authority local judges retain independent of textual scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_contradiction_boundary, empirical, 'Whether custom is blocked only by explicit textual prohibition or by any textual tension.').

omega_variable(
    method_vs_readings_framing,
    'Is the Maliki jurisprudential reading a stable method that persists across time, or is it a cluster of interpretations whose coherence is contested and revisable by later Maliki jurists?',
    'Periodization of Maliki jurisprudence: do muttaqadimmun (early masters), mutatawassit (middle period), and mutaghalliqun (later period) Maliki jurists maintain the same understanding of maslaha, ''amal, and ''urf, or do they revise and reinterpret core commitments? Comparison of foundational texts (Malik''s Muwatta) with later systematizations (al-Qarafī''s Preamble).',
    'If the method is a stable fixed framework, the constraint''s type and extractiveness are stable across the interval. If later Maliki jurists reinterpret or narrow the scope of maslaha and ''urf under pressure from Hanbali critique, the framework may evolve toward greater textualism, lowering extractiveness and shifting the type toward rock more than rope. If later interpreters expand scope, extractiveness rises and tension with textualist schools increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(method_vs_readings_framing, conceptual, 'Whether Maliki methodology is fixed or revisable across interpretive generations.').

omega_variable(
    kernel_reading_stability,
    'Does the Maliki reading remain a coherent instantiation of the ''usul_al_fiqh_method kernel, or has literalist-reformist critique successfully challenged the legitimacy of the core axioms (that maslaha mursala and custom are valid independent sources)?',
    'Genealogy of literalist-reformist challenge to Maliki jurisprudence (18th-century onward): do reformists successfully demonstrate internal incoherence in the Maliki method, or do they mount an external critique asserting a different reading of the kernel entirely? Comparison of reformist claims about what the kernel requires with Maliki defense of their instantiation.',
    'If reformists mount successful internal critique showing the Maliki method is unstable, the kernel reading itself becomes contested and unstable—the type may shift toward piton (maintained by inertia despite contested foundations). If reformists offer an alternative reading (their own literalist instantiation of the kernel), the Maliki and literalist readings coexist-with status is confirmed but with heightened resistance. The amplitude of the resistance measurement would indicate the severity of the challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether the Maliki reading remains a coherent kernel instantiation or is destabilized by literalist critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__maliki_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__maliki_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__maliki_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__maliki_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__maliki_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__maliki_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__maliki_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__maliki_reading, base_extractiveness, 30, 0.61).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__maliki_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__maliki_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__maliki_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__maliki_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__maliki_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__maliki_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__maliki_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__maliki_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The Maliki reading is one of four major jurisprudential schools instantiating different readings of the 'usul_al_fiqh_method kernel. Each school instantiates a distinct method with different source hierarchies, beneficiaries, and costs. The Maliki reading privileges Medinan practice and public interest (maslaha mursala) over universalist textualism; the Hanbali reading privileges textual restriction and blocks maslaha mursala; the Hanafi reading privileges expansive analogical reasoning (qiyas) and juristic discretion (ra'y); the Shafi'i reading systematizes hadith authentication as the prerequisite gate and formally hierarchizes sources. These are not different aspects of a single method—they are different answers to the same kernel question (what sources and methods are valid?), with different structural benefits and costs. Each reading has its own ε (the Maliki reading's ε = 0.62 measures how much it privileges practice-based reasoning over textualism; the Hanbali reading's ε measures the cost of restricting jurisprudence to text; they may diverge significantly). The network link indicates contamination propagation: if textualist authority erodes in Maliki regions, Hanbali competing claims strengthen (influences); if literalist-reformist movements successfully delegitimize maslaha mursala, Maliki extractiveness may fall as the method converges toward Hanbali restriction (influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__maliki_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
