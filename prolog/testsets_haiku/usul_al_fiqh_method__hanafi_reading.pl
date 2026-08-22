% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Usul al-Fiqh Jurisprudential Method
 *   domain: legal/theological/epistemic
 *
 * SUMMARY:
 *   This constraint instantiates the Hanafi reading of usul al-fiqh (Islamic
 *   jurisprudential methodology), emphasizing expansive applicability of
 *   qiyas (analogical reasoning), ra'y (reasoned opinion), and istihsan
 *   (juristic preference) when textual sources are silent. The Hanafi method
 *   establishes jurist reasoning as a primary, binding epistemic authority
 *   alongside scriptural sources. This reading is one of four competing
 *   schools of Islamic jurisprudence (Hanafi, Maliki, Shafi'i, Hanbali), each
 *   with a distinct answer to the question: what is the proper hierarchy of
 *   legal sources and the scope of jurist derivation? The Hanafi framework
 *   benefits its practitioners—the trained jurist class—by granting them
 *   maximum discretion in legal reasoning. It imposes costs on textualist
 *   claims by systematically marginalizing them within Hanafi-dominant legal
 *   systems. The constraint is claimed as tangled_rope (genuine coordination
 *   function of systematic legal derivation plus asymmetric extraction
 *   favoring rationalist jurists) and the metrics describe a moderately
 *   extractive, actively enforced interpretive arrangement that has
 *   strengthened over time as Hanafi institutional dominance crystallized.
 *
 * KEY AGENTS:
 *   - Hanafi jurist class (institutional agenda-setter, identity-locked to the method, greatest beneficiary)
 *   - Rationalist legal tradition (institutional beneficiary, vindicated by Hanafi validation of reason as source)
 *   - Textualist interpretive claim (non-agent payer, constrained within Hanafi framework)
 *   - Non-Hanafi legal communities (organized payer, structurally disadvantaged where Hanafi law is institutional default)
 *   - Hanbali textualist school (excluded from the Hanafi methodological conversation, constrained in mixed jurisdictions)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.55).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Usul al-Fiqh Jurisprudential Method").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/theological/epistemic").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'b7a54161-4ef5-4066-b777-a60c4ccbaea8').
narrative_ontology:cs_kernel_codification('b7a54161-4ef5-4066-b777-a60c4ccbaea8', formalized).
narrative_ontology:cs_authority_grounding('b7a54161-4ef5-4066-b777-a60c4ccbaea8', lineage).
narrative_ontology:cs_interpretation_layer_present('b7a54161-4ef5-4066-b777-a60c4ccbaea8').
narrative_ontology:cs_reading_relation('b7a54161-4ef5-4066-b777-a60c4ccbaea8', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7a54161-4ef5-4066-b777-a60c4ccbaea8', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7a54161-4ef5-4066-b777-a60c4ccbaea8', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('b7a54161-4ef5-4066-b777-a60c4ccbaea8', foundational, reason_as_independent_legal_source).
narrative_ontology:cs_axiom_status(reason_as_independent_legal_source, holdable).
narrative_ontology:cs_axiom_grounding('b7a54161-4ef5-4066-b777-a60c4ccbaea8', reason_as_independent_legal_source, deontological).
narrative_ontology:cs_axiom('b7a54161-4ef5-4066-b777-a60c4ccbaea8', foundational, qiyas_permissible_upon_textual_silence).
narrative_ontology:cs_axiom_status(qiyas_permissible_upon_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('b7a54161-4ef5-4066-b777-a60c4ccbaea8', qiyas_permissible_upon_textual_silence, conventional).
narrative_ontology:cs_axiom('b7a54161-4ef5-4066-b777-a60c4ccbaea8', secondary, istihsan_valid_for_public_interest).
narrative_ontology:cs_axiom_status(istihsan_valid_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('b7a54161-4ef5-4066-b777-a60c4ccbaea8', istihsan_valid_for_public_interest, instrumental).
narrative_ontology:cs_reference_frame('b7a54161-4ef5-4066-b777-a60c4ccbaea8', rationalist_jurisprudential_authority).
narrative_ontology:cs_drift_state('b7a54161-4ef5-4066-b777-a60c4ccbaea8', contemporary_textual_challenge_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('b7a54161-4ef5-4066-b777-a60c4ccbaea8', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_legal_tradition).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_interpretive_claim).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, non_hanafi_legal_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hanafi jurists set and defend the jurisprudential method permitting expansive qiyas, ra'y, and istihsan. They maintain the methodological framework through scholarly transmission, institutional training of successive generations, and adjudication of practical cases. They benefit from the expanded scope for rational derivation, which establishes their expertise as indispensable and their legal opinions as binding within the Hanafi madhhab. Their professional identity and intellectual authority are constituted through mastery of this particular usul framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, civilizational, identity_locked, global).

% The broader rationalist legal tradition within Islamic jurisprudence benefits from the Hanafi validation of reason ('aql) as a binding epistemic authority independent of textual constraint. This reading establishes that jurist reasoning is not merely supplementary to scriptural sources but constitutes a primary source of law. The tradition's intellectual prestige and institutional expansion depend on the Hanafi method's continued recognition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_legal_tradition, beneficiary,
    institutional, civilizational, arbitrage, global).

% The textualist claim—that scriptural sources (Quran and authenticated hadith) should be maximally restrictive and qiyas minimized—bears the cost of the Hanafi method's expansiveness. The textualist position is systematically constrained: where the Hanafi framework permits derivation by analogy and juristic preference, textualist legal claims are displaced or overridden. The textualist approach cannot advance its reading within Hanafi jurisprudence without fundamentally challenging the methodological foundations the Hanafi school has established.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_interpretive_claim, payer,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__hanafi_reading, textualist_interpretive_claim).

% Communities following other madhhabs (Maliki, Shafi'i, Hanbali) pay a competitive cost: within Hanafi-dominant legal systems and regions, their alternative methodologies are subordinated. Where Hanafi jurisprudence is the institutional default (Ottoman Empire, Mughal India, contemporary Turkey, parts of Egypt), practitioners of other schools operate at a structural disadvantage. They cannot freely apply their own methodological principles; they must either conform to Hanafi reasoning or accept institutional marginalization. Their exit is constrained by geographic location, institutional inheritance, and the difficulty of switching jurisprudential frameworks.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, non_hanafi_legal_communities, payer,
    organized, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, non_hanafi_legal_communities, excluded).

% The Hanbali school, which emphasizes textual restrictiveness and minimizes qiyas, is structurally excluded from the Hanafi methodological conversation. Where Hanafi jurisprudence dominates institutional settings, Hanbali reasoning is treated as overly rigid and impractical. Hanbali jurists can hold their textualist position, but they operate outside the framework that the Hanafi method establishes as authoritative within the institutions they inhabit.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanbali_textualist_school, excluded,
    organized, civilizational, constrained, global).

% Comparative Islamic legal scholarship examines the Hanafi method as one reading among four canonical school positions. Scholars analyze how the method's expansiveness in permitting qiyas, ra'y, and istihsan produces different legal outcomes than other schools, and how the historical dominance of Hanafi jurisprudence in certain regions reflected both theological coherence and political/institutional power.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, islamic_legal_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, reproducible method for deriving law when scriptural sources are silent or insufficient. By systematizing qiyas (analogy), ra'y (reasoned judgment), and istihsan (juristic preference), the Hanafi method permits jurists across generations to reach consistent legal conclusions through rational application of fixed principles, rather than via ad-hoc innovation or unprincipled discretion.
% TRANSFER_FUNCTION: Transfers authority from the strictest possible reading of texts to the combined authority of texts plus jurist reasoning. Hanafi jurisprudence moves the burden of proof: a textualist must show textual constraint is binding; the Hanafi jurist need only show the case is not clearly resolved by text to legitimize rational derivation. This redistributes interpretive power from textual closure to jurist expertise.
% ABSENT_VOICES: Textualist jurists, particularly those within Hanbali and literalist traditions, would object that the method permits excessive innovation ('bid'a) and subordinates revealed sources to human reason. They are structurally absent from the internal Hanafi conversation because the Hanafi framework defines their concern as already-resolved: the method IS the authoritative framework, and textualism is its boundary condition, not its rival.
% DISAPPEARANCE_RATIONALE: If the Hanafi method disappeared—replaced by pure textualism or another framework—the legal systems, training institutions, and jurisprudential literature that depend on it would require wholesale reorganization. Ottoman jurisprudence, Mughal legal practice, and contemporary Turkish law would need new methodological foundations. Thousands of legal conclusions derived through Hanafi reasoning would require re-examination under alternative principles.
% FOUNDING_PROBLEM: Early Islamic legal communities faced cases for which scriptural sources were genuinely insufficient: matters of technology, commerce, and social practice that post-dated revelation or were never explicitly addressed. The founding problem was: how does a jurist legitimately derive law when the Quran and authenticated hadith are silent? The Hanafi method systematizes the answer: through rational analogy to textually grounded principles.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists attest the founding problem is perpetually live: each generation encounters new cases requiring derivation beyond textual constraint. Textualist scholars (Hanbali, literalist) attest the founding problem has been misdiagnosed: they argue authentic hadith is far richer than rationalist readings acknowledge, and that cases truly silent in the sources should be deferred rather than resolved by reason. Contemporary Islamic legal scholarship confirms the disagreement is structural, not empirical: the contest is over whether scriptural silence is a gap to fill or a boundary to respect.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness increases modestly over the interval (0.52 → 0.68) as Hanafi jurisprudence becomes institutionalized and its assumptions naturalized, making the textualist alternative appear less viable within the system. Theater remains modest (0.28) because the coordination function (systematic legal derivation) is genuine; the method actually solves the problem of case-by-case inconsistency. Suppression is moderate (0.55) because the constraint's persistence requires active defense: alternative schools must be kept at the institutional margins through training, institutional hierarchy, and the self-reinforcing nature of jurisprudential tradition (practicing within the system requires mastery of its principles). Accessibility collapse is moderate (0.62): alternatives exist (Hanbali, Shafi'i methods) and are intellectually coherent, but they are difficult to access once one is embedded in Hanafi training and practice. Resistance is high (0.71) because textualist scholars and communities actively resist the Hanafi expansion of jurist discretion; the contest between schools is not settled but perpetual. All measurements are shared on one time grid spanning 40 units (t0=0, tn=40).
 *
 * PERSPECTIVAL GAP:
 *   Hanafi jurists sit at a seat of institutional power: they author and transmit the method, train the next generation, and adjudicate disputes within it. Their exit is identity-locked—leaving the Hanafi framework means abandoning professional identity, scholarly authority, and institutional position. From their seat, the framework solves a genuine problem and does so rationally. Textualist jurists sit at a seat of institutional marginalization within Hanafi-dominant systems: their reasoning is treated as impractical and overly rigid. Their exit is constrained—they can hold textualist views but must operate outside mainstream institutions. From their seat, the Hanafi framework appears as a cover story for jurist discretion. The engine computes these divergent classifications from the structural data; explaining the divergence is the point of the constraint story.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declaration drives directionality: beneficiaries (Hanafi jurists, rationalist tradition) sit near the beneficiary end of the directionality spectrum (d=0.0-0.2); victims (textualist claim, non-Hanafi communities) sit near the target end (d=0.8-1.0). Identity-locked exit for Hanafi jurists amplifies their beneficiary directionality—they cannot leave the framework without losing professional identity. Constrained exit for non-Hanafi communities amplifies their target directionality—they cannot freely apply their methods where Hanafi law dominates. The metrics (high extractiveness, moderate suppression) reflect an arrangement where the beneficiary seat exercises institutional control and the target seats must conform or accept marginalization.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as snare by the presence of genuine coordination function: when jurists in different times and places apply Hanafi usul, they reach consistent conclusions through systematic application of principles, not ad-hoc invention. The constraint avoids misclassification as rope by the asymmetric distribution of authority: a textualist jurist cannot advance textualist reasoning within Hanafi institutions; a Hanafi jurist can advance Hanafi reasoning throughout Hanafi-dominant systems. Tangled_rope is the correct type because both coordination and extraction are structural, neither is mere cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_silence_construction,
    'Is scriptural silence in Islamic sources a genuine gap requiring rational derivation, or a designed boundary that should be respected by deferring undecided questions to uncertainty?',
    'Comparative analysis of Quranic and hadith scope across centuries of Islamic jurisprudence. Examine whether the frequency of cases genuinely outside textual constraint has remained constant or declined as scholarship deepened. Survey contemporary Islamic legal practice: do modern jurists encounter cases that no text addresses, or do they cite textual constraint for cases of ambiguity?',
    'If silence is genuine and unavoidable, the Hanafi method''s coordination function is perpetually necessary, and extraction is the price of coordination. If cases of genuine silence are rare or shrinking, the method''s persistence becomes more theatrical, and mandatrophy risk rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_silence_construction, conceptual, 'Whether the founding problem (scriptural silence as gap) is empirically persistent or has been misidentified.').

omega_variable(
    suppression_mechanism_ambiguity,
    'To what extent is the Hanafi method''s dominance sustained by structural institutional gatekeeping versus internalized acceptance of its axioms by non-Hanafi scholars?',
    'Post-institutional suppression trajectory: examine whether textualist and non-Hanafi jurisprudential movements flourish when liberated from institutional gatekeeping (e.g., in contemporary contexts where multiple madhhabs coexist without state enforcement). If textualism revives when institutional suppression is removed, the suppression was structural; if textualism remains marginal despite institutional freedom, suppression is at least partially internalized.',
    'If suppression is purely structural, alternatives could be liberated by removing institutional preference (low cost to fix). If internalized, the constraint carries its suppressive force even after institutional gates are opened, making it more difficult to remediate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression of alternative jurisprudential approaches is structural or internalized in Islamic legal communities.').

omega_variable(
    kernel_reading_indexation,
    'Is the Hanafi reading of usul al-fiqh a discovered natural law of Islamic jurisprudence, or a constructed institutional choice that could have been otherwise?',
    'Historical counterfactual: examine the early Islamic period when multiple methodologies competed (8th-9th centuries) and identify path-dependent moments where the Hanafi approach could have been displaced. Assess whether Hanafi jurisprudence''s eventual dominance reflects inherent methodological superiority or institutional/political contingencies (Ottoman patronage, bureaucratic adoption, religious authority alignment).',
    'If discovered natural law, the constraint is closer to mountain classification and the beneficiary/victim structure reflects mere differential capacity rather than exploitation. If constructed choice, the beneficiary/victim structure reflects institutional capture of jurisprudential authority by the rationalist school.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indexation, conceptual, 'Whether the Hanafi reading is a canonical natural law or a contingent institutional choice that benefited particular actors.').

omega_variable(
    kernel_commensurate_alternatives,
    'For the sibling readings (Maliki, Shafi''i, Hanbali), do they instantiate genuinely incommensurable jurisprudential frameworks, or can they be reconciled within a single overarching Islamic legal principle?',
    'Comparative reconstruction of each school''s foundational axioms and their relationship to Quranic/hadith sources. Determine whether the schools differ on empirical claims about what texts say (resolvable by scholarship) or on normative claims about what role reason should play (not resolvable by scholarship). If empirical, convergence is possible; if normative, the schools may be permanently incommensurable.',
    'If incommensurable, the schools are genuinely competing positions and the Hanafi dominance is a constraint that subordinates alternatives. If resolvable, the Hanafi method might be correctible via better textual scholarship, and its dominance is temporary rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_commensurate_alternatives, conceptual, 'Whether competing jurisprudential schools are incommensurable or could be reconciled through deeper analysis.').

omega_variable(
    rationalist_tradition_independence,
    'Does the rationalist legal tradition constitute an independent agent benefiting from Hanafi jurisprudence, or is it merely a proxy for the interests of the Hanafi jurist class?',
    'Examine historical instances where rationalist principles diverged from Hanafi institutional interests. Assess whether rationalist jurisprudential movements have maintained their independence when separated from Hanafi institutional power (e.g., in Mu''tazili theology, contemporary Islamic legal reform). Determine whether rationalism is the axiom beneficiaries defend, or merely the tool they deploy.',
    'If independent, the extraction benefit is distributed across jurist class and rationalist tradition as distinct beneficiaries. If proxy, the extraction consolidates on the jurist class alone, and rationalist tradition is a beneficiary in name only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_tradition_independence, empirical, 'Whether rationalist legal tradition is an independent beneficiary or a proxy for jurist-class interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(usul_tr_t5, usul_al_fiqh_method__hanafi_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__hanafi_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(usul_tr_t15, usul_al_fiqh_method__hanafi_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(usul_tr_t25, usul_al_fiqh_method__hanafi_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(usul_be_t5, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(usul_be_t15, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(usul_be_t25, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(usul_su_t5, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(usul_su_t15, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(usul_su_t25, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel usul_al_fiqh_method. The four canonical readings (Hanafi, Hanbali, Maliki, Shafi'i) are authored as separate constraints, each with its own ε-invariant metrics, beneficiary/victim structure, and cs_structure axioms. The Hanafi reading instantiates the rationalist end of the methodological spectrum; the Hanbali reading instantiates the textualist end. The Maliki and Shafi'i readings occupy middle positions with distinct axioms. All four readings coexist as live jurisprudential traditions within Islamic law; none forecloses the others across the full span of Islamic legal history, though in particular regional/institutional contexts (Ottoman, Mughal, contemporary) one reading may dominate the others. The network.affects_constraints links document this family structure; each story's cs_structure.reading_relations specify the structural relationships from that reading to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
