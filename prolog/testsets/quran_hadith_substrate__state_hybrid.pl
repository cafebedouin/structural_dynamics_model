% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__state_hybrid, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State Hybrid Islamic Law Application (Qur'an/Hadith Substrate)
 *   domain: legal/political/religious
 *
 * SUMMARY:
 *   A religiously-identified state institutionalizes a dual-track legal
 *   system: classical Islamic jurisprudence (Qur'an, Hadith, established
 *   madhhab rulings) in family law and criminal codes; reformist, contextual,
 *   or secular frameworks in commercial, administrative, and economic law.
 *   Legitimacy is grounded in political sovereignty and instrumental efficacy
 *   rather than doctrinal fidelity to either comprehensive taqlid
 *   (traditionalist) or rights-centered ijtihad (reformist). The state
 *   benefits from satisfying multiple constituencies
 *   simultaneously—traditionalists gain authority recognition in culturally
 *   sensitive domains; commercial elites and modernizers gain operational
 *   flexibility; the regime gains legitimacy synthesis. Traditionalist and
 *   reformist scholars suffer symmetric costs: their comprehensive visions
 *   are truncated, and whichever reading the state excludes from a domain is
 *   effectively suppressed. Populations subject to classical family and
 *   criminal law bear the extraction cost as they cannot exit into reformed
 *   frameworks even where those would accord them greater autonomy or rights
 *   protection.
 *
 * KEY AGENTS:
 *   - State regime elites — agenda_setter, institutional power, arbitrage exit; sets domain boundaries instrumentally and collects legitimacy rents from both poles
 *   - Traditionalist scholars — organized power, constrained exit, secondary beneficiary; gain amplification in family/criminal domains, suffer suppression of comprehensive vision
 *   - Reformist scholars — organized power, constrained exit, secondary beneficiary; gain policy influence in commerce/administration, suffer suppression in identity-sensitive domains
 *   - Commercial interests — powerful, mobile exit; benefit from secular/reformist frameworks that enable modern economic operations
 *   - Populations in family law domain — powerless, identity_locked exit; forced into classical frameworks without reformist option
 *   - Populations in criminal law domain — powerless, trapped exit; subject to classical law enforced without alternative
 *   - Constitutional oversight institutions — observer, analytical exit; can measure doctrinal coherence but may be captured by regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.52).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State Hybrid Islamic Law Application (Qur'an/Hadith Substrate)").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "legal/political/religious").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, '68850ab3-4007-49ff-9eae-3136c86bb507').
narrative_ontology:cs_kernel_codification('68850ab3-4007-49ff-9eae-3136c86bb507', distributed).
narrative_ontology:cs_authority_grounding('68850ab3-4007-49ff-9eae-3136c86bb507', extraction).
narrative_ontology:cs_interpretation_layer_present('68850ab3-4007-49ff-9eae-3136c86bb507').
narrative_ontology:cs_reading_relation('68850ab3-4007-49ff-9eae-3136c86bb507', quran_hadith_substrate__traditionalist_taqlid, coexists_with).
narrative_ontology:cs_reading_relation('68850ab3-4007-49ff-9eae-3136c86bb507', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_axiom('68850ab3-4007-49ff-9eae-3136c86bb507', foundational, state_sovereignty_grounds_selection).
narrative_ontology:cs_axiom_status(state_sovereignty_grounds_selection, holdable).
narrative_ontology:cs_axiom_grounding('68850ab3-4007-49ff-9eae-3136c86bb507', state_sovereignty_grounds_selection, instrumental).
narrative_ontology:cs_axiom('68850ab3-4007-49ff-9eae-3136c86bb507', secondary, domain_partition_coordinates_legitimacy).
narrative_ontology:cs_axiom_status(domain_partition_coordinates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('68850ab3-4007-49ff-9eae-3136c86bb507', domain_partition_coordinates_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('68850ab3-4007-49ff-9eae-3136c86bb507', post_independence_dual_capacity_state).
narrative_ontology:cs_drift_state('68850ab3-4007-49ff-9eae-3136c86bb507', contemporary_rights_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68850ab3-4007-49ff-9eae-3136c86bb507', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_regime_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_interests).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, populations_bound_by_classical_rulings).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__state_hybrid_tests).
:- end_tests(quran_hadith_substrate__state_hybrid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint does solve a genuine coordination problem (multiple scholars producing incommensurable rulings would paralyze governance) while simultaneously extracting: the state instrumentalizes both doctrinal poles for legitimacy, neither scholars nor populations have meaningful exit, and the partition serves regime interests more than doctrinal logic. Suppression is moderate (0.52) because it is variable—suppression of disfavored readings is active and coercive, but not total (reformist scholars operate in permitted domains; traditionalist influence remains in permitted domains). Theater ratio is high and rising (0.48→0.61) because the justifications for domain-partition (classical law for identity, reformist law for modernity) increasingly function as cover for instrumental selectivity. As regime pressures shift, the boundary movements become visible and the rationales feel more performative. Rising theater with stable or slightly rising extraction is piton-grade signal: the regime is maintaining the legitimacy synthesis through increasingly theatrical appeals to principle. Accessibility collapse is moderate (0.48) because alternatives—exit to reformist regimes, traditionalist theocracies, or secular states—exist but are costly; populations cannot easily switch domains within the same state. Resistance is high (0.71) because traditionalist scholars, reformist scholars, and populations in restricted domains all mount active resistance to the partition, even under suppression.
 *
 * PERSPECTIVAL GAP:
 *   The state agenda-setter seat computes the constraint as coordination (solving real governance problems, maintaining legitimacy synthesis). The traditionalist and reformist scholar seats compute it as suppression (their comprehensive visions are truncated instrumentally). The victim populations compute it as pure extraction (their autonomy and legal options are removed without their consent). The commercial beneficiary seat computes it as pure benefit (they gain flexibility at no cost). The constitutional oversight seat, if independent, computes it as incoherent (the principled rationales do not explain the pattern). The engine derives these divergent types from the structural data: the beneficiary seat (state elites with arbitrage exit) sees coordination benefit; victim seats (constrained, trapped, or identity_locked exit) see extraction; observer seats see the pattern that triggered the analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   State regime elites hold arbitrage exit (they can switch ideological justifications and shift domain boundaries as regime interests change; they are not locked into either classical or reformist commitment). This derives d toward 0.0 (beneficiary). Traditionalist and reformist scholars hold constrained exit (they cannot exit the legal system or the regime; their voice is conditional on the state's decision about which domains their reading controls). This derives d toward 0.8–0.9 (partial target). Populations in family and criminal domains hold identity_locked or trapped exit (they cannot exit without apostasy or flight; they are bound to the frameworks the state selects). This derives d toward 1.0 (full target). Commercial interests hold mobile exit (they can relocate or pressure the regime; they benefit from the secular framework and face low cost to exit if it were removed). This derives d toward 0.2–0.3 (partial beneficiary). The directionality spread is wide because the constraint's effect is radically asymmetric across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-independence Islamic states need both Islamic legitimacy and modern governing capacity) is increasingly contested as live. Traditionalists argue taqlid includes maslaha reasoning that can accommodate contemporary challenges. Reformists argue ijtihad has always been the proper method and classical law is a historical contingency, not a requirement. Modern populations and international human rights bodies argue the founding problem was overstated—the partition now serves regime interests more than genuine necessity. As the founding problem status shifts from live to contested/dead while the constraint persists with rising theater ratio, the constraint enters mandatrophy territory. The regime maintains the partition not because it solves an essential problem but because it preserves regime flexibility and legitimacy synthesis. This is classical piton motion: a former coordination solution (managing the tension between Islamic authority and modern governance) has atrophied into pure theater. The state maintains classical family law not because taqlid consensus demands it but because abandoning it would face traditionalist backlash; it maintains secular commerce law not because reformist ijtihad demands it but because refusing it would paralyze economic policy. Neither is believed; both are performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contention,
    'Is this constraint one instantiation of the same kernel that traditionalist and reformist readings instantiate, or do the three readings infer different kernels entirely?',
    'Textual and jurisprudential analysis: do the three readings claim to interpret the same Qur''an/Hadith substrate, or do they disagree on what the substrate IS? If traditionalists and reformists agree that ijtihad and taqlid are both responses to a single source but the state-hybrid reading reframes the source as ''whatever the state declares,'' the readings are different kernels, not readings of one.',
    'If different kernels, each constraint is standalone; if one kernel, the three stories form a constraint family linked by network.affects_constraints and the committer frame. Classification and coupling analysis differ accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Whether all three readings instantiate a common kernel or infer different foundational commitments.').

omega_variable(
    suppression_source_ambiguity,
    'Is suppression of reformist and traditionalist scholars structural (state coercion, detention, travel restrictions) or internalized (scholars self-censor from fear of regime reaction, or from fusion of their scholarly identity with regime-approved domains)?',
    'Post-exit trajectory analysis: if reformist and traditionalist scholars continue self-censoring after loss of regime access (e.g., in exile or after regime change), suppression was partially internalized. If suppression ceases when coercive mechanisms are removed, it was structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests. If structural only, the metric captures the full operative force. Internalization would indicate deeper identity-fusion with state authority among scholars.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_source_ambiguity, empirical, 'Whether suppression of disfavored doctrinal readings is coercive or internalized.').

omega_variable(
    instrumental_versus_principled_partition,
    'Does the state''s selection of which domains apply classical versus reformist law rest on a principled doctrinal position (e.g., ''family and criminal law require traditional sources; commerce requires maslaha-driven reasoning''), or is it pure instrumental partition (whichever framework serves regime interests in each domain)?',
    'Compare the state''s stated rationale for partition against the empirical pattern: does the pattern follow the stated principle consistently, or does it track regime political incentives (e.g., family law enforced classically when it shores up conservative support, relaxed when urban constituencies demand rights)? Regime statements about principled partition with evidence of instrumental drift indicates instrumental framing dressed as principle.',
    'If principled, the constraint is a coherent doctrinal reading defensible within Islamic jurisprudence—it would be tangled_rope but with legitimate coordination content. If instrumental, the constraint is pure extraction using doctrinal language as cover—it migrates toward snare classification and mandatrophy analysis becomes relevant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_versus_principled_partition, empirical, 'Whether domain-selectivity rests on defensible doctrinal criteria or pure regime interest.').

omega_variable(
    axiom_holding_status_ambiguity,
    'Does the state-hybrid reading genuinely hold the axioms it claims (e.g., ''state sovereignty grounds doctrinal selection,'' ''family law must rest on classical sources,'' ''commerce requires flexibility''), or has the state abandoned these axioms in practice while maintaining them rhetorically?',
    'Track instances of contradiction: does the state enforce classical family law even when doing so damages regime legitimacy or creates fiscal cost? Does it apply reformist commercial law consistently, or does it suspend it for regime-connected merchants? Evidence of systematic exception-making for regime insiders indicates abandoned axioms dressed in rhetorical commitment.',
    'If axioms are held, the constraint is a live reading with internal consistency. If abandoned, the constraint is piton-grade theater—the axioms are purely performative and the actual rule is ''whatever stabilizes regime power.'' Classification would shift from tangled_rope toward piton; theater_ratio would rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_holding_status_ambiguity, empirical, 'Whether state-hybrid axioms are genuinely held or maintained only rhetorically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.48).
narrative_ontology:measurement(qura_tr_t8, quran_hadith_substrate__state_hybrid, theater_ratio, 8, 0.54).
narrative_ontology:measurement(qura_tr_t16, quran_hadith_substrate__state_hybrid, theater_ratio, 16, 0.59).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__state_hybrid, theater_ratio, 24, 0.62).
narrative_ontology:measurement(qura_tr_t32, quran_hadith_substrate__state_hybrid, theater_ratio, 32, 0.61).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t8, quran_hadith_substrate__state_hybrid, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(qura_be_t16, quran_hadith_substrate__state_hybrid, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__state_hybrid, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(qura_be_t32, quran_hadith_substrate__state_hybrid, base_extractiveness, 32, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(qura_su_t8, quran_hadith_substrate__state_hybrid, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(qura_su_t16, quran_hadith_substrate__state_hybrid, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__state_hybrid, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(qura_su_t32, quran_hadith_substrate__state_hybrid, suppression_requirement, 32, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.18).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, quran_hadith_substrate__reformist_ijtihad).

% DUAL FORMULATION NOTE:
% The quran_hadith_substrate kernel decomposes into three constraint stories, each a reading of how Islamic jurisprudence should be deployed in modern states. The state-hybrid reading differs from traditionalist (which claims comprehensive taqlid obligation) and reformist (which claims contextual ijtihad mandate) by grounding legitimacy in political sovereignty rather than doctrinal coherence. ε values differ by magnitude: traditionalist reading has low ε (natural law character—taqlid is claimed as inevitable consensus); reformist reading has moderate ε (genuine coordination of rights and contextual reasoning, but contested); state-hybrid reading has moderate ε (solves coordination problem but extracts through instrumental partition). All three share the kernel (Qur'an/Hadith substrate) but infer different constraints from it. Network edges connect all three; upstream is traditionalist (most established), downstream is state-hybrid (most instrumentally extractive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__state_hybrid, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
