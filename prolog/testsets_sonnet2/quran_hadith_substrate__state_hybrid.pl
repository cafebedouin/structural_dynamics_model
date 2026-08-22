% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__state_hybrid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: quran_hadith_substrate__state_hybrid
 *   human_readable: State-Selective Sharia Hybridization (Family/Criminal Classical, Commercial/Administrative Reformist)
 *   domain: religious_authority/legal_theory/political_legitimacy
 *
 * SUMMARY:
 *   This constraint models the state_hybrid reading of the Quran-Hadith
 *   substrate kernel: a governing arrangement in which the state selectively
 *   applies classical fiqh rulings in family law and criminal codes while
 *   adopting reformist or secularized frameworks in commercial and
 *   administrative law, with legitimacy ultimately grounded in political
 *   sovereignty rather than doctrinal fidelity to any single interpretive
 *   tradition. This is distinct from the traditionalist_taqlid reading (which
 *   holds the classical madhhab tradition binding across all domains) and the
 *   reformist_ijtihad reading (which mandates contextual reinterpretation
 *   across all domains, including family and criminal law). The state_hybrid
 *   reading is neither of these — it is the regime's pragmatic
 *   instrumentalization of BOTH traditions simultaneously, applying each
 *   where politically convenient and neither consistently.
 *
 * KEY AGENTS:
 *   - ruling_regime: agenda_setter (institutional/arbitrage) - draws the boundary between classical and reformist domains
 *   - state_legal_elites: beneficiary/agenda_setter (institutional/arbitrage) - administer and profit from the ambiguity
 *   - commercial_investor_class: beneficiary (powerful/mobile) - protected from classical commercial constraints
 *   - traditionalist_scholars: payer (moderate/constrained) - comprehensive doctrinal vision truncated
 *   - reformist_jurists: payer (moderate/trapped) - critical method coopted in commerce, denied in family/criminal law
 *   - women_under_classical_family_code: payer (powerless/trapped) - bear the direct cost of the classical selection
 *   - criminal_defendants_under_hudud_provisions: payer (powerless/trapped) - bear symbolic-disciplinary classical penalties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__state_hybrid, 0.38).
domain_priors:suppression_score(quran_hadith_substrate__state_hybrid, 0.55).
domain_priors:theater_ratio(quran_hadith_substrate__state_hybrid, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, extractiveness, 0.38).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quran_hadith_substrate__state_hybrid, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__state_hybrid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__state_hybrid, "State-Selective Sharia Hybridization (Family/Criminal Classical, Commercial/Administrative Reformist)").
narrative_ontology:topic_domain(quran_hadith_substrate__state_hybrid, "religious_authority/legal_theory/political_legitimacy").

domain_priors:requires_active_enforcement(quran_hadith_substrate__state_hybrid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__state_hybrid, 'd8eb5788-5586-4f60-ad21-4ae9595b95a7').
narrative_ontology:cs_kernel_codification('d8eb5788-5586-4f60-ad21-4ae9595b95a7', distributed).
narrative_ontology:cs_authority_grounding('d8eb5788-5586-4f60-ad21-4ae9595b95a7', extraction).
narrative_ontology:cs_interpretation_layer_present('d8eb5788-5586-4f60-ad21-4ae9595b95a7').
narrative_ontology:cs_reading_relation('d8eb5788-5586-4f60-ad21-4ae9595b95a7', quran_hadith_substrate__traditionalist_taqlid, influences).
narrative_ontology:cs_reading_relation('d8eb5788-5586-4f60-ad21-4ae9595b95a7', quran_hadith_substrate__reformist_ijtihad, influences).
narrative_ontology:cs_axiom('d8eb5788-5586-4f60-ad21-4ae9595b95a7', foundational, political_sovereignty_grounds_legal_authority).
narrative_ontology:cs_axiom_status(political_sovereignty_grounds_legal_authority, holdable).
narrative_ontology:cs_axiom_grounding('d8eb5788-5586-4f60-ad21-4ae9595b95a7', political_sovereignty_grounds_legal_authority, conventional).
narrative_ontology:cs_axiom('d8eb5788-5586-4f60-ad21-4ae9595b95a7', secondary, domain_specific_doctrinal_selection_is_legitimate_statecraft).
narrative_ontology:cs_axiom_status(domain_specific_doctrinal_selection_is_legitimate_statecraft, holdable).
narrative_ontology:cs_axiom_grounding('d8eb5788-5586-4f60-ad21-4ae9595b95a7', domain_specific_doctrinal_selection_is_legitimate_statecraft, instrumental).
narrative_ontology:cs_reference_frame('d8eb5788-5586-4f60-ad21-4ae9595b95a7', sovereign_discretionary_pluralism).
narrative_ontology:cs_drift_state('d8eb5788-5586-4f60-ad21-4ae9595b95a7', post_arab_spring_legitimacy_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8eb5788-5586-4f60-ad21-4ae9595b95a7', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__state_hybrid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, state_legal_elites).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, ruling_regime).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__state_hybrid, commercial_investor_class).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, reformist_jurists).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, women_under_classical_family_code).
narrative_ontology:constraint_victim(quran_hadith_substrate__state_hybrid, criminal_defendants_under_hudud_provisions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines which legal domains receive classical treatment and which receive reformist or secular treatment. Draws religious legitimacy from enforcing sharia visibly in family and criminal law, while shielding the commercial and administrative sectors that generate revenue and foreign investment from doctrinal constraints that would slow capital flows. Can recalibrate the split whenever political incentives shift, and answers to no external adjudicator of doctrinal consistency.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, ruling_regime, agenda_setter,
    institutional, generational, arbitrage, national).

% Judges, ministry jurists, and state muftis who administer the hybrid system. They gain career security, prestige, and discretionary interpretive power from the ambiguity of the split — they alone can declare which forum a given dispute belongs to, and profit from being indispensable arbiters of a system with no single doctrinal logic.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, state_legal_elites, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_hadith_substrate__state_hybrid, state_legal_elites, agenda_setter).

% Domestic and foreign capital operating under the secular/reformist commercial code. Benefits from predictable contract law, interest-bearing finance, and administrative procedure untouched by classical prohibitions (e.g., riba). Can exit to other jurisdictions if commercial law were reclassicized, which is precisely why it isn't.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, commercial_investor_class, beneficiary,
    powerful, biographical, mobile, global).

% Hold that sharia is a comprehensive, indivisible system; the state's compartmentalization amputates the doctrine to whatever is politically convenient, especially by exempting the lucrative commercial sphere. They lose authority to declare the whole legal order un-Islamic without being cast as agitators, and their comprehensive vision is structurally unrealizable within a hybrid state that treats classical fiqh as one input among several.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, traditionalist_scholars, payer,
    moderate, civilizational, constrained, national).

% Argue for contextual ijtihad and human-rights-consistent reinterpretation across ALL domains, including family and criminal law. The state's hybrid model coopts their commercial-law reasoning while refusing to extend it to family/criminal codes, where their critique would most directly threaten patriarchal and penal arrangements the regime uses for legitimacy theater. Publishing or litigating for full reform risks blasphemy exposure or professional exclusion; they cannot exit the jurisdiction without abandoning their public role.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, reformist_jurists, payer,
    moderate, biographical, trapped, national).

% Bear the direct cost of the split: divorce, custody, inheritance, and guardianship rules follow classical rulings precisely because that domain was selected for doctrinal 'authenticity,' while the commercial sphere touching the same regime's elites was reformed. Exit requires emigration or extralegal arrangements; formal legal recourse operates entirely inside the classical framework the state chose not to reform.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, women_under_classical_family_code, payer,
    powerless, biographical, trapped, national).

% Face classical criminal penalties (hudud, qisas) selectively retained for their symbolic and disciplinary value, even as administrative law around them has modernized. Cannot appeal to the reformist logic used elsewhere in the same legal system; the doctrinal inconsistency is invisible from inside their case.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, criminal_defendants_under_hudud_provisions, payer,
    powerless, immediate, trapped, national).

% Diaspora communities, transnational scholarly networks, and international human rights observers who track the coherence (or incoherence) of the state's sharia claims. They can publicize the doctrinal inconsistency but hold no formal power over domestic legal outcomes.
narrative_ontology:constraint_stakeholder(quran_hadith_substrate__state_hybrid, external_muslim_publics, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_hadith_substrate__state_hybrid, ruling_regime).
narrative_ontology:fixing_cost_class(quran_hadith_substrate__state_hybrid, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the regime a legible, dual-track legal order: religious legitimacy signaling through visible classical enforcement in family/criminal law, paired with predictable, investment-friendly commercial and administrative law needed for state revenue and international economic participation. Solves the regime's genuine problem of needing both religious authority and economic modernity simultaneously.
% TRANSFER_FUNCTION: Moves interpretive discretion and legitimacy credit to the ruling regime and its legal elites, while moving the doctrinal and material costs of that discretion onto whichever population sits in the domain selected for classical treatment — disproportionately women in family law and criminal defendants — and onto both traditionalist and reformist scholars, whose comprehensive positions are structurally excluded from ever governing the whole system.
% ABSENT_VOICES: Traditionalist scholars who would demand the commercial code also submit to classical fiqh, and reformist jurists who would extend contextual ijtihad into family and criminal law, are both structurally unable to shift the boundary the state has drawn — the boundary itself is drawn to exclude the version of consistency either side would insist on.
% DISAPPEARANCE_RATIONALE: If the selective hybridization dissolved overnight — forcing either full classical application or full reformist/secular application across all domains — the regime's dual legitimacy base would collapse: either the commercial sector would face immediate doctrinal constraints threatening investment and revenue, or the state's religious legitimacy claim before its domestic base would evaporate. Family and criminal law populations would experience an abrupt change in governing rules either way. The arrangement is load-bearing for the regime's survival strategy, not incidental to it.
% FOUNDING_PROBLEM: Post-colonial and post-independence states needed to reconcile popular demand for sharia-based legitimacy (often mobilized against colonial-era secular codes) with the practical requirement of participating in an international commercial and financial order built on non-classical contract, corporate, and banking law.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal scholarship on post-colonial Muslim-majority states (documenting near-uniform retention of classical family/penal codes alongside modernized commercial codes across otherwise very different regimes) corroborates the founding problem's persistence from outside the regimes themselves; however, the SAME scholarship is frequently produced by academics with reformist commitments, so independent traditionalist corroboration of the founding-problem framing (as opposed to their objection to its resolution) is thin — noted as a gap rather than resolved.
narrative_ontology:disappearance_verdict(quran_hadith_substrate__state_hybrid, world_rearranges).
narrative_ontology:founding_problem_status(quran_hadith_substrate__state_hybrid, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_hadith_substrate__state_hybrid, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_hadith_substrate__state_hybrid, 'none', 1).
narrative_ontology:epsilon_provenance(quran_hadith_substrate__state_hybrid, 0.38, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored at 0.38 (low-moderate, per the expected bin) because the hybrid arrangement genuinely solves a real coordination problem — reconciling religious legitimacy demand with commercial modernity — and is not primarily a rent-extraction vehicle in the way a pure snare would be. Suppression is moderate-to-rising (0.55 at T=60) because maintaining the split requires active policing of both traditionalist calls for full classicization and reformist calls for full ijtihad; the state must continuously suppress both boundary-challenges. Theater ratio rises to 0.48 because an increasing share of the state's doctrinal justification (claiming the split reflects 'authentic' sharia principles rather than administrative convenience) is performative rather than substantively grounded in consistent jurisprudential reasoning.
 *
 * PERSPECTIVAL GAP:
 *   From the ruling regime's seat, this looks like prudent statecraft — a rope solving a genuine dual-legitimacy problem. From the traditionalist and reformist seats, the same structure looks like extraction: their coherent doctrinal programs are permanently foreclosed from governing the whole legal order, and the state extracts legitimacy credit from a system that satisfies neither tradition on its own terms. From the position of women under the classical family code and criminal defendants under hudud provisions, the arrangement is not abstract at all — it is the direct, material determinant of their legal treatment, imposed without their having chosen which tradition would govern their case.
 *
 * DIRECTIONALITY LOGIC:
 *   The ruling regime and state legal elites sit at the low end of directionality — they set the boundary and collect discretionary authority and legitimacy from its ambiguity. The commercial investor class similarly benefits (mobile exit, low d) from the reformist carve-out that protects their transactions. Traditionalist scholars and reformist jurists sit at high d despite moderate power — their structural position is that of parties whose comprehensive programs are permanently blocked by the very existence of the split, even though neither is powerless in the way the family-code and hudud populations are. Women under classical family law and hudud defendants sit at the highest d: trapped exit, powerless, and directly subject to the domain the state selected for classical treatment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling religious legitimacy with commercial modernity) remains live in most state_hybrid contexts, which is why this reading resists a pure mandatrophy verdict — the coordination function has not simply outlived its use. However, the founding_problem_status is authored as 'live' rather than 'dead' precisely because the tension it manages (legitimacy vs. modernity) has not resolved; what has shifted is that the split increasingly serves the regime's own institutional survival independent of whether it still optimally solves the original tension, which is what the rising theater_ratio and suppression_requirement track.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_doctrinal_fidelity_ambiguity,
    'Is the state''s legitimacy claim over sharia genuinely grounded in political sovereignty as an independent source of religious authority, or is ''sovereignty'' itself a legitimating gloss that conceals an ad hoc, interest-driven selection process with no principled jurisprudential basis?',
    'Compare stated doctrinal justifications for domain selection (e.g., official commissions'' reasoning for retaining classical family law while reforming commercial law) against the revealed pattern of selection across multiple regimes and time periods — if the pattern tracks regime revenue and stability interests more closely than any consistent jurisprudential principle, sovereignty-grounding is better read as rationalization.',
    'If sovereignty-grounding is genuine, the coordination function is more robust and the classification leans toward tangled_rope with a real (if asymmetric) coordination core; if sovereignty-grounding is rationalization for interest-driven selection, the constraint drifts toward snare, since the coordination story becomes cover for extraction with no independent jurisprudential logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_doctrinal_fidelity_ambiguity, conceptual, 'Whether political-sovereignty legitimacy is a real independent ground or a legitimating gloss over ad hoc selection.').

omega_variable(
    kernel_reading_boundary_stability,
    'Where is the disagreement between this state_hybrid reading and its sibling readings (traditionalist_taqlid, reformist_ijtihad) actually located — is it about which rulings are correct, or about who has authority to decide domain boundaries at all?',
    'Trace historical instances where a regime''s domain-selection was challenged: did traditionalist and reformist critics contest the SUBSTANCE of a specific ruling, or did they contest the STATE''S AUTHORITY to draw the classical/reformist boundary in the first place? Court records, fatwa literature, and parliamentary debate transcripts would show which contestation dominates.',
    'If the disagreement is substantive (about specific rulings), the three readings are more like competing interpretive outputs of a shared authority structure. If the disagreement is about authority itself (who gets to decide domain boundaries), the state_hybrid reading represents a genuinely distinct claim about WHERE legitimacy sits (sovereignty vs. doctrinal lineage vs. contextual reasoning) rather than merely a different answer to the same question — this affects whether state_hybrid should be read as `influences` or `forecloses` relative to the siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, conceptual, 'Whether the kernel readings disagree on ruling substance or on the location of interpretive authority itself.').

omega_variable(
    variable_suppression_across_regimes,
    'Given the expected structural delta notes ''variable suppression depending on regime incentives,'' how much does the authored suppression value (0.55) actually generalize across different state_hybrid regimes, versus reflecting a specific illustrative regime type?',
    'Cross-national comparison of suppression intensity (censorship of traditionalist and reformist critique, prosecution of dissenting jurists) across multiple Muslim-majority states operating hybrid legal systems, coded against regime type (authoritarian vs. semi-democratic) and resource dependence (rentier vs. diversified economy).',
    'High variance would mean this single ε/suppression pairing understates the true range of the state_hybrid reading and might argue for splitting into regime-type-specific sub-stories in a future decomposition; low variance would validate treating state_hybrid as one coherent constraint across contexts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(variable_suppression_across_regimes, empirical, 'Whether authored suppression level generalizes across state_hybrid regime variants or masks high cross-regime variance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__state_hybrid, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_hadith_substrate__state_hybrid, theater_ratio, 0, 0.32).
narrative_ontology:measurement(qura_tr_t12, quran_hadith_substrate__state_hybrid, theater_ratio, 12, 0.36).
narrative_ontology:measurement(qura_tr_t24, quran_hadith_substrate__state_hybrid, theater_ratio, 24, 0.4).
narrative_ontology:measurement(qura_tr_t36, quran_hadith_substrate__state_hybrid, theater_ratio, 36, 0.43).
narrative_ontology:measurement(qura_tr_t48, quran_hadith_substrate__state_hybrid, theater_ratio, 48, 0.46).
narrative_ontology:measurement(qura_tr_t60, quran_hadith_substrate__state_hybrid, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_hadith_substrate__state_hybrid, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qura_be_t12, quran_hadith_substrate__state_hybrid, base_extractiveness, 12, 0.31).
narrative_ontology:measurement(qura_be_t24, quran_hadith_substrate__state_hybrid, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(qura_be_t36, quran_hadith_substrate__state_hybrid, base_extractiveness, 36, 0.35).
narrative_ontology:measurement(qura_be_t48, quran_hadith_substrate__state_hybrid, base_extractiveness, 48, 0.37).
narrative_ontology:measurement(qura_be_t60, quran_hadith_substrate__state_hybrid, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_hadith_substrate__state_hybrid, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t12, quran_hadith_substrate__state_hybrid, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(qura_su_t24, quran_hadith_substrate__state_hybrid, suppression_requirement, 24, 0.49).
narrative_ontology:measurement(qura_su_t36, quran_hadith_substrate__state_hybrid, suppression_requirement, 36, 0.51).
narrative_ontology:measurement(qura_su_t48, quran_hadith_substrate__state_hybrid, suppression_requirement, 48, 0.53).
narrative_ontology:measurement(qura_su_t60, quran_hadith_substrate__state_hybrid, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__state_hybrid, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_hadith_substrate__state_hybrid, 0.1).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, traditionalist_taqlid).
narrative_ontology:affects_constraint(quran_hadith_substrate__state_hybrid, reformist_ijtihad).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quran_hadith_substrate kernel. traditionalist_taqlid claims classical madhhab consensus binds all domains (ε reflects contested but doctrinally coherent authority). reformist_ijtihad claims contextual reinterpretation is mandated across all domains including family/criminal law (ε reflects a minority, actively suppressed critical position). state_hybrid (this file) claims neither wins outright — political sovereignty adjudicates domain-by-domain, producing the lowest ε of the three (0.25-0.45 band) because a genuine coordination function (reconciling legitimacy and commercial modernity) offsets what would otherwise be pure extraction. All three share the same underlying textual corpus (Quran and Hadith) as their contested kernel but instantiate structurally distinct constraints with distinct beneficiary/victim sets and distinct persistence mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
