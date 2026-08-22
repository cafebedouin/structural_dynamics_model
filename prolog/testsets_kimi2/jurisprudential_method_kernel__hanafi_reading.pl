% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Analogical Extension and Juristic Preference
 *   domain: legal/religious/institutional
 *
 * SUMMARY:
 *   The Hanafi school of Islamic jurisprudence holds that divine law extends
 *   beyond the literal text of Qur'an and Hadith through analogical reasoning
 *   (qiyas) and juristic preference (istihsan), making human reason a
 *   legitimate instrument for deriving rulings on novel cases. Founded in the
 *   8th century under the administrative pressures of imperial expansion, the
 *   method became the official jurisprudence of the Ottoman and Mughal
 *   empires, embedding a trained jurist class as the necessary intermediary
 *   between scripture and society. The constraint coordinates legal responses
 *   to unprecedented situationsâcommercial instruments, administrative
 *   disputes, technological changeâbut simultaneously concentrates
 *   interpretive authority in a rationalist scholarly elite, marginalizing
 *   textualist communities that assert direct, unmediated scriptural access.
 *   The kernel is contested: the Hanafi reading is one of four major readings
 *   of the same jurisprudential_method_kernel, differing from the Maliki
 *   emphasis on Medinan practice, the Shafii standardization of hadith
 *   transmission, and the Hanbali rejection of analogical reasoning as
 *   innovation. This story authors the Hanafi reading as a tangled rope:
 *   genuine coordination layered with asymmetric extraction.
 *
 * KEY AGENTS:
 *   - hanafi_jurist_class: Primary agenda-setter and beneficiary (institutional/arbitrage) â administers analogical method and collects interpretive authority
 *   - textualist_communities: Primary target (moderate/constrained) â bears exclusion from direct textual authenticity and institutional voice
 *   - lay_muslims: Secondary target (powerless/constrained) â bears mediated access to legal guidance and loss of autonomous interpretation
 *   - ruling_authorities: Secondary beneficiary (powerful/mobile) â gains bureaucratic stability from professional jurist class
 *   - other_madhhab_jurists: Excluded seat (organized/constrained) â structurally barred from official posts in Hanafi-dominated jurisdictions
 *   - modern_reformists: Analytical observer (moderate/mobile) â evaluates method against direct ijtihad and secular legal ideals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.72).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Analogical Extension and Juristic Preference").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "legal/religious/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'c15db253-dae1-4463-aab0-78435cf912c8').
narrative_ontology:cs_kernel_codification('c15db253-dae1-4463-aab0-78435cf912c8', fixed_text).
narrative_ontology:cs_authority_grounding('c15db253-dae1-4463-aab0-78435cf912c8', lineage).
narrative_ontology:cs_interpretation_layer_present('c15db253-dae1-4463-aab0-78435cf912c8').
narrative_ontology:cs_reading_relation('c15db253-dae1-4463-aab0-78435cf912c8', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('c15db253-dae1-4463-aab0-78435cf912c8', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('c15db253-dae1-4463-aab0-78435cf912c8', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('c15db253-dae1-4463-aab0-78435cf912c8', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('c15db253-dae1-4463-aab0-78435cf912c8', reason_extends_divine_intent, theological).
narrative_ontology:cs_axiom('c15db253-dae1-4463-aab0-78435cf912c8', foundational, juristic_preference_valid_corrective).
narrative_ontology:cs_axiom_status(juristic_preference_valid_corrective, holdable).
narrative_ontology:cs_axiom_grounding('c15db253-dae1-4463-aab0-78435cf912c8', juristic_preference_valid_corrective, conventional).
narrative_ontology:cs_reference_frame('c15db253-dae1-4463-aab0-78435cf912c8', rationalist_juristic_framework).
narrative_ontology:cs_drift_state('c15db253-dae1-4463-aab0-78435cf912c8', contemporary_textualist_resurgence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c15db253-dae1-4463-aab0-78435cf912c8', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, ruling_authorities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_communities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained in rationalist legal methodology (qiyas, istihsan); author fatwas, teach in madrasas, and serve as judges. Their authority depends on the legitimacy of extending divine law through reason; they are the necessary intermediaries for novel cases and move between posts, courts, and teaching positions across the Islamic world.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, hanafi_jurist_class, beneficiary).

% Assert that law derives exclusively from the literal text of Qur'an and Hadith without analogical extension. Marginalized within Hanafi-dominated institutions; their scholars are excluded from state fatwa councils and judicial appointments when they reject qiyas. Experience the constraint as a barrier to direct textual authenticity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_communities, payer,
    moderate, biographical, constrained, national).

% Endorse the Hanafi school as state madhhab to secure a professional judiciary capable of ruling on novel commercial, administrative, and criminal cases through consistent analogical reasoning. Benefit from stability and predictability, though they could theoretically switch madhhabs or secularize.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, ruling_authorities, beneficiary,
    powerful, generational, mobile, national).

% Must rely on Hanafi jurists for rulings on novel matters not explicitly covered by sacred text. Their direct access to divine guidance is mediated by the juristic class; independent analogical reasoning by laypeople lacks institutional legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_muslims, payer,
    powerless, biographical, constrained, national).

% Jurists of Maliki, Shafii, or Hanbali schools whose methodologies differ on source hierarchy and the validity of istihsan. Excluded from official judicial posts in Hanafi-dominated states and their legal opinions are not enforced by the state apparatus.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, other_madhhab_jurists, excluded,
    organized, generational, constrained, regional).

% Modernist Muslims and legal reformers who argue for direct ijtihad or bypassing classical madhhab constraints. They observe the Hanafi method as an institutional bottleneck but lack power to displace it in traditional jurisdictions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, modern_reformists, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_jurist_class).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends divine legal guidance to novel casesânew technologies, commercial instruments, territorial expansionâfor which Qur'an and Hadith provide no explicit ruling, through a trainable, repeatable methodology that scales across imperial administration.
% TRANSFER_FUNCTION: Moves interpretive authority from the textual sources and lay readers to the class of trained rationalist jurists; moves legitimacy from literal-text-exclusive claims to analogically-extended rulings validated by the juristic hierarchy.
% ABSENT_VOICES: Textualist scholars who reject qiyas as illegitimate innovation, and lay Muslims who believe they can derive law directly from scripture without jurist mediation, are structurally excluded from Hanafi institutional fatwa and judiciary apparatus. Their absence from the agenda-setting table creates the appearance of scholarly consensus.
% DISAPPEARANCE_RATIONALE: If the Hanafi method vanished overnight, the trained juristic class would lose its coordinative monopoly on novel cases; state legal bureaucracies built around madhhab-trained judges would face jurisdictional chaos or forced reconstitution around another school. Textualist communities would gain immediate interpretive legitimacy, and the landscape of Islamic legal authority would fragment or reorganize around direct-text adjudication.
% FOUNDING_PROBLEM: The rapid expansion of the Islamic empire in the 7th-8th centuries created vast territories and novel situationsânew contracts, cultures, administrative problemsânot explicitly addressed by Qur'an and Medinan practice, requiring a scalable method to derive lawful rulings.
% FOUNDING_PROBLEM_CORROBORATION: Historical chroniclers and modern historians of Islamic law attest to the administrative pressures of the Umayyad and early Abbasid periods. Ottoman and Mughal state archives corroborate adoption of the Hanafi method for imperial scalability. Textualist critics from outside the beneficiary classâIbn Taymiyya, the Wahhabi movement, and modern Salafi juristsâcorroborate that the problem of novel cases existed but argue the solution was unnecessary because scripture and sound hadith suffice.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the analogical method makes trained jurists the necessary gatekeepers for any case without explicit textual precedent, extracting interpretive autonomy from lay Muslims and textualists. Suppression (0.68) reflects active institutional exclusion of text-only adjudication and state enforcement of madhhab boundaries. Theater ratio (0.28) captures moderate ritualization: much qiyas reasoning is functional, but a significant share of juristic activity maintains the performative authority of the method rather than solving novel coordination problems. Accessibility collapse (0.75) is high because once the Hanafi framework is accepted, direct-text alternatives appear jurisprudentially illegitimate for complex cases. Resistance (0.55) registers persistent textualist movementsâfrom Ibn Taymiyya to contemporary Salafismâthat reject jurist mediation. The measurement series share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The Hanafi jurist seat experiences the constraint as indispensable coordination: without qiyas, the empire cannot administer justice on novel matters. The textualist and lay payer seats experience the same structure as an artificial monopoly that interposes a human interpretive class between the believer and divine guidance. The engine computes this divergence from the structural dataâbeneficiary/victim declarations, exit options, and power levelsâwithout requiring the author to reconcile the frames.
 *
 * DIRECTIONALITY LOGIC:
 *   The hanafi_jurist_class and ruling_authorities are declared beneficiaries with mobile or arbitrage exit, placing them at the low-d (beneficiary) end of the directionality spectrum; their effective extraction is damped or inverted into subsidy. The textualist_communities and lay_muslims are declared victims with constrained exit, placing them at the high-d (target) end; their effective extraction is amplified. The high spatial scope (national to global) further amplifies target extraction because verification of analogical soundness becomes harder across vast imperial distances.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâimperial expansion producing novel cases without explicit textual guidanceâremains live across the interval, as new technologies and commercial forms continue to arise. Because the problem persists and the beneficiary class (jurists and ruling authorities) actively maintains the method, the constraint is not a piton. The coordination function is genuine: qiyas produces real rulings that enable scalable administration. The extraction is asymmetric: the same structure that coordinates also empowers a specific professional class and suppresses direct-text alternatives. The mandatrophy analysis prevents mislabeling this as pure extraction (snare) because the coordination would collapse without the method, and prevents mislabeling it as pure coordination (rope) because the victim set is structurally necessary to the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_legitimacy_ambiguity,
    'Is analogical reasoning (qiyas) an intrinsic feature of divine-law jurisprudence, or an artifact of the jurist-class institutionalization that empowers a specific professional group?',
    'Comparative historical analysis of legal systems: if non-jurist societies solve novel-case problems without analogous priestly or scholarly mediation, the Hanafi method is more constructed than natural.',
    'If constructed, the high extractiveness is rent captured by the jurist class; if intrinsic, a portion of the measured extraction is the necessary cost of legitimate legal extension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_legitimacy_ambiguity, conceptual, 'Ambiguity over natural vs constructed status of analogical reasoning').

omega_variable(
    state_enforcement_vs_voluntary_adoption,
    'Does the Hanafi school''s historical dominance reflect voluntary scholarly consensus on methodological superiority, or imperial enforcement that suppressed alternative madhhabs and textualist readings?',
    'Archival analysis of judicial appointments, endowment records, and state sponsorship patterns across the Ottoman, Mughal, and Safavid empires.',
    'If dominance was primarily state-coerced, suppression is higher than the doctrinal story suggests and the coordination function is partly cover for imperial control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_vs_voluntary_adoption, empirical, 'State enforcement vs voluntary scholarly adoption of Hanafi method').

omega_variable(
    textualist_exclusion_mechanism,
    'Are textualist communities excluded by structural state barriers (appointment vetoes, curriculum control) or by internalized epistemic deference to the juristic class?',
    'Post-exit trajectory: in jurisdictions that secularized or switched madhhabs, do textualist communities reassert direct-text authority immediately (structural exclusion) or remain deferential (internalized suppression)?',
    'If internalized, effective suppression exceeds the structural measure; the constraint operates partly through cognitive capture rather than institutional gatekeeping alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_exclusion_mechanism, empirical, 'Structural vs internalized suppression of textualist readings').

omega_variable(
    sibling_reading_delta,
    'What would change structurally if the Hanbali readingâwhere analogical reasoning is bid''ahâwere adopted as the sole framework?',
    'Comparative institutional analysis of Hanbali-dominated polities versus Hanafi-dominated ones regarding judicial training length, novel-case backlog, and lay direct-text activism.',
    'Abolition of trained-jurist mediation would collapse the extraction seat but potentially create coordination chaos in complex commercial and administrative law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Structural delta between Hanafi and Hanbali kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanafi_jurist_method_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hanafi_jurist_method_tr_t250, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 250, 0.15).
narrative_ontology:measurement(hanafi_jurist_method_tr_t500, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 500, 0.22).
narrative_ontology:measurement(hanafi_jurist_method_tr_t750, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 750, 0.3).
narrative_ontology:measurement(hanafi_jurist_method_tr_t1000, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1000, 0.35).
narrative_ontology:measurement(hanafi_jurist_method_tr_t1250, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1250, 0.28).

% Extraction over time
narrative_ontology:measurement(hanafi_jurist_method_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(hanafi_jurist_method_be_t250, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 250, 0.6).
narrative_ontology:measurement(hanafi_jurist_method_be_t500, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 500, 0.68).
narrative_ontology:measurement(hanafi_jurist_method_be_t750, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 750, 0.78).
narrative_ontology:measurement(hanafi_jurist_method_be_t1000, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1000, 0.72).
narrative_ontology:measurement(hanafi_jurist_method_be_t1250, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1250, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(hanafi_jurist_method_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hanafi_jurist_method_su_t250, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 250, 0.58).
narrative_ontology:measurement(hanafi_jurist_method_su_t500, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 500, 0.65).
narrative_ontology:measurement(hanafi_jurist_method_su_t750, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 750, 0.82).
narrative_ontology:measurement(hanafi_jurist_method_su_t1000, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1000, 0.75).
narrative_ontology:measurement(hanafi_jurist_method_su_t1250, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1250, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jurisprudential_method_kernel. Sibling readings (maliki, shafii, hanbali) instantiate structurally distinct constraints from the same kernel. Decomposition follows the Îµ-invariance principle: the Hanafi reading's high Îµ on analogical extension differs from the Hanbali reading's near-zero Îµ (text-only), and from the Shafii reading's moderate Îµ (standardized hadith arbitration). Each reading has its own constraint_id and is linked conceptually as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
