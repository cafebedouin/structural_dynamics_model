% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Doctrine - Sovereignty Conditional on Protection
 *   domain: international_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) doctrine represents one reading of
 *   the tension between Article 2(7) of the UN Charter (prohibition on
 *   intervention in internal affairs) and Chapter VII (Security Council
 *   enforcement powers). Under the R2P reading instantiated here, sovereignty
 *   is conditional on protecting populations from systematic atrocity. When a
 *   state perpetrates or tolerates genocide, ethnic cleansing, or mass
 *   starvation, it forfeits the immunity that sovereignty normally provides,
 *   and the international community acquires a responsibility to intervene.
 *   This reading emerges from the atrocity-prevention failures of the 1990s
 *   and is advocated by human rights bodies, some powerful states, and
 *   international legal scholars. It conflicts with the sovereignty-first
 *   reading (the sibling constraint in this kernel), which treats sovereignty
 *   as foundational and intervention as requiring explicit consent or
 *   inter-state aggression triggers. This instantiation focuses on the R2P
 *   reading's own legitimating structure and extraction mechanics, not on the
 *   dispute between readings.
 *
 * KEY AGENTS:
 *   - persecuted_populations: powerless, trapped, immediate horizon — protection depends on external actors invoking R2P
 *   - targeted_states: institutional power, constrained exit, generational horizon — lose consent-based sovereignty immunity if atrocity threshold is crossed
 *   - intervening_powers: powerful institutional actors, arbitrage exit, biographical horizon — set practical thresholds and execute intervention, extracting legitimacy
 *   - international_human_rights_advocacy: organized, arbitrage exit, generational horizon — frames intervention as human rights defense, sets agenda for atrocity assessment
 *   - security_council: institutional, analytical, generational — nominally authorizes R2P interventions but deadlocked by veto power
 *   - traditional_sovereignty_norm: non-agent doctrinal entity — bears the cost of conditional redefinition under R2P
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.71).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Doctrine - Sovereignty Conditional on Protection").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, '1dc7a262-b656-4603-b3fe-258ee3d327a9').
narrative_ontology:cs_kernel_codification('1dc7a262-b656-4603-b3fe-258ee3d327a9', fixed_text).
narrative_ontology:cs_authority_grounding('1dc7a262-b656-4603-b3fe-258ee3d327a9', lineage).
narrative_ontology:cs_interpretation_layer_present('1dc7a262-b656-4603-b3fe-258ee3d327a9').
narrative_ontology:cs_reading_relation('1dc7a262-b656-4603-b3fe-258ee3d327a9', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('1dc7a262-b656-4603-b3fe-258ee3d327a9', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('1dc7a262-b656-4603-b3fe-258ee3d327a9', sovereignty_conditional_on_protection, deontological).
narrative_ontology:cs_axiom('1dc7a262-b656-4603-b3fe-258ee3d327a9', foundational, atrocity_prevention_primacy).
narrative_ontology:cs_axiom_status(atrocity_prevention_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1dc7a262-b656-4603-b3fe-258ee3d327a9', atrocity_prevention_primacy, deontological).
narrative_ontology:cs_reference_frame('1dc7a262-b656-4603-b3fe-258ee3d327a9', protection_conditional_sovereignty).
narrative_ontology:cs_drift_state('1dc7a262-b656-4603-b3fe-258ee3d327a9', contemporary_geopolitical_deployment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1dc7a262-b656-4603-b3fe-258ee3d327a9', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, international_human_rights_advocacy).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states_losing_consent_requirement).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, human_rights_supremacy_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, atrocity_prevention_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face systematic atrocity (genocide, ethnic cleansing, mass starvation) perpetrated or tolerated by their own state. Under R2P doctrine, their protection becomes a matter of international responsibility; intervention is justified by their claim to protection rather than by territorial sovereignty. They lack exit options and depend on external intervention for survival.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% States perpetrating or tolling systematic atrocities lose the consensual basis for sovereignty immunity. Under R2P, they can be intervened in without their consent or explicit Chapter VII authorization if internal atrocity crosses the threshold. They bear the cost of lost sovereignty shield and forced intervention.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states_losing_consent_requirement, payer,
    institutional, generational, constrained, national).

% Collects moral authority and institutional mandate from R2P doctrine. NGOs, UN bodies, and advocacy coalitions operate the doctrine as a legitimacy claim for emergency action and norm-setting. They frame intervention as human rights defense and set the agenda for when R2P fires.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_human_rights_advocacy, beneficiary,
    organized, generational, arbitrage, global).

% Powerful states and coalitions execute R2P interventions, claiming humanitarian mandate. They extract legitimacy from the doctrine (framing self-interest as protection) and set the practical threshold for when atrocity is deemed systematic enough. They decide unilaterally whether to act, despite R2P's formal UN oversight.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, intervening_powers, agenda_setter,
    powerful, biographical, arbitrage, global).

% The institutional doctrine that states are immune from external intervention absent consent or inter-state aggression. R2P doctrine conditions sovereignty on protection performance, which erodes the norm's unconditional character and substitutes a performance gate for a jurisdictional one.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm, payer,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, traditional_sovereignty_norm).

% States that lack military capacity or political standing to intervene. They are excluded from the decision to invoke R2P even though they remain bound by the doctrine's constraints on sovereignty. Smaller states and regional actors outside great-power coalitions have no say in when the doctrine activates.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, non_intervening_states, excluded,
    moderate, generational, constrained, global).

% Formally retains authorization power over R2P interventions (Chapter VII); however, permanent members' veto power and great-power disagreement mean the Council often deadlocks while atrocities continue. The Council's role is nominally authoritative but practically constrained by power asymmetry and political will.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, security_council, agenda_setter,
    institutional, generational, analytical, universal).

% Legal scholars and international relations analysts examine whether R2P interventions follow the doctrine's own stated criteria. They document threshold-setting gaps, selective application, and the doctrine's role in legitimizing intervention rationales that diverge from humanitarian intent.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an international responsibility mechanism to prevent and halt systematic atrocities when states are unwilling or unable to protect their own populations. Coordinates global capacity (military, humanitarian, investigative) around a shared harm-prevention standard rather than leaving protection entirely to state consent.
% TRANSFER_FUNCTION: Moves decision-making authority over state intervention from the state targeted to international actors (powerful intervening states, UN bodies, human rights advocates). Transfers sovereignty immunity conditioned on protection performance: a state that permits systematic atrocity loses the shield against intervention. This transfer operates asymmetrically: powerful states retain veto and enforce the doctrine selectively.
% ABSENT_VOICES: Persecuted populations themselves are never at the table where R2P is invoked; intervention decisions happen in UN chambers and foreign capitals, not in affected communities. States that would counsel caution (especially non-intervening regional powers and smaller states) are absent from the enforcement decision. Victims of false-positive interventions (civilians harmed in military action justified by R2P) have no voice in threshold-setting.
% DISAPPEARANCE_RATIONALE: If R2P doctrine disappeared, the international legal framework would revert to unqualified state sovereignty and explicit consent/Chapter VII authorization as the only lawful intervention triggers. States perpetrating internal atrocities would regain full immunity. Humanitarian intervention would persist (powerful states act unilaterally) but lose the legitimacy doctrine R2P provides, surfacing naked power as the actual arbiter instead of hiding it behind protection rationale.
% FOUNDING_PROBLEM: The gap between state obligations to protect populations and the traditional sovereignty rule that bars intervention in internal affairs. Rwanda (1994) and Bosnia (1992-95) exposed the contradiction: systematic atrocities happening within a state's jurisdiction while the state itself perpetrated them, and the international order had no doctrine permitting emergency action short of state consent or inter-state war.
% FOUNDING_PROBLEM_CORROBORATION: UN Secretariat and human rights bodies attest the founding problem persists (documented in Secretary-General reports on R2P). However, scholars of international law and peace studies document that R2P has been invoked selectively and often as post-hoc legitimation for interventions driven by geopolitical interests (Syria, Libya cases); the founding problem's continued severity is disputed by those who note that atrocity-prevention mechanisms outside R2P (ICC, sanctions, peacekeeping) address portions of the gap. The doctrine's own advocates and skeptics coexist in the international legal community.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 (end of interval) because R2P doctrine legitimizes asymmetric intervention authority: powerful states interpret and enforce the doctrine without binding themselves equally to its standards (Libya 2011 was humanitarian intervention; Syria is geopolitical conflict, despite comparable atrocity scales). Suppression is 0.71 because the doctrine requires suppressing the traditional sovereignty rule (state consent) and the option to refuse intervention. Theater_ratio is 0.42 because a real coordination function (atrocity prevention) coexists with performative use of the doctrine as post-hoc legitimation for power-driven interventions. The measurement series shows extractiveness and suppression rising over the interval (from ~0.51 to 0.68 and ~0.58 to 0.71, respectively) as R2P invocations accumulate and the doctrine's selective application becomes clearer. Theater rises from 0.25 to 0.42 as the gap between stated atrocity-prevention purpose and actual geopolitical deployment widens. Accessibility_collapse is 0.61 because once R2P doctrine is established, states cannot exit the frame by claiming absolute sovereignty — the doctrine has become part of international law, and states must argue within it or against it explicitly (they cannot ignore it). Resistance is high (0.79) because powerful states resist R2P application against their allies, non-intervening states resist the unequal enforcement burden, and scholars dispute the doctrine's legal and moral foundations.
 *
 * PERSPECTIVAL GAP:
 *   From the persecuted_populations' and international_human_rights_advocates' seats, R2P is genuine coordination: it fills a gap in traditional sovereignty doctrine and provides a mechanism for protection when states fail. From the targeted_states' seat (especially those not aligned with intervening powers), R2P is pure extraction: it strips their sovereignty shield and submits them to intervention by more powerful actors. From intervening_powers' seats, R2P is coordination-with-extraction: they benefit from legitimacy for actions they would take anyway, and they set the thresholds unilaterally. From non-intervening states' seats, R2P is exclusionary: they bear the constraints (cannot claim absolute sovereignty) but have no voice in enforcement decisions. The engine computes these divergences from the structural data — different power levels, exit options, and beneficiary/victim positions produce different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (persecuted_populations, international_human_rights_advocacy) derive low directionality (0.1–0.2) because they benefit from the doctrine and have no suppressed alternatives to exit to. Victims (targeted_states, traditional_sovereignty_norm) derive high directionality (0.75–0.9) because they bear extraction and suppression of their prior immunity. Intervening_powers split: they are agenda-setters (control enforcement), so their d is near 0.4–0.5 (symmetric or slightly extractive); their arbitrage exit prevents full target-lock. Non-intervening_states are constrained payers (excluded from decisions but bound by the norm), d near 0.65–0.75. The ascending extractiveness in measurements reflects the doctrine's increasing use to legitimize selective interventions, which raises the extraction component relative to the genuine coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state inability to protect populations from internal atrocities) remains live in the world (conflicts in Syria, Yemen, Myanmar document ongoing atrocities). However, R2P invocation has not tracked atrocity severity uniformly — Libya (2011) triggered intervention; Syria did not despite comparable death tolls. The mismatch between founding_problem_status=live and selective disappearance_verdict=world_rearranges (intervention is selective, not universal) creates a mandatrophy signal: the doctrine was founded to prevent atrocities but operates as a selective legitimacy tool. If interventions were applied uniformly to all atrocities, the founding problem would have some purchase. The selective application suggests the founding purpose has been partially obsoleted by geopolitical deployment — a mandatrophy situation where the doctrine persists not because it solves the problem it claims to solve, but because it provides legitimacy for power-driven action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_ambiguity,
    'What constitutes systematic atrocity sufficient to trigger R2P responsibility? Is the threshold cross-culturally defined or imposed by intervening powers?',
    'Comparison of R2P invocations and non-invocations against standardized atrocity metrics (death toll, displacement, intent documentation); analysis of who sets the threshold (Security Council votes, individual power declarations, UN bodies).',
    'If thresholds are power-relative (atrocities by allied governments tolerated while enemies are sanctioned), the doctrine becomes a disguised extraction mechanism for powerful states. If thresholds are objective, the doctrine''s extraction is lower (genuine coordination on harm prevention).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_threshold_ambiguity, empirical, 'Whether R2P threshold for intervention is universally applied or selectively deployed by powerful states.').

omega_variable(
    kernel_reading_contest,
    'Does R2P doctrine fundamentally revise the UN Charter''s Article 2(7) prohibition on intervention, or does it represent an interpretation consistent with existing sovereignty-conditional-on-protection language?',
    'Textual analysis of Article 2(7) and the International Commission on Intervention and State Sovereignty (ICISS) framing; examination of whether R2P advocates claim they are revising the Charter or interpreting its latent intent.',
    'If R2P is a genuine revision, it legitimizes reshaping sovereignty rules without formal amendment — extraction for those setting the new interpretation. If R2P is an interpretation, it does not rewrite the law but only clarifies a protection condition already latent in the Charter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether R2P is a revision of the UN Charter''s sovereignty rule or an interpretation of its latent terms.').

omega_variable(
    powerful_state_agenda_capture,
    'To what extent do intervening powers use R2P doctrine to legitimize interventions driven by geopolitical interest rather than atrocity prevention?',
    'Pattern analysis of R2P invocations matched against: (a) timing relative to geopolitical shifts, (b) comparative invocation rates for same-magnitude atrocities by allies vs. adversaries, (c) mission creep in stated intervention objectives once intervention begins, (d) post-intervention burden-sharing (who bears humanitarian costs vs. extraction gains).',
    'High correlation between geopolitical interest and R2P invocation would establish the doctrine as a disguised power mechanism (snare masquerading as rope). Low correlation would support genuine coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(powerful_state_agenda_capture, empirical, 'Whether R2P operates as atrocity prevention or as legitimacy cover for power-driven intervention.').

omega_variable(
    sovereignty_norm_degradation_pace,
    'Is the traditional sovereignty norm being eroded by R2P, or is it being preserved by redefining sovereignty as conditional rather than absolute?',
    'Institutional analysis of state behavior: do states increasingly accept conditional sovereignty language, or do they resist and re-assert unconditional sovereignty? Survey of new treaty language and state practice post-R2P codification.',
    'If norm erosion is real, R2P is extraction (rewriting the rules that protected state autonomy). If sovereignty is preserved (only redefined), R2P is genuine coordination (clarifying what sovereignty entails).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_norm_degradation_pace, empirical, 'Whether R2P erodes or redefines the traditional sovereignty norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t5, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(arti_tr_t5, observed).
narrative_ontology:measurement(arti_tr_t10, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(arti_tr_t10, observed).
narrative_ontology:measurement(arti_tr_t15, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(arti_tr_t15, observed).
narrative_ontology:measurement(arti_tr_t20, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(arti_tr_t20, observed).
narrative_ontology:measurement(arti_tr_t25, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(arti_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t5, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(arti_be_t5, observed).
narrative_ontology:measurement(arti_be_t10, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(arti_be_t10, observed).
narrative_ontology:measurement(arti_be_t15, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(arti_be_t15, observed).
narrative_ontology:measurement(arti_be_t20, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(arti_be_t20, observed).
narrative_ontology:measurement(arti_be_t25, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(arti_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t5, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(arti_su_t5, observed).
narrative_ontology:measurement(arti_su_t10, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(arti_su_t10, observed).
narrative_ontology:measurement(arti_su_t15, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(arti_su_t15, observed).
narrative_ontology:measurement(arti_su_t20, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(arti_su_t20, observed).
narrative_ontology:measurement(arti_su_t25, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(arti_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__r2p_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension__sovereignty_first_reading).

% DUAL FORMULATION NOTE:
% This story instantiates the R2P reading of the article_2_7_chapter_vii_tension kernel. The sibling story article_2_7_chapter_vii_tension__sovereignty_first_reading instantiates the sovereignty-first reading of the same kernel. The two readings diverge on the foundational question of whether sovereignty is conditional on protection or foundational and unconditional. Both readings interpret the same Charter text (Articles 2(7) and Chapter VII) but arrive at opposed conclusions about what the Charter commits to. Neither reading can be falsified by pure textual analysis; the contest is resolved through practice and institutional authority. Link the two stories via network.affects_constraints to indicate kernel kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
