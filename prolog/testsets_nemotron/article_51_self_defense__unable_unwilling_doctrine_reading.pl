% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense — Unable/Unwilling Doctrine Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   The unable/unwilling doctrine emerged post-9/11 as a reading of Article
 *   51 that permits self-defense against non-state actors when the host state
 *   is unable or unwilling to suppress the threat. It presents itself as a
 *   coordination mechanism: a legal standard that balances state sovereignty
 *   with the reality of transnational non-state threats. But structurally, it
 *   operates as a hybrid constraint: it coordinates by providing a threshold
 *   (genuine coordination function — states want clarity on when cross-border
 *   force is lawful) while extracting by enabling powerful states to bypass
 *   weak states' sovereignty at will (asymmetric extraction). The doctrine's
 *   persistence depends on active enforcement — the military and intelligence
 *   apparatus that conducts strikes, the legal apparatus that produces the
 *   doctrine, and the diplomatic apparatus that suppresses alternatives. Host
 *   states are the primary victims (sovereignty bypassed), non-state actors
 *   are trapped targets, and civilians in strike zones are powerless victims.
 *   Intervening states and their counterterrorism institutions are the
 *   primary beneficiaries and agenda-setters. The constraint has drifted over
 *   2001-2024: extractiveness rose as the doctrine expanded from Afghanistan
 *   to Yemen, Somalia, Syria, Sahel; theater rose as legal justifications
 *   became more performative relative to operational necessity; suppression
 *   requirement rose as host state objections were increasingly ignored. The
 *   claimed type is tangled_rope — genuine coordination function (threshold
 *   clarity) AND asymmetric extraction (sovereignty bypass for the powerful).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense — Unable/Unwilling Doctrine Reading").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, '79c5edcb-fca1-4800-8cfb-37f4a9de1063').
narrative_ontology:cs_kernel_codification('79c5edcb-fca1-4800-8cfb-37f4a9de1063', fixed_text).
narrative_ontology:cs_authority_grounding('79c5edcb-fca1-4800-8cfb-37f4a9de1063', lineage).
narrative_ontology:cs_interpretation_layer_present('79c5edcb-fca1-4800-8cfb-37f4a9de1063').
narrative_ontology:cs_reading_relation('79c5edcb-fca1-4800-8cfb-37f4a9de1063', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('79c5edcb-fca1-4800-8cfb-37f4a9de1063', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('79c5edcb-fca1-4800-8cfb-37f4a9de1063', foundational, host_state_duty_to_suppress_non_state_threats).
narrative_ontology:cs_axiom_status(host_state_duty_to_suppress_non_state_threats, holdable).
narrative_ontology:cs_axiom_grounding('79c5edcb-fca1-4800-8cfb-37f4a9de1063', host_state_duty_to_suppress_non_state_threats, conventional).
narrative_ontology:cs_axiom('79c5edcb-fca1-4800-8cfb-37f4a9de1063', foundational, unable_unwilling_threshold_triggers_self_defense).
narrative_ontology:cs_axiom_status(unable_unwilling_threshold_triggers_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('79c5edcb-fca1-4800-8cfb-37f4a9de1063', unable_unwilling_threshold_triggers_self_defense, instrumental).
narrative_ontology:cs_reference_frame('79c5edcb-fca1-4800-8cfb-37f4a9de1063', article_51_textual_necessity_proportionality).
narrative_ontology:cs_drift_state('79c5edcb-fca1-4800-8cfb-37f4a9de1063', post_9_11_state_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('79c5edcb-fca1-4800-8cfb-37f4a9de1063', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars_supporting_doctrine).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, counterterrorism_institutions).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_sovereignty_bypassed).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actors_targeted_without_host_consent).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_target_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with global counterterrorism operations (primarily US, also UK, France, Israel, Turkey, Russia) that invoke the unable/unwilling doctrine to justify cross-border kinetic and non-kinetic strikes. They author the legal interpretation, control the intelligence apparatus that assesses 'unwillingness' and 'inability', and bear minimal political cost for civilian harm in target states. Their exit from the constraint is arbitrage-grade: they can shift to alternative legal justifications (collective self-defense, consent, Security Council authorization) or simply ignore the constraint when inconvenient.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates, agenda_setter,
    institutional, generational, arbitrage, global).

% States from whose territory non-state actors operate (Afghanistan 2001-2021, Pakistan, Yemen, Somalia, Syria, Iraq, Mali, Philippines, etc.). They lose territorial sovereignty and monopoly on force; face domestic political costs from civilian casualties; may be pressured to consent retroactively. Their exit is constrained: they can protest diplomatically, seek UNSC action (vetoed by intervening states), or attempt to suppress the non-state actors themselves — but lack capacity or face political constraints. Some (Pakistan, Yemen) have protested while privately acquiescing, creating identity-locked dynamics.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_sovereignty_bypassed, payer,
    moderate, biographical, constrained, national).

% Armed groups (Al-Qaeda, ISIS, Al-Shabaab, Houthis, Taliban, etc.) targeted under the doctrine. They bear the direct kinetic costs of the constraint. Their exit is trapped: they cannot appeal to international law, have no diplomatic channel, and face elimination or dispersion. The constraint's enforcement mechanism (drone strikes, special operations) is designed to deny them exit. Some embed in civilian populations, creating identity-locked dynamics for the civilians.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actors_targeted_without_host_consent, payer,
    organized, biographical, trapped, regional).

% Civilians in areas where unable/unwilling strikes occur (FATA Pakistan, Yemen, Somalia, Syria, etc.). Bear collateral harm, displacement, psychological trauma, and erosion of local governance. No exit from the strike zone; no voice in the legal framework; no remedy for harm. Their situation is the sharpest extraction point of the constraint — they pay in blood for a legal doctrine they cannot contest.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, civilian_populations_in_target_zones, payer,
    powerless, immediate, trapped, local).

% Scholars and institutions (certain Western law schools, think tanks, NATO COE) that produce the doctrinal architecture legitimizing the unable/unwilling test. They benefit professionally (citations, funding, policy access) from a reading that expands lawful force. Their exit is mobile: they can shift to other specializations, but the field's incentive structure rewards doctrinal innovation that enables state action.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_legal_scholars_supporting_doctrine, beneficiary,
    moderate, generational, mobile, global).

% Institutional complexes (JSOC, CIA, DGSE, MIT, GRU equivalents, Five Eyes CT fusion cells) that operationalize the doctrine. They gain mission authorization, budget, and bureaucratic permanence from a legal framework that permits unilateral action. Their exit is arbitrage-grade: the institutions would persist under any legal framework that authorizes counterterrorism operations; the doctrine is a convenience, not a necessity.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, counterterrorism_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% P5 states that both invoke the doctrine (US, UK, France, Russia) and guard the UNSC's primacy (China, Russia). They experience the constraint as an analytical observer of the tension between Article 51's text and state practice. Their analytical seat sees the full structure: the doctrine erodes the UNSC's exclusive Chapter VII authority while providing cover for great power intervention.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council_permanent_members, observer,
    institutional, generational, analytical, global).

% The ICJ has been structurally excluded from adjudicating unable/unwilling cases because intervening states do not accept its compulsory jurisdiction on use-of-force matters, and host states lack standing or political will to bring cases. The Court's Wall Advisory Opinion (2004) and Armed Activities judgment (2005) articulate the narrow reading; its exclusion from the doctrine's application is a structural feature, not an accident.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_court_of_justice, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_ct_mandates).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal threshold for when cross-border force against non-state actors is permissible without host state consent, replacing the binary of 'armed attack by a state' vs. 'no lawful force' with a graded standard tied to host state capacity and will.
% TRANSFER_FUNCTION: Transfers the burden of threat suppression from the host state (which is unable/unwilling) to the intervening state, and transfers the physical costs of that suppression (strikes, raids, civilian harm) to the host state's territory and population. The intervening state gains operational freedom; the host state loses territorial integrity; civilians bear the kinetic externalities.
% ABSENT_VOICES: The civilian populations in strike zones have no representation in the legal debate; host state legislatures and courts are bypassed by executive acquiescence; regional organizations (AU, Arab League, ASEAN) are consulted selectively; the UN General Assembly has no enforcement role.
% DISAPPEARANCE_RATIONALE: If the unable/unwilling doctrine vanished overnight, intervening states would lose their primary post-2001 legal justification for cross-border counterterrorism strikes. They would revert to narrower Article 51 readings (requiring state attribution), seek host state consent case-by-case, or rely on UNSC authorization — each option constraining operational tempo and scope. Host states would regain a stronger sovereignty claim. The global counterterrorism architecture would reorganize around consent and collective security mechanisms.
% FOUNDING_PROBLEM: After 9/11, the existing Article 51 framework required an 'armed attack' by a state, but the threat emanated from a non-state actor (Al-Qaeda) operating from a state (Afghanistan/Taliban) that harbored them but did not 'send' them in the traditional sense. The unable/unwilling doctrine was constructed to bridge this gap: permitting force against the non-state actor when the host state could not or would not act.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the intervening states (US DOJ white papers, NATO statements) as still live — non-state actor threats persist, host states still fail to suppress them. It is contested by the ICJ (Wall Opinion, Armed Activities), the UN Special Rapporteur on counterterrorism and human rights, the ICRC, and a substantial body of international legal scholarship (Cortin, Green, Lubell, etc.) who argue the founding problem was either mischaracterized or has been solved by state practice evolving toward consent-based frameworks, and that the doctrine now serves as a permissive frame for great power competition rather than a narrow necessity exception.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrine transfers substantial costs (sovereignty, civilian lives, legal stability) to host states and civilians while concentrating operational benefits on intervening states. Suppression (0.72) is high because the doctrine's persistence requires active exclusion of rival legal frameworks (narrow reading, ICJ jurisprudence, UNSC primacy) and physical enforcement (strikes that deny exit to targets). Theater (0.31) is moderate and rising — the 'unwilling/unable' assessment is increasingly a legal formality rather than a genuine factual inquiry. Accessibility collapse (0.42) is moderate: alternatives exist (consent, UNSC, narrow reading) but are structurally suppressed. Resistance (0.55) is significant: host state protests, ICJ jurisprudence, scholarly critique, and UNGA debates all contest the doctrine, but have not displaced it. The constraint requires active enforcement (military, legal, diplomatic) and has both beneficiaries and victims — the tangled_rope gate is satisfied.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening state seat (institutional, arbitrage exit), the constraint is genuine coordination: it solves the legal uncertainty of post-9/11 threats. From the host state seat (moderate, constrained exit), it is extraction: sovereignty is conditioned on capacity the host state may not have, and the assessment is controlled by the intervener. From the civilian seat (powerless, trapped), it is pure extraction with no coordination benefit. The engine computes these per-seat types from the structural data — this reading's claimed_type (tangled_rope) is the analytical seat's assessment of the overall structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and CT institutions are beneficiaries (d near 0.0) — they collect operational freedom, budget, and legal cover. Host states are payers (d near 0.8-0.9) — they bear sovereignty costs, civilian harm, and political instability with constrained exit. Non-state actors are trapped targets (d=1.0) — they bear kinetic elimination with no exit. Civilians are powerless victims (d=1.0) — they bear collateral harm with zero exit. Legal scholars are incidental beneficiaries (d~0.2) — professional gain from doctrinal work. The ICJ and UNGA are excluded observers — their structural position is analytical but they are denied adjudicative authority. Directionality overrides are not needed; the beneficiary/victim declarations plus exit options derive the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine was founded on a live problem (9/11-style transnational threat from non-state actors in failed/hostile states). That problem has evolved: non-state actors now operate from states with varying capacity/will; great powers invoke the doctrine against each other's proxies; the threshold has lowered. The mandate (narrow necessity exception) has atrophied into a broad permissive framework. The constraint persists not because the founding problem demands it, but because the beneficiaries (intervening states, CT institutions) have institutionalized it and the victims lack coalition power to displace it. This is mandatrophy: the coordination function has been captured by the extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_unwilling_natural_law_vs_constructed,
    'Does the unable/unwilling threshold reflect a genuine customary international law norm, or is it a constructed legal interpretation that benefits intervening states?',
    'Comparative analysis of state practice and opinio juris before and after 2001; ICJ advisory opinions; UNSC resolution voting patterns on cross-border counterterrorism operations.',
    'If genuine customary norm, extraction is lower and constraint approaches mountain-like coordination; if constructed, the constraint is a legitimating frame for power projection and extraction rises toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_unwilling_natural_law_vs_constructed, conceptual, 'Whether the unable/unwilling standard is customary law or a power-serving legal construction.').

omega_variable(
    host_state_consent_fiction,
    'Is the host state''s ''inability'' genuinely assessed, or is the unable/unwilling test a consent fiction that legitimizes violations of territorial sovereignty?',
    'Case-by-case analysis of interventions invoking the doctrine: correlation between host state capacity metrics and intervention decisions; diplomatic records of host state objections vs. acquiescence.',
    'If consent fiction, suppression is structural and extraction is higher (snare/tangled_rope); if genuine assessment, the constraint has a coordination function limiting arbitrary intervention (rope/tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(host_state_consent_fiction, empirical, 'Whether host state inability is genuinely evaluated or pretextual.').

omega_variable(
    kernel_reading_article_51_self_defense_unable_unwilling_doctrine,
    'This constraint is the unable/unwilling doctrine reading of the contested kernel ''article_51_self_defense''. What would the sibling readings (narrow_armed_attack_reading, expansive_preventive_reading) change structurally?',
    'Map each sibling reading''s beneficiary/victim structure, exit options for target states, and claimed_type against this reading''s structural profile.',
    'Narrow reading forecloses cross-border force against non-state actors without state attribution — would reclassify this constraint as snare from host state seat. Expansive reading removes the ''attack occurred'' threshold — would increase extraction and suppression, moving toward snare from intervening state seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_article_51_self_defense_unable_unwilling_doctrine, conceptual, 'Structural differences between this reading and its sibling readings of the article_51 kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(arti_tr_t2006, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(arti_tr_t2011, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2011, 0.22).
narrative_ontology:measurement(arti_tr_t2016, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2016, 0.27).
narrative_ontology:measurement(arti_tr_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(arti_tr_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2001, 0.35).
narrative_ontology:measurement(arti_be_t2006, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(arti_be_t2011, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2011, 0.48).
narrative_ontology:measurement(arti_be_t2016, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2016, 0.52).
narrative_ontology:measurement(arti_be_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2021, 0.56).
narrative_ontology:measurement(arti_be_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement(arti_su_t2006, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2006, 0.6).
narrative_ontology:measurement(arti_su_t2011, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2011, 0.65).
narrative_ontology:measurement(arti_su_t2016, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2016, 0.69).
narrative_ontology:measurement(arti_su_t2021, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement(arti_su_t2024, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.1).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, chapter_vii_security_council_authority).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, state_sovereignty_non_intervention_principle).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, international_humanitarian_law_civilian_protection).

% DUAL FORMULATION NOTE:
% This reading decomposes the article_51 kernel with narrow_armed_attack_reading and expansive_preventive_reading. The unable/unwilling reading has higher extractiveness (0.58) than the narrow reading (~0.2) because it permits cross-border force without state attribution, but lower than the expansive reading (~0.75) because it retains the 'attack occurred' threshold. The three readings form a constraint family linked by network.affects_constraints; each has distinct beneficiary/victim structures and claimed_types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
