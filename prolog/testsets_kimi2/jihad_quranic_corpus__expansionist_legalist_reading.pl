% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Jihad as Obligation to Establish Islamic Governance (Expansionist Legalist Reading)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This constraint story instantiates the expansionist legalist reading of
 *   the jihad quranic corpus kernel. It treats jihad as a collective
 *   obligation (fard kifaya) under caliphal authority to extend Islamic
 *   governance to territories not under Muslim rule, subject to classical
 *   juridical conditions: prior invitation to Islam, legitimate imam
 *   authorization, and proportionality in conduct. Non-Muslims who reject the
 *   invitation face combat or subordinate dhimmi status with jizya. This
 *   reading is structurally distinct from the defensive-spiritual reading
 *   (which restricts armed jihad to defense and internal moral struggle) and
 *   the revolutionary-vanguard reading (which individualizes obligation
 *   against apostate rulers via takfir). The core is a commitment system
 *   grounding territorial expansion in fixed textual authority, mediated by a
 *   juristic interpretation layer that both regulates and legitimates
 *   extraction.
 *
 * KEY AGENTS:
 *   - caliphal_authority: agenda_setter (institutional/constrained) â directs campaigns and captures revenue
 *   - muslim_polity: beneficiary (organized/identity_locked) â receives security and redistribution
 *   - non_muslim_target_populations: payer (powerless/trapped) â bears subordination and tribute
 *   - islamic_jurists: beneficiary (organized/identity_locked) â derives authority from interpretive monopoly
 *   - internal_dissenters: excluded (moderate/constrained) â marginalized defensive-reading scholars
 *   - rival_non_muslim_polities: excluded (powerful/constrained) â targets with no voice in the legal framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.76).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.74).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad as Obligation to Establish Islamic Governance (Expansionist Legalist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'f62f856d-5cd8-452b-8c1c-91fea97d4d35').
narrative_ontology:cs_kernel_codification('f62f856d-5cd8-452b-8c1c-91fea97d4d35', fixed_text).
narrative_ontology:cs_authority_grounding('f62f856d-5cd8-452b-8c1c-91fea97d4d35', lineage).
narrative_ontology:cs_interpretation_layer_present('f62f856d-5cd8-452b-8c1c-91fea97d4d35').
narrative_ontology:cs_reading_relation('f62f856d-5cd8-452b-8c1c-91fea97d4d35', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('f62f856d-5cd8-452b-8c1c-91fea97d4d35', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('f62f856d-5cd8-452b-8c1c-91fea97d4d35', foundational, offensive_jihad_permitted_under_legitimate_imam).
narrative_ontology:cs_axiom_status(offensive_jihad_permitted_under_legitimate_imam, holdable).
narrative_ontology:cs_axiom_grounding('f62f856d-5cd8-452b-8c1c-91fea97d4d35', offensive_jihad_permitted_under_legitimate_imam, theological).
narrative_ontology:cs_axiom('f62f856d-5cd8-452b-8c1c-91fea97d4d35', foundational, non_muslim_liminal_subordination_legitimate).
narrative_ontology:cs_axiom_status(non_muslim_liminal_subordination_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f62f856d-5cd8-452b-8c1c-91fea97d4d35', non_muslim_liminal_subordination_legitimate, theological).
narrative_ontology:cs_reference_frame('f62f856d-5cd8-452b-8c1c-91fea97d4d35', classical_caliphal_expansionism).
narrative_ontology:cs_drift_state('f62f856d-5cd8-452b-8c1c-91fea97d4d35', post_westphalian_nation_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f62f856d-5cd8-452b-8c1c-91fea97d4d35', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_polity).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_target_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monopolizes the declaration of jihad under classical fiqh, directs military campaigns, administers conquered territories, and receives spoils and jizya. Bound by the juridical conditions of prior invitation, legitimate leadership, and proportionality, but interprets these conditions.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from territorial expansion, security perimeter enlargement, and redistribution of conquest revenues. Religious identity is fused with political supremacy; exit from the obligation framework is doctrinally unthinkable for the community as a whole.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_polity, beneficiary,
    organized, generational, identity_locked, global).

% Receive invitation to accept Islam; upon refusal face military campaign or subordinate dhimmi status with jizya payment. Cannot exit the framework except by conversion, submission, or successful resistance. Political sovereignty and equal standing are structurally inaccessible outcomes.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_target_populations, payer,
    powerless, immediate, trapped, regional).

% Derive professional and religious authority from elaborating the conditions of legitimate jihad (imam authority, proportionality, invitation). Their interpretive labor simultaneously regulates violence and legitimates expansion; departure from the classical framework would collapse their epistemic role.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_jurists, beneficiary,
    organized, generational, identity_locked, continental).

% Scholars and movements arguing for purely defensive or spiritual readings of jihad. Marginalized within classical fiqh curricula and institutional authority, but present historically and contemporarily.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, internal_dissenters, excluded,
    moderate, biographical, constrained, national).

% States and empires targeted by expansionist campaigns. Excluded from the legal framework's protections except through temporary treaty (sulh) or submission; their resistance is treated as belligerency rather than legitimate self-defense within the reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, rival_non_muslim_polities, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels tribal and military energy into state-directed expansion rather than anarchic raiding, and provides a unified legal framework for governing diverse conquered populations under a single caliphal authority.
% TRANSFER_FUNCTION: Moves sovereignty, territory, and tribute (jizya, kharaj, spoils) from non-Muslim populations to the caliphal state and broader Muslim polity, in exchange for protected but politically subordinate dhimmi status.
% ABSENT_VOICES: Non-Muslim populations are not consulted on the terms of their subordination; internal dissenters arguing for purely defensive readings are institutionally marginalized; rival Muslim readings that bypass caliphal authority (revolutionary vanguard) are juridically excluded.
% DISAPPEARANCE_RATIONALE: If the legal framework vanished, the caliphal monopoly on expansion would collapse, freelance military entrepreneurship would likely proliferate, and the systematic legitimation of conquest would be replaced by either purely defensive isolation or unregulated non-state violence.
% FOUNDING_PROBLEM: The early Muslim community faced encirclement by hostile empires (Byzantine, Sassanian) and the need to channel martial capacity into state-directed expansion while establishing governance over rapidly conquered diverse peoples.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists (al-Shaybani, al-Mawardi) attest the expansionist framework from within the tradition. Modern revisionist historians (e.g., Donner) contest the narrative of universal early hostility, suggesting commercial and diplomatic alternatives existed. Contemporary Islamic international law scholars outside the traditionalist beneficiary set debate whether the offensive obligation was a later juristic construction.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.76) reflects systematic transfer of sovereignty, territory, and tribute from non-Muslim populations through rule-bound but permitted offensive campaigns. Suppression (0.74) captures the active enforcement requirement: state monopoly on declaration, military organization, and denial of equal political alternatives. Theater ratio (0.42) registers that conditions like invitation and proportionality are genuine legal constraints in classical fiqh but also performatively legitimate outcomes that serve expansion. Accessibility collapse (0.70) is high because permanent equal peace is not a live option within this reading; the menu is conquest, subordination, or conversion. Resistance (0.58) reflects persistent historical and modern contestation. The temporal arc shows high extraction and suppression under classical and early modern empires, collapse under colonialism when enforcement capacity dissolved, and modern ideological revival increasing extractiveness without fully restoring classical enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (caliphal authority) experiences the constraint as a burden of legitimate governance constrained by juridical conditions. The payer seat (non-Muslim target populations) experiences it as structural subordination backed by military force. The beneficiary seat (Muslim polity) experiences it as security expansion. The jurist seat experiences it as an interpretive discipline generating professional authority. The engine should compute divergent seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority sits at low d (near beneficiary) because it controls activation and captures material gains, though constrained by legal conditions. Muslim polity sits at low-moderate d (net beneficiary of security and redistribution, identity-locked). Non-Muslim target populations sit at high d (full target) because the constraint literally operates through their subordination or conquest; exit is trapped. Islamic jurists sit at moderate-low d (benefit from interpretive monopoly but are textually bound). Internal dissenters and rival polities are excluded from directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling the coordination function as pure extraction: the framework genuinely solved collective-action problems (channeling martial energy into state-directed expansion and governing diverse peoples). It also prevents mislabeling extraction as pure coordination: jizya and dhimmi status are political-subordination rents, not payments for services at market equivalence. The founding problem (early community survival and expansion) is contested in status; the measurements show a modern revival after colonial collapse, suggesting the mandate is contested rather than clearly dead. If the caliphal vacancy proves permanent and the framework is maintained only by inertia, it would drift toward piton; if non-state actors capture its enforcement, toward snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caliphal_vacuum_operativity,
    'Is the expansionist legal framework operative without a caliph, or does the caliphal vacancy invalidate the obligation and push enforcement toward non-state actors?',
    'Comparative jurisprudential analysis of modern Islamist movements: do their legal arguments revive the caliphal condition through alternative authority (amir, bay''a to a non-caliph), or do they abandon the imam condition entirely?',
    'If the framework requires a caliph and none exists, the constraint is either zombified (piton) or displaced by revolutionary readings that abandon state monopoly. If alternative authority suffices, the constraint remains active tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caliphal_vacuum_operativity, conceptual, 'Whether the caliphal authority condition is satisfiable in the modern state system.').

omega_variable(
    proportionality_as_constraint_or_cover,
    'Are the juridical conditions (invitation to Islam first, proportionality, imam authority) genuine constraints on extraction, or primarily performative legal cover for expansion that would proceed regardless?',
    'Historical case study: compare campaigns conducted under jurist supervision versus freelance conquest; measure whether condition-violating campaigns were retroactively legitimated or condemned by the same jurists.',
    'If purely performative, theater_ratio rises and the constraint shifts toward snare; if genuinely constraining, the coordination function is stronger and the extraction is moderated by rule-of-law properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_constraint_or_cover, empirical, 'Whether legal conditions genuinely constrain or merely legitimate expansion.').

omega_variable(
    dhimmi_status_net_extraction,
    'Does dhimmi protection represent a net benefit relative to available historical alternatives (enslavement, massacre, anarchic warfare), or is it fundamentally extractive subordination regardless of comparative welfare?',
    'Comparative historical sociology of conquered populations under caliphate versus under rival empires of the same era, paired with analysis of jizya rates and dhimmi legal capacities relative to subject populations elsewhere.',
    'If net benefit, the payer seat directionality moderates and the constraint appears more coordinative; if fundamentally extractive, the victim classification strengthens and effective extraction rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dhimmi_status_net_extraction, preference, 'Whether dhimmi status is protective coordination or subordinating extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_exp_leg_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jihad_exp_leg_tr_t280, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 280, 0.25).
narrative_ontology:measurement(jihad_exp_leg_tr_t560, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 560, 0.4).
narrative_ontology:measurement(jihad_exp_leg_tr_t840, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 840, 0.55).
narrative_ontology:measurement(jihad_exp_leg_tr_t1120, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1120, 0.6).
narrative_ontology:measurement(jihad_exp_leg_tr_t1400, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1400, 0.42).

% Extraction over time
narrative_ontology:measurement(jihad_exp_leg_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(jihad_exp_leg_be_t280, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 280, 0.78).
narrative_ontology:measurement(jihad_exp_leg_be_t560, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 560, 0.74).
narrative_ontology:measurement(jihad_exp_leg_be_t840, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 840, 0.65).
narrative_ontology:measurement(jihad_exp_leg_be_t1120, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1120, 0.35).
narrative_ontology:measurement(jihad_exp_leg_be_t1400, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1400, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(jihad_exp_leg_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(jihad_exp_leg_su_t280, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 280, 0.9).
narrative_ontology:measurement(jihad_exp_leg_su_t560, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 560, 0.88).
narrative_ontology:measurement(jihad_exp_leg_su_t840, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 840, 0.8).
narrative_ontology:measurement(jihad_exp_leg_su_t1120, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1120, 0.45).
narrative_ontology:measurement(jihad_exp_leg_su_t1400, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1400, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
