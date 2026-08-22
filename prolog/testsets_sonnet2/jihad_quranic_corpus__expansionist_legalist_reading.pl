% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Classical Expansionist-Legalist Jihad Doctrine (Siyar Framework)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This story instantiates the expansionist-legalist reading of the jihad
 *   kernel: jihad as a state-administered legal obligation to extend Islamic
 *   political order into territories where it is absent, governed by
 *   procedural conditions (prior invitation to conversion or submission,
 *   exclusive authority of the imam/caliph to declare, proportionality in
 *   conduct) that in principle constrain but do not prohibit offensive
 *   campaigns. This is the classical siyar (Islamic international law)
 *   framework associated with jurists like al-Shaybani and al-Mawardi,
 *   distinct from a defensive-only reading and from a revolutionary reading
 *   that bypasses state authority entirely. The ε authored here reflects the
 *   standing arrangement as this reading itself construes it — a rule-bound
 *   but genuinely expansionist and extractive apparatus — not the more
 *   restrained ideal the doctrine's own procedural conditions gesture toward.
 *
 * KEY AGENTS:
 *   - caliphal_state_apparatus: primary agenda_setter, holds exclusive declaration authority and captures tribute/territorial gains
 *   - military_commanders: executing beneficiary, captures spoils and land
 *   - classical_jurist_class: interpretive beneficiary, professional authority tied to framework's continued operation
 *   - non_muslim_polities_under_expansion and conquered_non_muslim_populations: primary targets of the expansionist license
 *   - dhimmi_subjects: permanent subordinated tier created by the framework's outcome structure
 *   - sedentary_rural_muslim_populations: excluded internal cost-bearers with no voice in declaration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.62).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.71).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Classical Expansionist-Legalist Jihad Doctrine (Siyar Framework)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'da068a4d-6726-4120-9a98-b8ce4ba684d1').
narrative_ontology:cs_kernel_codification('da068a4d-6726-4120-9a98-b8ce4ba684d1', fixed_text).
narrative_ontology:cs_authority_grounding('da068a4d-6726-4120-9a98-b8ce4ba684d1', lineage).
narrative_ontology:cs_interpretation_layer_present('da068a4d-6726-4120-9a98-b8ce4ba684d1').
narrative_ontology:cs_reading_relation('da068a4d-6726-4120-9a98-b8ce4ba684d1', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('da068a4d-6726-4120-9a98-b8ce4ba684d1', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('da068a4d-6726-4120-9a98-b8ce4ba684d1', foundational, offensive_jihad_permissible_under_imam_authority).
narrative_ontology:cs_axiom_status(offensive_jihad_permissible_under_imam_authority, holdable).
narrative_ontology:cs_axiom_grounding('da068a4d-6726-4120-9a98-b8ce4ba684d1', offensive_jihad_permissible_under_imam_authority, conventional).
narrative_ontology:cs_axiom('da068a4d-6726-4120-9a98-b8ce4ba684d1', foundational, declaration_authority_vested_exclusively_in_established_imam).
narrative_ontology:cs_axiom_status(declaration_authority_vested_exclusively_in_established_imam, holdable).
narrative_ontology:cs_axiom_grounding('da068a4d-6726-4120-9a98-b8ce4ba684d1', declaration_authority_vested_exclusively_in_established_imam, conventional).
narrative_ontology:cs_reference_frame('da068a4d-6726-4120-9a98-b8ce4ba684d1', classical_siyar_caliphal_framework).
narrative_ontology:cs_drift_state('da068a4d-6726-4120-9a98-b8ce4ba684d1', post_caliphate_abolition_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('da068a4d-6726-4120-9a98-b8ce4ba684d1', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state_apparatus).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, military_commanders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities_under_expansion).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_subjects).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, dar_al_islam_dar_al_harb_distinction).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, imam_monopoly_on_just_war_declaration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the exclusive legal authority (imamah) to declare and direct offensive campaigns, structures the invitation-to-Islam procedural sequence, negotiates dhimmi treaty terms, and collects tribute (jizya, kharaj) and territorial control as the campaigns succeed. The jurisprudential conditions (invitation first, proportionality, no targeting non-combatants) are administered and interpreted by this same authority, giving it wide discretion over when conditions are deemed satisfied.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, continental).

% Execute campaigns under caliphal authorization, receive shares of ghanima (spoils) and land grants, and build political capital and legitimacy through successful expansion. Their career advancement and material fortune are directly tied to continued campaigns being classified as licit jihad rather than mere conquest.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, military_commanders, beneficiary,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__expansionist_legalist_reading, military_commanders, agenda_setter).

% Produces and refines the siyar (international law) literature defining the procedural conditions for licit jihad, occupies a necessary interpretive role between text and state action, and derives professional authority and patronage from being the arbiters of when campaigns satisfy jurisprudential requirements. Their institutional position depends on the framework remaining in active use.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_jurist_class, beneficiary,
    organized, civilizational, constrained, continental).

% Receive the formal invitation to convert or submit, and face campaign if they decline both conversion and tributary submission. Their political sovereignty is the direct object of the doctrine's expansionist license; the 'invitation first' condition offers a procedural off-ramp but does not remove the underlying threat of subjugation if refused.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities_under_expansion, payer,
    moderate, biographical, trapped, regional).

% Once territory falls, become subject populations under new political order regardless of individual consent to the outcome; bear the costs of the campaign itself (loss of independent governance, warfare's human toll) whether or not they are later granted dhimmi status.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_non_muslim_populations, payer,
    powerless, generational, trapped, regional).

% Live under negotiated protected status after submission, paying jizya in exchange for communal autonomy and physical protection. Occupy a permanent liminal legal tier below Muslim subjects — protected from campaign but structurally subordinated indefinitely, with formal conversion the only route out of the tier.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_subjects, payer,
    powerless, civilizational, constrained, regional).

% Bear conscription burdens and taxation to fund campaigns decided at the caliphal center; have no voice in the imam's decision to declare offensive jihad despite bearing much of its material and mortal cost. Their interests are not represented in the jurisprudential conditions, which govern relations toward the enemy, not obligations owed to the mobilized.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, sedentary_rural_muslim_populations, excluded,
    powerless, biographical, trapped, local).

% Study the siyar corpus, campaign records, and treaty texts to assess how far declared jurisprudential conditions actually constrained state practice versus functioning as post-hoc legitimation for expansion already underway.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, caliphal_state_apparatus).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule-bound procedure — centralizing declaration authority in the imam, requiring prior invitation, and setting proportionality limits — that in principle restrains warfare, distinguishes licit campaigns from mere raiding, and gives conquered populations a defined legal status (dhimmi) rather than arbitrary treatment.
% TRANSFER_FUNCTION: Moves political sovereignty, territory, and tribute revenue from non-Muslim polities and their populations to the caliphal state and its military and juristic elite, formally routed through a legal procedure (invitation, declaration, proportionate conduct, tribute settlement) that legitimates the transfer within the framework's own terms.
% ABSENT_VOICES: Non-Muslim polities and populations subject to the invitation-or-campaign choice have no voice in whether the imam declares jihad against them; conscripted Muslim rural populations bearing the material cost of campaigns are likewise absent from the deliberation, since the jurisprudential conditions govern conduct toward the external party, not consent from the internal party bearing the burden.
% DISAPPEARANCE_RATIONALE: If the doctrine's authorization for offensive campaigns vanished, caliphal and successor states would lose their primary legal warrant for continued territorial expansion; military and juristic elites dependent on campaign-derived revenue, land grants, and interpretive authority would lose a major source of legitimacy and income; and neighboring non-Muslim polities would face a materially different risk calculus in their relations with Islamic states.
% FOUNDING_PROBLEM: Early Muslim community sought a framework distinguishing legitimate warfare in the path of establishing and expanding Islamic political order from unregulated tribal raiding, and needed to determine the political and legal status (submission, tribute, conversion, or resistance) of populations encountered as the polity expanded beyond Arabia.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists (al-Shaybani, al-Mawardi) and dependent successor states attest the framework remains a live obligation grounded in scriptural and prophetic precedent. Independent historians of early Islamic conquest and comparative international-law scholars note the procedural conditions (invitation, proportionality) were frequently honored in form but not consistently in substance, and that many campaigns proceeded with only nominal compliance — corroboration from outside the beneficiary class supports reading the doctrine as substantially, though not purely, a legitimation apparatus for expansion decided on other grounds.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.62) reflects that the doctrine's core function is legitimating a transfer of sovereignty and resources from non-Muslim polities to the Islamic state, even though the transfer is procedurally regulated. Suppression (0.71) is high because the framework's persistence depends on active state monopoly over the declaration power and armed enforcement against those who refuse both conversion and tribute; the procedural conditions restrain conduct but do not eliminate the coercive core. Theater ratio is moderate-low (0.28): the jurisprudential conditions (invitation, proportionality) are not pure performance — they demonstrably shaped conduct in some campaigns — but historical practice shows frequent nominal-compliance-only satisfaction of the invitation requirement, which is why theater rises rather than staying negligible. Accessibility collapse (0.5) and resistance (0.6) sit at the tangled-rope midpoint: real coordination value (regulated warfare, defined legal status for conquered populations) coexists with real, mounted resistance from those it targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphal apparatus, military commanders, and jurist class sit near the beneficiary end: they set the terms, execute for material and status gain, and control interpretation of when conditions are satisfied. Non-Muslim polities and conquered/dhimmi populations sit near the target end: trapped or constrained exit, bearing the sovereignty loss, tribute burden, or permanent subordinate legal tier the doctrine produces. Rural Muslim populations are a structurally distinct excluded party — they are not the doctrine's target but bear conscription and taxation costs while having no voice in the imam's declaration decision; this is why they are marked excluded rather than payer in the beneficiary/victim sense tied to the doctrine's declared purpose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing licit warfare and defining status for encountered populations from unregulated raiding) is contested rather than dead: successor movements and jurists maintain it is a live civilizational obligation, while comparative historians note the underlying political-military conditions that motivated seventh-to-ninth century formulations (an expanding, resource-constrained polity without settled borders) no longer describe most contemporary Muslim-majority states. This mismatch (status=contested + world_rearranges) is the diagnostic signal the framework is built to surface, not a resolved verdict — the classification here is of the classical legalist doctrine as a historical-jurisprudential structure, not a claim about its contemporary applicability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_conditions_binding_or_ceremonial,
    'Did the jurisprudential conditions (invitation first, imam authority, proportionality) function as genuine constraints on when and how campaigns proceeded, or were they primarily post-hoc legitimating formalities satisfied nominally regardless of actual practice?',
    'Comparative historical analysis of campaign records against jurist-prescribed procedure: frequency of documented invitation being extended and genuinely awaited versus campaigns proceeding immediately; frequency of proportionality violations recorded by contemporaneous or near-contemporaneous chroniclers, including from Muslim sources critical of specific campaigns.',
    'If conditions were substantially binding, the framework functions closer to genuine rule-governed coordination (lower effective extraction); if they were substantially ceremonial, the framework functions closer to a pure legitimation apparatus for expansion decided on political-military grounds alone (higher effective extraction, theater_ratio should be revised upward).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_conditions_binding_or_ceremonial, empirical, 'Whether procedural conditions substantively constrained campaigns or mainly legitimated them after the fact.').

omega_variable(
    reading_selection_and_kernel_indeterminacy,
    'Is the expansionist-legalist reading the dominant classical position, or one of several coexisting classical positions with no single authoritative resolution — and does treating it as ''the'' classical jihad doctrine understate genuine internal juristic disagreement (e.g. Hanafi vs. Shafi''i differences on the necessity and scope of invitation, or disagreement on whether offensive jihad is fard kifaya at all in the absence of an active caliph)?',
    'Systematic comparison across the four Sunni madhahib and Shi''i jurisprudence on: (a) whether offensive jihad is obligatory absent provocation, (b) whether the invitation requirement is a strict precondition or a recommended courtesy, (c) whether a functioning caliphate is a precondition for licit offensive declaration.',
    'If juristic consensus was thinner than this story implies, the doctrine described here is itself a reading of a smaller sub-corpus within classical jurisprudence, not ''the'' classical position — this would suggest a further decomposition into distinct madhhab-specific constraint stories rather than treating expansionist_legalist_reading as a single unified doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_indeterminacy, conceptual, 'Whether internal classical juristic disagreement warrants further decomposition beyond the three-reading kernel structure.').

omega_variable(
    dhimmi_status_coordination_or_subordination,
    'Is the dhimmi arrangement best read as a genuine coordination mechanism (protection and communal autonomy in exchange for tribute, an improvement over unregulated conquest) or as a durable structural subordination whose ''protection'' framing is the extractive mechanism''s own justification?',
    'Comparative study of dhimmi legal and social status across different eras and polities against contemporaneous alternatives (treatment of conquered populations under non-Islamic contemporaneous empires) to establish whether dhimmi status was a net improvement, a net cost, or highly variable by period and location.',
    'If genuinely protective relative to contemporaneous alternatives, this weighs toward the coordination pole of the tangled-rope classification; if primarily a durable subordination mechanism, this weighs the extraction pole higher, potentially shifting the classification of this specific sub-arrangement toward snare-like characteristics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dhimmi_status_coordination_or_subordination, conceptual, 'Whether dhimmi status functions primarily as protective coordination or durable subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 80, 0.3).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.1).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the jihad_quranic_corpus kernel, each authored as an independent, ε-invariant constraint per the ε-invariance principle. expansionist_legalist_reading (this file) authors an offensive-permitting, state-monopolized, procedurally-bounded doctrine with substantial extraction (ε=0.62) directed at non-Muslim polities and conquered populations. defensive_spiritual_reading authors a narrower doctrine limited to internal struggle and defensive response, expected to carry substantially lower extraction since it denies any offensive mandate. revolutionary_vanguard_reading authors an individual-obligation doctrine that bypasses state authority via takfir, expected to show a different beneficiary/victim structure (targeting nominally-Muslim rulers and occupiers rather than external non-Muslim polities) and a different suppression profile (enforcement is decentralized/vigilante rather than state-monopolized). The three readings are not measurements of the same constraint from different angles — they are structurally distinct claims about who holds declaration authority, who is targeted, and under what conditions armed struggle is licit, and are linked here for contamination/family analysis, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
