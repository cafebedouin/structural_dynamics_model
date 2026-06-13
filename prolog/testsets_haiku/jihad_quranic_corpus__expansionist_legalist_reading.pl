% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Jihad Obligation: Expansionist Legalist Reading (Quranic Corpus)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of a contested kernel: the
 *   Quranic corpus on jihad and Islamic governance. The expansionist legalist
 *   reading interprets the corpus to permit systematic territorial expansion
 *   under jurisprudentially specified conditions (prior invitation to Islam,
 *   imam/caliph authority, proportionality, incorporation of non-Muslims as
 *   dhimmis or converts). This reading competes within the Islamic tradition
 *   with defensive-spiritual readings (emphasizing internal struggle and
 *   defensive-only armed response) and revolutionary-vanguard readings
 *   (bypassing state authority via takfir and individual obligation). All
 *   three readings claim the same Quranic foundation but reach structurally
 *   different conclusions about permissible expansion, state monopoly on war
 *   declaration, and the scope of obligatory jihad. This story models ONLY
 *   the expansionist legalist reading as a coherent, internally consistent
 *   constraint — not as the truth of the kernel, but as the structural claim
 *   this reading commits its adherents to making.
 *
 * KEY AGENTS:
 *   - Caliph or Imam Authority: institutional agenda-setter, monopolizes jihad declaration and conditions assessment
 *   - Islamic Scholarly Tradition: powerful beneficiary, elaborates jurisprudential framework legitimizing expansion
 *   - Non-Muslim Polities in Liminal Status: moderate-power payer, targeted for conversion or conquest, offered dhimmi status
 *   - Populations Subject to Conquest Campaigns: powerless payer, bear direct costs of expansion (war, displacement, subordination)
 *   - Competing Islamic Readings: powerful beneficiary/payer, contest for interpretive authority within tradition
 *   - Populations Under Islamic Rule: moderate beneficiary, receive coordination benefits and stable law; also carry expansion costs
 *   - Western Secular Authorities: institutional observer/excluded, operate under different legitimacy kernel
 *   - International Humanitarian Law Tradition: institutional observer, documents constraint and analyzes humanitarian implications
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.68).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.72).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad Obligation: Expansionist Legalist Reading (Quranic Corpus)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '845e276d-ed1c-4de9-9886-b3dd15007b92').
narrative_ontology:cs_kernel_codification('845e276d-ed1c-4de9-9886-b3dd15007b92', fixed_text).
narrative_ontology:cs_authority_grounding('845e276d-ed1c-4de9-9886-b3dd15007b92', lineage).
narrative_ontology:cs_interpretation_layer_present('845e276d-ed1c-4de9-9886-b3dd15007b92').
narrative_ontology:cs_reading_relation('845e276d-ed1c-4de9-9886-b3dd15007b92', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('845e276d-ed1c-4de9-9886-b3dd15007b92', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('845e276d-ed1c-4de9-9886-b3dd15007b92', foundational, offensive_expansion_lawful_under_conditions).
narrative_ontology:cs_axiom_status(offensive_expansion_lawful_under_conditions, holdable).
narrative_ontology:cs_axiom_grounding('845e276d-ed1c-4de9-9886-b3dd15007b92', offensive_expansion_lawful_under_conditions, deontological).
narrative_ontology:cs_axiom('845e276d-ed1c-4de9-9886-b3dd15007b92', foundational, caliph_monopoly_on_jihad_declaration).
narrative_ontology:cs_axiom_status(caliph_monopoly_on_jihad_declaration, holdable).
narrative_ontology:cs_axiom_grounding('845e276d-ed1c-4de9-9886-b3dd15007b92', caliph_monopoly_on_jihad_declaration, conventional).
narrative_ontology:cs_reference_frame('845e276d-ed1c-4de9-9886-b3dd15007b92', quranic_expansion_framework).
narrative_ontology:cs_drift_state('845e276d-ed1c-4de9-9886-b3dd15007b92', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('845e276d-ed1c-4de9-9886-b3dd15007b92', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_apparatus).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliph_or_imam_authority).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities_in_liminal_status).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, populations_subject_to_conquest_campaigns).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).

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
 *   Extractiveness is high and rising (0.52→0.68 over interval) because the constraint vests authority to declare expansion and assess conditions in the caliph/imam apparatus, creating principal-agent separation between decision-maker and affected populations. Suppression is highest of all metrics (0.72 at end) because the constraint's persistence depends on preventing rival interpretations (defensive, revolutionary) from institutionalizing alternative jurisprudence — the caliph's monopoly must be maintained against both internal contestation and external secular frameworks. Theater is moderate-rising (0.28→0.41) because the jurisprudential conditions (da'wa, proportionality) are often performed rather than substantively applied — the formal framework legitimizes expansion even where conditions are honored minimally. Measurements run on a shared time grid: extractiveness rises as Islamic state apparatus consolidates, suppression hardens as competing readings gain popular appeal and require more institutional force to suppress, theater increases as more expansion is justified through the framework's conditions while external observers document routine non-compliance with those conditions. Accessibility collapse (0.58) reflects that alternatives to Islamic governance remain available to liminal polities but are narrowed by the framework's pressure; resistance (0.64) is substantial because targeted populations resist conquest and competing Islamic readings resist the monopoly on jurisprudential authority.
 *
 * PERSPECTIVAL GAP:
 *   The most acute perspectival gap lies between the caliph/imam authority and the non-Muslim payer seats. From the institutional agenda-setter's perspective, the constraint solves a coordination problem: how to expand Islamic governance systematically while maintaining legal coherence and preventing arbitrary predation. The jurisprudential conditions (invitation first, proportionality, dhimmi incorporation) appear as genuine constraints on power. From the target population's perspective, the same framework is a legalistic cover for systematic expansion: the conditions are imposed on populations that did not agree to them, the caliph monopolizes their interpretation, and the escape routes (conversion, dhimmi status) are both constrained by the framework itself. The scholarly tradition experiences it as coordination within the tradition's intellectual ecosystem; competing readings experience it as suppression of alternative jurisprudence. The constraint's claimed type (tangled rope) reflects this: it combines genuine coordination function (for Islamic governance and the scholarly tradition) with asymmetric extraction (from non-Muslim populations and rival readings). A defensive-reading seat would perceive the same texts and framework as fundamentally misinterpreted — the constraint would compute as snare from that seat. A revolutionary-reading seat would perceive it as institutional capture of legitimate struggle — also snare. The engine captures this divergence through per-seat classification; do not reconcile these perspectives in the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional asymmetry is extreme and structural. The caliph/imam apparatus and Islamic scholarly tradition sit at the beneficiary end (d near 0.0-0.2): they collect interpretive authority, territorial expansion, revenues from incorporated populations, and political legitimacy. Non-Muslim polities and conquest-campaign populations sit at the target end (d near 0.8-1.0): they bear the costs of war, loss of sovereignty, subordinate legal status, and have minimal exit. Competing Islamic readings occupy a peculiar middle position (d near 0.4-0.6): they benefit from engaging with the tradition's intellectual resources but suffer suppression when institutional authority consolidates around the expansionist reading. Populations under Islamic rule are near symmetric (d near 0.5): genuine coordination benefits (stable law, legal security, state protection) offset by the constraint's expansion logic applied elsewhere and the subordinate status of non-Muslims within their own governance. The engine will compute divergent per-seat classifications from this structural data: the caliph seat computes as coordination (rope), the payer seats compute as extraction (snare or tangled rope depending on whether coordination benefits exist), the competing-reading seat computes as captured or suppressed depending on its power trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy dynamic is the inverse of simple obsolescence: the founding problem (how to systematize and legitimize territorial expansion under law) is NOT dead, nor has its function changed. Instead, the constraint exhibits what might be called 'problematic-function persistence': the framework successfully does what it was built to do (legitimize expansion within jurisprudential rules), but the expansion it systematizes is contested at the foundational level. The constraint persists not because no one wants to fix it (competitive readings explicitly want to replace it), but because the caliph/institutional apparatus has captured the authority to interpret the founding problem itself. Defensive readings reframe the founding problem as 'how to maintain Islamic community amid external threats' (solved by defense, not expansion). Revolutionary readings reframe it as 'how to establish Islamic governance against apostate rulers' (solved by internal vanguard action, not systematic state expansion). From the expansionist reading's own framework, there is no mandatrophy — the constraint is working as designed. But from outside the framework, the constraint exhibits mandate drift: it legitimizes actions (conquest campaigns against non-threatening populations, subordination of non-Muslims) that the original problem-statement may not have required. This is a case where mandatrophy resolution requires disputing the founding problem itself, not just its solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expanding_vs_consolidating_state_incentive,
    'Does the measured extractiveness (0.68) reflect genuine expansion-driving incentives of the caliph apparatus, or is the constraint increasingly performative — invoked to justify consolidation of already-conquered territories while actual territorial expansion slows?',
    'Historical comparison of expansion rate (territory gained per decade) against institutionalization rate (state bureaucracy, taxation systems, legal codification). If expansion rate falls while extractiveness stays high, the constraint is increasingly theater-driven (internalizing justifications for holding rather than taking).',
    'If consolidation-driven, the constraint is drifting toward piton (framework persists without primary function, theater ratio would eventually rise above 0.5). If expansion-driven, extractiveness reflects real asymmetric power and the constraint remains snare/tangled-rope for payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expanding_vs_consolidating_state_incentive, empirical, 'Whether extractiveness measures ongoing expansion incentive or defensive consolidation dressed in expansionist language').

omega_variable(
    internalized_vs_structural_suppression_of_rival_readings,
    'Within Islamic scholarly tradition, is the dominance of the expansionist reading sustained by structural suppression (institutional exclusion of defensive and revolutionary readings from authority positions) or by genuine intellectual conviction that the expansionist reading is more coherent?',
    'Post-suppression trajectory: if structural suppression is lifted (rival readings gain state patronage), do they rapidly gain adherents, or do they remain marginal? If marginal, the reading''s dominance is intellectual; if they surge, suppression was the primary constraint.',
    'If structural, suppression is necessary to maintain the reading''s institutional dominance and the measured 0.72 suppression is real enforcement. If intellectual, suppression may be performative and the reading is more robust than the metrics suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression_of_rival_readings, empirical, 'Whether rival readings are suppressed or genuinely less persuasive within the tradition').

omega_variable(
    reading_choice_structural_vs_theological,
    'This constraint instantiates ONE reading of the Quranic corpus. Is the choice of this reading over defensive or revolutionary alternatives primarily STRUCTURAL (benefits the caliph apparatus, so it is institutionalized) or primarily THEOLOGICAL (the expansionist reading is the most coherent interpretation of the texts)?',
    'Comparative analysis of Quranic exegesis across readings: do the readings differ in which Surahs they emphasize, or do they interpret the same Surahs differently? Are the differences resolvable by better scholarship, or are they built into the reading-choice itself (i.e., all three readings are defensible from the texts)? Does institutional support correlate with scholarly quality or with political utility?',
    'If structural, the reading is a snare whose legitimacy rests on power, not on exegetical merit — the constraint''s persistence is vulnerable to power shifts. If theological, the reading''s coherence is independent of institutional support, making it more robust to political challenge. Hybrid: the reading may be both defensible AND strategically useful — in this case, both factors matter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_structural_vs_theological, conceptual, 'Whether the expansionist reading''s dominance reflects its exegetical merit, its political utility, or both').

omega_variable(
    dhimmi_status_coordination_vs_subordination,
    'The constraint offers non-Muslim populations incorporated into Islamic governance a legally specified status (dhimmi) with protections and obligations. Is dhimmi status a genuine coordination benefit (stable law, property rights, religious autonomy, predictability) that partially offsets extraction costs, or is it primarily a mechanism for subordination and revenue extraction?',
    'Comparative study of non-Muslim populations under Islamic governance: do populations flourish (economically, culturally, intellectually) under dhimmi status compared to pre-incorporation alternatives, or do they stagnate? Do they choose to stay or emigrate when given the option? Do property rights and religious autonomy functionally constrain the state''s extraction?',
    'If genuine coordination benefit, the constraint for non-Muslim populations is tangled rope (coordination + extraction) and they are legitimate beneficiaries alongside payers. If primarily subordination, it is snare (extraction with minimal coordination function). This determines whether the engine classifies non-Muslim populations as payers or dual-positioned.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dhimmi_status_coordination_vs_subordination, empirical, 'Whether dhimmi status is coordination benefit or legalized subordination').

omega_variable(
    reading_foreclosure_relationship_to_defensive,
    'This expansionist reading forecloses or coexists with the defensive-spiritual reading? The core premises diverge: expansionist permits offensive campaigns under conditions; defensive restricts to response and internal struggle. Can both be held within a single Islamic framework, or does accepting one require rejecting the other?',
    'Textual analysis: are there Islamic legal scholars or frameworks that genuinely hold both readings simultaneously (selective application based on context), or do all coherent positions reduce to one or the other? Does institutional pluralism (multiple schools of Islamic law) permit simultaneous holding, or do positions always collapse to a single reading when pressed?',
    'If forecloses: the readings are incompatible at the foundational level and institutional dominance of one reading blocks the other. The relationship is zero-sum competition. If coexists: both readings are live positions and institutional dominance prevents formal plurality without logical foreclosure. The relationship is suppression, not elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_relationship_to_defensive, conceptual, 'Whether expansionist and defensive readings are logically incompatible or merely institutionally competing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(jiha_tr_t8, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(jiha_tr_t16, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(jiha_tr_t24, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(jiha_tr_t32, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(jiha_be_t8, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(jiha_be_t16, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(jiha_be_t24, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement(jiha_be_t32, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(jiha_su_t8, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(jiha_su_t16, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(jiha_su_t24, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(jiha_su_t32, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_legal_status_subordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_monopoly_on_war_declaration).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the jihad_quranic_corpus kernel. The three readings (expansionist_legalist, defensive_spiritual, revolutionary_vanguard) are structurally distinct constraints with different ε values, victim/beneficiary structures, and institutional implications. All three claim the same Quranic foundation but instantiate different constraints through different interpretive choices. Each reading is authored separately (per ε-invariance principle); they are linked via network.affects_constraints to show family relationship. The expansionist reading influences (upstream pressure on) both defensive and revolutionary readings by institutionalizing the state monopoly on interpretation; if the expansionist reading gains institutional dominance, defensive and revolutionary readings are suppressed but not logically foreclosed. See commentary.kernel_context for framing details.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, powerful, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
