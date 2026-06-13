% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Jihad (Expansionist Legalist Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'expansionist legalist' reading of Jihad
 *   within the Quranic corpus, which views Jihad as an obligation to
 *   establish Islamic governance in lands where it is absent. This reading is
 *   characterized by specific jurisprudential conditions, including an
 *   initial invitation to Islam, the necessity of an Imam's (state/caliph)
 *   authority for declaration, and adherence to proportionality in warfare.
 *   Crucially, it permits offensive campaigns under these conditions,
 *   legitimizing systematic expansion within a legal framework. Non-Muslims
 *   in territories under consideration are placed in a liminal status, either
 *   as potential dhimmi (protected non-Muslim subjects) or combatants.
 *
 * KEY AGENTS:
 *   - islamic_state_caliphate: Agenda setter (institutional/arbitrage) — declares and leads campaigns
 *   - ulama_jurists: Beneficiary (institutional/constrained) — provides legitimacy and interpretation
 *   - non_muslim_polities: Payer (powerful/trapped) — target of campaigns, faces subjugation or destruction
 *   - non_muslim_populations: Payer (powerless/trapped) — faces conversion, dhimmi status, or combatant status
 *   - muslim_soldiers: Beneficiary (moderate/identity_locked) — participates in campaigns, receives spiritual and material rewards
 *   - defensive_spiritual_scholars: Excluded (organized/constrained) — advocates for a different reading, but their views are marginalized by this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.65).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.75).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad (Expansionist Legalist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '5f46fa70-8c74-436d-a155-f7ecf0a2a082').
narrative_ontology:cs_kernel_codification('5f46fa70-8c74-436d-a155-f7ecf0a2a082', fixed_text).
narrative_ontology:cs_authority_grounding('5f46fa70-8c74-436d-a155-f7ecf0a2a082', lineage).
narrative_ontology:cs_interpretation_layer_present('5f46fa70-8c74-436d-a155-f7ecf0a2a082').
narrative_ontology:cs_reading_relation('5f46fa70-8c74-436d-a155-f7ecf0a2a082', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('5f46fa70-8c74-436d-a155-f7ecf0a2a082', jihad_quranic_corpus__revolutionary_vanguard_reading, influences).
narrative_ontology:cs_axiom('5f46fa70-8c74-436d-a155-f7ecf0a2a082', foundational, islamic_governance_universal_obligation).
narrative_ontology:cs_axiom_status(islamic_governance_universal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5f46fa70-8c74-436d-a155-f7ecf0a2a082', islamic_governance_universal_obligation, theological).
narrative_ontology:cs_axiom('5f46fa70-8c74-436d-a155-f7ecf0a2a082', foundational, imam_monopoly_on_jihad_declaration).
narrative_ontology:cs_axiom_status(imam_monopoly_on_jihad_declaration, holdable).
narrative_ontology:cs_axiom_grounding('5f46fa70-8c74-436d-a155-f7ecf0a2a082', imam_monopoly_on_jihad_declaration, conventional).
narrative_ontology:cs_reference_frame('5f46fa70-8c74-436d-a155-f7ecf0a2a082', classical_islamic_legal_tradition).
narrative_ontology:cs_drift_state('5f46fa70-8c74-436d-a155-f7ecf0a2a082', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5f46fa70-8c74-436d-a155-f7ecf0a2a082', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurists).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority (e.g., a Caliph or Imam) responsible for declaring and leading offensive Jihad campaigns. Benefits from territorial expansion, increased resources, and enhanced legitimacy as the enforcer of divine law. Controls the interpretation and application of jurisprudential conditions.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate, agenda_setter,
    institutional, generational, arbitrage, global).

% Religious scholars and legal experts who provide the jurisprudential framework and legitimacy for the expansionist legalist reading of Jihad. Their authority and influence are enhanced by the state's adherence to their interpretations. They benefit from the establishment of Islamic governance and the enforcement of Sharia law.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurists, beneficiary,
    institutional, generational, constrained, global).

% Independent non-Muslim states or political entities that are targeted by offensive Jihad campaigns. They face the choice of conversion, subjugation (e.g., becoming dhimmi under Islamic rule), or military resistance, often leading to their dissolution or absorption into the Islamic state. Their existence as independent entities is directly threatened.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities, payer,
    powerful, generational, trapped, regional).

% Individuals and communities living in non-Muslim territories targeted by Jihad. They face pressure to convert to Islam, accept dhimmi status (with associated taxes and restrictions), or become combatants. Their cultural, religious, and political autonomy is severely curtailed or eliminated.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, biographical, trapped, local).

% Individuals who participate in offensive Jihad campaigns under the authority of the Imam. They are motivated by religious duty, promises of spiritual reward (martyrdom, paradise), and potential material gains (booty, land). Their identity is often deeply intertwined with their role as defenders and expanders of Islam.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiers, beneficiary,
    moderate, biographical, identity_locked, national).

% Scholars and movements who advocate for a 'defensive spiritual' reading of Jihad, emphasizing internal struggle and defensive warfare only. Their interpretations are marginalized or actively suppressed by the expansionist legalist framework, which views their positions as undermining the religious obligation to establish Islamic governance. They are not part of the decision-making or legitimizing structure of this constraint.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_scholars, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__expansionist_legalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the military, legal, and theological resources of an Islamic state to systematically expand its territorial and political influence, establishing a unified Islamic order where it is deemed absent. It provides a framework for legitimizing conquest and integrating newly acquired territories and populations.
% TRANSFER_FUNCTION: Transfers political sovereignty, territorial control, and resources from non-Muslim polities to the Islamic state. It also transfers religious and cultural allegiance from non-Muslim populations to Islam, or imposes dhimmi status, extracting taxes and enforcing social hierarchies.
% ABSENT_VOICES: Non-Muslim polities and populations, as well as dissenting Muslim scholars who advocate for purely defensive or spiritual interpretations of Jihad, are structurally excluded from the discourse that legitimizes and directs these campaigns. Their objections are either dismissed as illegitimate or suppressed by force.
% DISAPPEARANCE_RATIONALE: If this expansionist legalist reading of Jihad disappeared, the geopolitical landscape of regions historically or currently influenced by this doctrine would fundamentally rearrange. The justification for offensive military campaigns would vanish, leading to a re-evaluation of state legitimacy, territorial claims, and the status of non-Muslim minorities. The Islamic state's foreign policy and internal governance structures would require radical transformation.
% FOUNDING_PROBLEM: The perceived absence of comprehensive Islamic governance and the presence of non-Islamic rule in various lands, seen as an affront to divine law and an impediment to the full flourishing of the Muslim community (Ummah).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is considered 'live' by proponents of this reading, who continue to assert the religious obligation to establish Islamic governance globally. However, external observers, including international legal bodies and secular states, contest this, viewing it as a justification for aggression rather than a response to a legitimate 'problem.' Independent historians document the historical context of early Islamic expansion, but its contemporary 'liveness' is primarily asserted by the benefiting parties.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.65) because this reading permits offensive campaigns aimed at establishing Islamic governance, which inherently involves the subjugation or conversion of non-Muslim populations and polities. Suppression is also high (0.75) as it relies on military force and the suppression of alternative political systems. The 'invitation to Islam first' and 'proportionality' conditions provide a veneer of ethical constraint, but the underlying goal is expansion, making the coordination function (establishing a unified Islamic order) inseparable from extraction. Theater ratio is low (0.20) because the military and legal mechanisms are genuinely aimed at achieving the stated expansionist goals, not merely performing a function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Islamic state/caliphate and the legitimizing ulama, this is a divinely ordained and legally structured coordination mechanism for establishing justice and order. From the perspective of non-Muslim polities and populations, it is a highly extractive and suppressive force, offering limited options (conversion, subjugation, or resistance with severe consequences). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic state/caliphate and ulama are primary beneficiaries (d near 0.0) as they gain authority, territory, and resources. Muslim soldiers are also beneficiaries (d near 0.2) through spiritual rewards and material gains from conquest. Non-Muslim polities and populations are clear targets (d near 1.0), facing existential threats or forced subjugation. The 'invitation to Islam' is a coordination mechanism for potential converts, but for those who refuse, it becomes a prelude to extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of Jihad is not subject to mandatrophy in the traditional sense, as its mandate (establishing Islamic governance) is considered a perpetual religious obligation. However, the 'contested' status of its founding problem (the absence of Islamic governance) suggests that its justification is challenged by those who argue for a more pluralistic world order or a purely defensive interpretation of Jihad. The persistence of this reading, despite its high extractiveness, is maintained by the institutional power of those who benefit from it and the suppression of alternative interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''expansionist legalist'' reading of Jihad, or is it a mischaracterization of the broader Quranic corpus?',
    'Comparative textual analysis by independent Islamic scholars, examining the historical application and jurisprudential consensus across different schools of thought.',
    'If mischaracterized, the constraint''s claimed legitimacy as a ''legalist'' framework collapses, reclassifying it as a Snare or even a revolutionary_vanguard_reading, with higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as one specific reading of the ''jihad_quranic_corpus'' kernel, distinct from other interpretations.').

omega_variable(
    state_authority_vs_individual_obligation,
    'Does the ''imam authority'' condition genuinely constrain offensive jihad to state actors, or is it a rhetorical cover for individual or non-state actors to declare it?',
    'Empirical observation of historical and contemporary declarations of jihad, tracing the chain of command and legitimacy claims. Analysis of jurisprudential rulings on unauthorized declarations.',
    'If individual declarations are common and legitimized, the constraint''s suppression and extractiveness would be higher, as it would bypass formal checks and balances, potentially reclassifying it as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_vs_individual_obligation, empirical, 'Ambiguity regarding the practical enforcement of state monopoly on jihad declaration.').

omega_variable(
    proportionality_enforcement,
    'Are the proportionality and non-combatant immunity conditions genuinely enforced during offensive campaigns, or are they selectively applied or ignored?',
    'Historical and contemporary case studies of military campaigns conducted under this reading, assessing adherence to proportionality and non-combatant rules through independent human rights reports and historical accounts.',
    'If these conditions are routinely violated, the constraint''s extractiveness and victim count would be significantly higher, pushing it closer to a Snare by removing its claimed ethical limits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_enforcement, empirical, 'Uncertainty about the practical application and enforcement of ethical limits on warfare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(jiha_tr_t200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 200, 0.2).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(jiha_be_t200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement(jiha_su_t200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 200, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'jihad_quranic_corpus' kernel. This 'expansionist legalist' reading differs from the 'defensive spiritual' reading by permitting offensive campaigns, and from the 'revolutionary vanguard' reading by requiring state authority and jurisprudential conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
