% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Imperial Mandate: Loyalist Restoration Reading
 *   domain: political/philosophical/historical
 *
 * SUMMARY:
 *   This constraint instantiates the loyalist_restoration_reading of the
 *   imperial_mandate kernel: the claim that divine legitimacy requires the
 *   emperor to exercise sovereignty directly, rendering any intermediary
 *   governance (shogunate, domain lords) usurpation. It was the
 *   constitutional logic of the Meiji Restoration (1868) and the subsequent
 *   dismantling of Tokugawa feudalism. The sibling reading,
 *   bakufu_delegation_reading, holds that the emperor's legitimacy-granting
 *   function is separable from administration; the loyalist reading
 *   forecloses that possibility, insisting on institutional rupture to
 *   restore direct rule. The constraint coordinates national unity and
 *   modernization while extracting authority and status from the shogunate,
 *   daimyo, and samurai class.
 *
 * KEY AGENTS:
 *   - restorationist_elites (agenda_setter/beneficiary): organized power, mobile exit â orchestrate the overthrow and administer the new state in the emperor's name
 *   - emperor (beneficiary): powerful symbolic actor, identity_locked â the non-refusable locus of sovereignty, simultaneously elevated and captive
 *   - shogunate_apparatus (payer): institutional power, trapped exit â dissolved as usurpers
 *   - domain_daimyo (payer): powerful regional actors, constrained exit â stripped of autonomous domains
 *   - status_quo_samurai (payer): moderate power, identity_locked â class abolished, privileges revoked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.75).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.6).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Imperial Mandate: Loyalist Restoration Reading").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political/philosophical/historical").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '3275983d-a56e-48b6-a540-1925745f8ef1').
narrative_ontology:cs_kernel_codification('3275983d-a56e-48b6-a540-1925745f8ef1', fixed_text).
narrative_ontology:cs_authority_grounding('3275983d-a56e-48b6-a540-1925745f8ef1', lineage).
narrative_ontology:cs_interpretation_layer_present('3275983d-a56e-48b6-a540-1925745f8ef1').
narrative_ontology:cs_reading_relation('3275983d-a56e-48b6-a540-1925745f8ef1', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('3275983d-a56e-48b6-a540-1925745f8ef1', foundational, divine_unmediated_mandate).
narrative_ontology:cs_axiom_status(divine_unmediated_mandate, holdable).
narrative_ontology:cs_axiom_grounding('3275983d-a56e-48b6-a540-1925745f8ef1', divine_unmediated_mandate, theological).
narrative_ontology:cs_axiom('3275983d-a56e-48b6-a540-1925745f8ef1', secondary, intermediary_governance_usurpation).
narrative_ontology:cs_axiom_status(intermediary_governance_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('3275983d-a56e-48b6-a540-1925745f8ef1', intermediary_governance_usurpation, deontological).
narrative_ontology:cs_reference_frame('3275983d-a56e-48b6-a540-1925745f8ef1', ancient_imperial_supremacy).
narrative_ontology:cs_drift_state('3275983d-a56e-48b6-a540-1925745f8ef1', post_restoration_oligarchy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3275983d-a56e-48b6-a540-1925745f8ef1', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restorationist_elites).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunate_apparatus).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, domain_daimyo).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, status_quo_samurai).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, sonno_joi_ideology).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, imperial_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lower-ranking court nobles and samurai activists who orchestrate the overthrow of the shogunate and construct a centralized state. They collect governing authority, military command, and tax revenues by enforcing the doctrine that sovereignty must be exercised directly by the emperor, while themselves administering the state in his name.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, restorationist_elites, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, restorationist_elites, beneficiary).

% The sovereign whose person becomes the unique locus of legitimate authority under this reading. He is elevated above all other political actors as the necessary source of law and governance, yet his actual decisions are prepared and executed by the restorationist oligarchy, binding him to a role he cannot renounce without annihilating the polity's legitimacy.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor, beneficiary,
    powerful, civilizational, identity_locked, national).

% The Tokugawa military government and its administrative retainers whose authority derived from a centuries-old delegation of practical governance. The restoration reading redefines this delegation as usurpation of divine prerogative, targeting the entire apparatus for dissolution, disarmament, and replacement.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunate_apparatus, payer,
    institutional, immediate, trapped, national).

% Feudal lords exercising autonomous governance over hereditary domains. The loyalist reading delegitimizes their authority as incompatible with unmediated imperial sovereignty, ultimately abolishing their domains and converting them into centrally administered prefectures under the new state.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, domain_daimyo, payer,
    powerful, generational, constrained, regional).

% Warriors whose social identity, economic stipends, and political function depended on the feudal hierarchy of delegated authority. The reading's enforcement dissolves their class privileges, pensioning some while reducing others to commoner status or driving them into armed revolt against the new order.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, status_quo_samurai, payer,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, restorationist_elites).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies fragmented sovereignty under a single sacred source to enable rapid national mobilization, modernization, and diplomatic engagement without the friction of feudal intermediaries.
% TRANSFER_FUNCTION: Moves governing authority, tax extraction, military command, and foreign-policy initiative from the shogunate and domain lords to the centralized imperial state administered by restorationist elites.
% ABSENT_VOICES: Bakufu loyalists who view the shogunate as the practical guarantor of order, and regional autonomy advocates who see domain-level governance as culturally legitimate, are excluded from the founding constitutional conversation; they are present only as targets of delegitimization, not as interlocutors.
% DISAPPEARANCE_RATIONALE: If the loyalist restoration reading vanished overnight, the shogunate's delegated authority would regain normative standing, domain autonomy would reassert itself, and the centralized Meiji state would fracture; the entire modern Japanese state structure was built on this reading's authority.
% FOUNDING_PROBLEM: Feudal fragmentation under the Tokugawa system had produced a decentralized polity unable to coordinate a unified response to Western imperial pressure, internal economic crisis, and the military weakness exposed by Perry's arrival.
% FOUNDING_PROBLEM_CORROBORATION: Foreign observers and Tokugawa reformers attested to the crisis of fragmentation and foreign threat. However, the specific claim that only unmediated imperial rule could solve it is corroborated only by the restorationist beneficiaries themselves; no party outside the benefiting set attests that direct emperor rule was the necessary solution, as opposed to bakufu-led centralization.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75 at interval end) because the reading demands the total transfer of governing authority from existing intermediaries to the imperial center; it is not merely a tax but an existential dissolution of status and function for the old elite. Suppression is high (peaking at 0.92) because the transformation required active civil war, domain abolition, and suppression of samurai revolt. Theater_ratio rises steadily (0.65 at end) because as the oligarchy consolidated, the emperor became increasingly a ceremonial figurehead while the constitution maintained the fiction of unmediated rule â the gap between the claim and the practice widened. Accessibility_collapse is high (0.85) because once the reading became state orthodoxy, delegated rule became literally unthinkable within Japanese constitutional discourse until 1945. Resistance is high (0.70) due to the Satsuma Rebellion and persistent samurai/daimyo opposition. The metrics are independent of the claimed_type: the constraint coordinates genuine modernization (justifying tangled_rope rather than snare) but the coordination is inseparable from the violent extraction required to build the centralized state.
 *
 * PERSPECTIVAL GAP:
 *   From the restorationist seat, the constraint is a necessary national salvation, a rope of coordination saving Japan from colonization. From the shogunate, daimyo, and samurai seats, it is a snare of usurpation destroying legitimate order. The emperor's seat is split: the theory names him supreme beneficiary, but the structural reality makes him a symbolic prisoner of the oligarchs â the engine will compute different directionalities for the named beneficiary (low d) and the actual captive (high d), a divergence this story documents rather than resolves.
 *
 * DIRECTIONALITY LOGIC:
 *   The restorationist_elites are declared agenda_setter with secondary beneficiary status: they direct the constraint and capture the extracted state capacity, so their derived d sits near the beneficiary end. The emperor is declared beneficiary under the reading's own logic, giving him low derived d; however, the commentary and omega note the instrumentalization ambiguity. The shogunate_apparatus, domain_daimyo, and status_quo_samurai are declared victims/payers: they lose authority, territory, and status, giving them high derived d. The automatic derivation produces strong seat divergence without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â feudal fragmentation and foreign threat â was substantially solved by the 1890s, yet the imperial sovereignty framework persisted and even intensified its theatricality (rising theater_ratio). Mandatrophy is resolved by the R5 fields: status=dead, verdict=world_rearranges, corroboration absent from outside beneficiaries. This flags the constraint as a zombie structure after its problem was solved, preventing misclassification as a live rope. It also prevents pure snare classification because the coordination function (modernization, national unity) was genuine and historically successful, not merely a cover story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unmediated_vs_delegated_sovereignty,
    'Is the imperial divine mandate structurally inseparable from active, unmediated governance, or can it legitimately operate through institutional delegation without constituting usurpation?',
    'Comparative historical analysis of pre-Tokugawa imperial practice versus Edo-period delegation; archaeological and textual evidence of the degree to which classical sovereignty was actually centralized or mediated through aristocratic and military houses.',
    'If delegation is historically legitimate, the loyalist reading is a novel construction rather than a restoration, and its delegitimization of the shogunate becomes a pretext for oligarchic capture rather than a return to authentic constitutional order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unmediated_vs_delegated_sovereignty, conceptual, 'Core kernel ambiguity between unmediated and delegated imperial sovereignty').

omega_variable(
    emperor_instrumentalization,
    'Does the requirement of unmediated imperial sovereignty empower the emperor as an autonomous political agent, or does it instrumentalize the throne as a non-refusable legitimizing device for oligarchic extraction?',
    'Archival analysis of imperial rescripts versus cabinet deliberations 1868-1890: measure the divergence between decisions attributed to the emperor and those initiated by the oligarchy.',
    'If the emperor is purely instrumental, the reading''s beneficiary structure is misaligned with its theory: extraction accrues to the agenda-setter, not the named beneficiary, and the constraint functions as a snare of the throne rather than a rope of national unity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emperor_instrumentalization, empirical, 'Whether the emperor is agent or artifact under this reading').

omega_variable(
    founding_problem_necessity,
    'Was the crisis of foreign pressure and feudal fragmentation soluble only through the abolition of intermediary governance, or could the Tokugawa bakufu have achieved equivalent centralization through reform?',
    'Counterfactual analysis of Tokugawa reform programs and comparison with contemporaneous centralizing reforms in non-imperial contexts.',
    'If the bakufu could have modernized without imperial restoration, the loyalist reading''s coordination function is separable from its extraction, confirming tangled rope classification; if not, the extraction may have been the necessary price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_necessity, empirical, 'Whether the extraction from old elites was necessary for modernization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impe_tr_t5, imperial_mandate__loyalist_restoration_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(impe_tr_t10, imperial_mandate__loyalist_restoration_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(impe_tr_t15, imperial_mandate__loyalist_restoration_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(impe_tr_t20, imperial_mandate__loyalist_restoration_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(impe_tr_t25, imperial_mandate__loyalist_restoration_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(impe_tr_t30, imperial_mandate__loyalist_restoration_reading, theater_ratio, 30, 0.63).
narrative_ontology:measurement(impe_tr_t35, imperial_mandate__loyalist_restoration_reading, theater_ratio, 35, 0.65).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(impe_be_t5, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(impe_be_t10, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(impe_be_t15, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 15, 0.82).
narrative_ontology:measurement(impe_be_t20, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(impe_be_t25, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(impe_be_t30, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(impe_be_t35, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 35, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(impe_su_t5, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(impe_su_t10, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 10, 0.92).
narrative_ontology:measurement(impe_su_t15, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(impe_su_t20, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(impe_su_t25, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(impe_su_t30, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(impe_su_t35, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 35, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel decomposes into two readings: bakufu_delegation_reading (delegated sovereignty) and loyalist_restoration_reading (unmediated sovereignty). They share the same divine-mandate kernel but assign opposite structural relationships to intermediary governance. Each reading has distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
