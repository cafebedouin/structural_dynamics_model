% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Imperial Mandate Through Bakufu Delegation (Bifurcated Sovereignty Reading)
 *   domain: political_philosophy/constitutional_systems
 *
 * SUMMARY:
 *   This reading instantiates ONE bifurcation of the imperial mandate kernel:
 *   Divine legitimacy operates through institutional delegation, separating
 *   the emperor's ritual/legitimacy function from the shogun's governing
 *   function. Under this reading, the mandate is preserved precisely by
 *   REMOVING the emperor from politics—the emperor's withdrawal becomes the
 *   condition of institutional stability. The emperor grants legitimacy but
 *   does not govern; the shogun governs but derives legitimacy only from
 *   imperial delegation. This reading sustained the bakufu system for nearly
 *   seven centuries (1185–1868 CE) until the Meiji Restoration inverted it by
 *   claiming the mandate required direct imperial sovereignty. The sibling
 *   reading (loyalist_restoration_reading) held that the mandate REQUIRES
 *   unmediated imperial governance—that delegation is a usurpation
 *   masquerading as institutional design. These readings coexist as live
 *   historical positions: the bakufu reading controlled institutional power;
 *   the loyalist reading drove periodic restoration movements. The
 *   claim/metric divergence is deliberate and structural: this reading is
 *   CLAIMED as tangled_rope (genuine coordination function + asymmetric
 *   extraction enforced by suppression of the alternative reading), and the
 *   metrics describe that structure.
 *
 * KEY AGENTS:
 *   - imperial_court_ritual_authority: Claims and retains legitimacy-granting supremacy at the cost of political agency; structured under this reading to accept permanent political subordination
 *   - shogun_administrative_authority: Receives delegated governing power and enforces the reading through institutional machinery; benefits from extraction legitimated by delegation theory
 *   - samurai_governing_class: Established as the legitimate governing stratum; extracts military taxes and status monopolies from commoners; depends on this reading for legitimacy
 *   - imperial_loyalist_faction: Holds the rival reading (mandate requires direct imperial governance); suppressed by bakufu enforcement; their voice is systematically excluded
 *   - regional_daimyo_autonomy: Retain regional control under bakufu supremacy; constrained by enforcement; represent potential challengers to the reading
 *   - cultivators_merchants_commoners: Bear extractive costs (land taxes, corvée labor, legal disability); invisible to the court-shogun relationship; trapped at the base of the legitimacy hierarchy
 *   - neo_confucian_scholars: Provide intellectual legitimacy for the reading; theorize bifurcated sovereignty and delegation legitimacy; occupy the analytical seat
 *   - bakufu_enforcement_apparatus: Maintains the reading through surveillance, punishment, and narrative control; must prevent loyalist restoration and police daimyo compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.68).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Imperial Mandate Through Bakufu Delegation (Bifurcated Sovereignty Reading)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/constitutional_systems").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, '79de1434-8692-4eb9-bb2c-7ad4064db4c0').
narrative_ontology:cs_kernel_codification('79de1434-8692-4eb9-bb2c-7ad4064db4c0', fixed_text).
narrative_ontology:cs_authority_grounding('79de1434-8692-4eb9-bb2c-7ad4064db4c0', extraction).
narrative_ontology:cs_interpretation_layer_present('79de1434-8692-4eb9-bb2c-7ad4064db4c0').
narrative_ontology:cs_reading_relation('79de1434-8692-4eb9-bb2c-7ad4064db4c0', imperial_mandate__loyalist_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('79de1434-8692-4eb9-bb2c-7ad4064db4c0', foundational, institutional_separability_of_legitimacy_governance).
narrative_ontology:cs_axiom_status(institutional_separability_of_legitimacy_governance, holdable).
narrative_ontology:cs_axiom_grounding('79de1434-8692-4eb9-bb2c-7ad4064db4c0', institutional_separability_of_legitimacy_governance, conventional).
narrative_ontology:cs_axiom('79de1434-8692-4eb9-bb2c-7ad4064db4c0', foundational, delegation_legitimates_subordinate_authority).
narrative_ontology:cs_axiom_status(delegation_legitimates_subordinate_authority, holdable).
narrative_ontology:cs_axiom_grounding('79de1434-8692-4eb9-bb2c-7ad4064db4c0', delegation_legitimates_subordinate_authority, deontological).
narrative_ontology:cs_axiom('79de1434-8692-4eb9-bb2c-7ad4064db4c0', secondary, samurai_natural_administrative_stratum).
narrative_ontology:cs_axiom_status(samurai_natural_administrative_stratum, overridden).
narrative_ontology:cs_axiom_grounding('79de1434-8692-4eb9-bb2c-7ad4064db4c0', samurai_natural_administrative_stratum, empirically_contingent).
narrative_ontology:cs_reference_frame('79de1434-8692-4eb9-bb2c-7ad4064db4c0', bifurcated_sovereignty_design).
narrative_ontology:cs_drift_state('79de1434-8692-4eb9-bb2c-7ad4064db4c0', meiji_restoration_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('79de1434-8692-4eb9-bb2c-7ad4064db4c0', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogun_administrative_authority).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_class).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court_political_agency).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, non_samurai_cultivators).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, regional_daimyo_autonomy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 (early bakufu, uncertain legitimacy, competing readings) to 0.71 (Tokugawa consolidation, reading solidified) and then falls slightly to 0.68 (late Edo, enforcement becoming theatrical as the founding problem is long solved). Suppression follows a similar trajectory, rising from 0.35 to 0.74 as the bakufu must work harder to suppress loyalist challenges and daimyo autonomy claims, then slightly declining as institutional fatigue sets in. Theater ratio is the most revealing metric: it rises from 0.12 (early bakufu, when delegation actually solved a governing problem) to 0.45 (late Edo, when the enforcement is mostly ceremonial maintenance of a centuries-old narrative). The measurements are authored on a single time grid spanning 683 years; each metric is valued at every examined time point. The trajectory shows Mandatrophy in real time: a reading that solved a founding coordination problem becomes a vehicle for extraction and suppression of alternatives, and eventually becomes mostly theater as the founding problem disappears but the enforcement persists.
 *
 * PERSPECTIVAL GAP:
 *   The imperial court and shogun compute entirely differently from this reading: the court experiences the constraint as political subordination (high d toward target), while the shogun experiences it as delegated authority (low d, near beneficiary). Samurai experience it as legitimated monopoly (low d). Commoners experience it as unquestionable hierarchy (high d, maximum target status). The bakufu enforcement apparatus experiences it as the reading it administers (symmetric d, neither benefiting nor bearing costs—they are the reading itself). Loyalist factions experience it as a usurpation disguised as institutional design (high d, constrained by suppression, invisible to the framework). The engine should compute fundamentally different types across these seats from the same structural data: what appears as rope to the shogun appears as snare to the court and commoners, and as mountain-status (inescapable hierarchy) to the loyalists under suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The imperial court is declared beneficiary (retains ritual supremacy, unquestioned legitimacy) but also victim (political agency suppressed, cannot govern). The shogun is beneficiary (receives delegated authority, legitimacy, extraction revenue). Samurai class is beneficiary (legitimated monopoly on governance). Commoners, daimyo, and loyalists are victims (extraction, subordination, suppression). Analytical scholars sit at d=0.5 (neither collecting nor paying; providing analysis that serves the bakufu but not coerced to do so). The directionality derivation should show: commoners at maximum d (powerless, trapped, bearing all extraction costs, no visible benefit); court at high-middle d (institutional power but political suppression, identity-locked); shogun at low d (beneficiary, architect of the constraint); samurai at low-middle d (beneficiary but constrained by bakufu hierarchy). No overrides are necessary—the structural data (beneficiary/victim + power + exit) determines the directionality correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows full mandatrophy: founding_problem_status=dead, disappearance_verdict=world_rearranges. The founding problem (fragmented regional power, imperial court administrative collapse) was solved by bakufu consolidation within ~150 years (by 1330 CE). Yet the reading and its enforcement machinery persisted for another 538 years (1330–1868 CE) with no connection to the original problem. The theater_ratio drift tells the story: early theater_ratio (0.12–0.28) represents real functional coordination; late theater_ratio (0.39–0.45) represents mostly ceremonial maintenance. The reading became a vehicle for samurai class monopoly, imperial court political neutralization, and extraction from commoner classes—all justified by an answer to a problem no longer live. The Meiji Restoration inverted the reading by claiming the mandate REQUIRED direct imperial governance, and the bakufu reading collapsed immediately when challenged because it had no functional foundation remaining, only the enforcement power and theatrical narrative. The classification as tangled_rope (not pure snare) is justified by the genuine coordination function the reading originally solved, but the mandatrophy flags that the coordination function has atrophied while the extraction and suppression machinery persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_separability_naturalness,
    'Is the separation of legitimacy-granting from governing function a natural institutional limit (the constraint is a Mountain grounded in the logic of stable delegation), or is it a constructed constraint that benefits identifiable parties (the bakufu, the samurai class)?',
    'Comparative institutional analysis: do other cultures and periods solve the same coordination problem (stable succession and institutional continuity) require the same separation, or do alternative institutional designs (direct imperial governance with strong succession rules, electoral legitimacy transfer, etc.) achieve the same stability without requiring political neutralization of the ritual authority?',
    'If the separation is natural/necessary, the constraint is a genuine coordination mechanism and some extraction is legitimate cost-of-governance. If the separation is constructed and benefits particular parties, the reading is revealed as extractive capture disguised as institutional necessity—shifting the classification toward snare and flagging samurai monopoly as the real function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_separability_naturalness, conceptual, 'Whether institutional separability of legitimacy and governance is a natural law or a constructed constraint benefiting the bakufu and samurai.').

omega_variable(
    court_political_agency_suppression_mechanism,
    'Is the imperial court''s political neutrality maintained through external force (bakufu enforcement and threat), or through internalized acceptance of the reading''s legitimacy (the court genuinely believes separation is divinely ordained)?',
    'Historical evidence of court resistance, escape attempts, or secret organizing; post-bakufu collapse analysis of whether the court rapidly reasserts political power (indicating internalized suppression) or reiterates the legitimacy of delegation (indicating genuine commitment). The answer determines whether measured suppression is primarily structural (external) or partially internalized.',
    'If suppression is primarily structural, the court carries low effective suppression after the bakufu is removed and would rapidly reassert political agency (what actually occurred: the Meiji Restoration). If suppression is primarily internalized, the court would carry suppression with them even after the bakufu enforcement is gone—they would believe their political neutrality is legitimate. Evidence: the court did NOT reassert direct governance after Meiji; instead they accepted the Meiji reading that the mandate requires direct imperial sovereignty, then delegated to constitutional governance. This suggests the suppression was structurally enforced, not internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(court_political_agency_suppression_mechanism, empirical, 'Whether imperial court suppression is external (bakufu enforcement) or internalized (court''s own belief in the reading''s legitimacy).').

omega_variable(
    loyalist_reading_structural_alternative,
    'What would the institutional structure look like under the loyalist_restoration_reading (mandate requires direct imperial governance)? Would it be stable, or would it require the same suppression machinery that the bakufu reading uses?',
    'Comparative analysis of the Meiji Restoration (direct imperial restoration) and early Meiji governance: did the shift to direct imperial sovereignty eliminate the need for enforcement/suppression, or did it simply redirect enforcement toward different constraints (constitutional limits on imperial power, military hierarchy, etc.)?',
    'If loyalist reading would also require suppression machinery, both readings are tangled_rope and the constraint is fundamentally about power distribution (bakufu control vs. imperial control), not about natural institutional limits. If loyalist reading would be stable without enforcement, then the bakufu reading''s suppression is evidence of constructed extraction rather than coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(loyalist_reading_structural_alternative, conceptual, 'Whether the rival loyalist reading is a structurally viable alternative or would also require enforcement and suppression.').

omega_variable(
    samurai_class_necessity_for_delegation,
    'Is the samurai class''s legitimation as the governing stratum a logical consequence of the delegation reading, or is it an additional extracted benefit layered onto the reading?',
    'Counterfactual institutional analysis: could delegation operate with samurai as administrative agents (not legitimated as natural governing class) vs. samurai as legitimated monopoly on political authority? What would change in the constraint''s operation if commoners could aspire to governance roles?',
    'If samurai legitimation is logically inseparable from delegation, then some samurai extraction is coordination cost. If samurai legitimation is an additional benefit layered onto the reading, then samurai extraction (taxes, labor obligations, status monopolies) is pure capture and should be classified under snare rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(samurai_class_necessity_for_delegation, conceptual, 'Whether samurai class legitimation is inherent to the delegation reading or an additional extracted benefit.').

omega_variable(
    reading_vs_sibling_foreclosure_test,
    'Does the bakufu_delegation_reading logically foreclose the loyalist_restoration_reading, or do they coexist as genuinely incompatible but independently holdable positions?',
    'Logical analysis of the core premises: bakufu reading claims ''the mandate operates through delegation and separation is divinely ordained.'' Loyalist reading claims ''the mandate requires direct exercise of sovereign power.'' These claims directly contradict each other about what the mandate IS. If they are logically incompatible, the relation should be ''forecloses.'' If they are incompatible empirically but not logically (both could be true under different interpretations of the mandate), the relation is ''coexists_with.''',
    'If the readings logically foreclose each other, then institutional conflict is inevitable and the bakufu''s suppression of loyalism is suppression of a logically ruled-out position. If they coexist as live alternatives, then suppression is suppression of genuine alternatives and is revealed as non-deductive exercise of power—flagging the bakufu reading as more snare-like (suppression of genuine alternatives) than tangled_rope (coordination + enforcement of asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure_test, conceptual, 'Whether the bakufu delegation reading logically forecloses the loyalist restoration reading, or they coexist as incompatible but independently holdable positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1185, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1185, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1185, 0.12).
narrative_ontology:measurement(impe_tr_t1330, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1330, 0.18).
narrative_ontology:measurement(impe_tr_t1530, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1530, 0.28).
narrative_ontology:measurement(impe_tr_t1700, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1700, 0.39).
narrative_ontology:measurement(impe_tr_t1800, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1800, 0.45).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.42).

% Extraction over time
narrative_ontology:measurement(impe_be_t1185, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1185, 0.42).
narrative_ontology:measurement(impe_be_t1330, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1330, 0.55).
narrative_ontology:measurement(impe_be_t1530, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1530, 0.62).
narrative_ontology:measurement(impe_be_t1700, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1700, 0.68).
narrative_ontology:measurement(impe_be_t1800, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1800, 0.71).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1185, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1185, 0.35).
narrative_ontology:measurement(impe_su_t1330, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1330, 0.48).
narrative_ontology:measurement(impe_su_t1530, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1530, 0.61).
narrative_ontology:measurement(impe_su_t1700, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1700, 0.69).
narrative_ontology:measurement(impe_su_t1800, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1800, 0.74).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imperial_mandate__bakufu_delegation_reading, 0.18).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% The imperial_mandate kernel decomposes into two structurally distinct constraints: bakufu_delegation_reading and loyalist_restoration_reading. They are sibling readings of the same kernel, holding incompatible answers to the question 'How does divine mandate operate in institutional governance?' The bakufu reading (this story) claims bifurcated sovereignty with institutional delegation; the loyalist reading claims direct imperial sovereignty. The readings are linked via network.affects_constraints because the bakufu reading's suppression of loyalist alternatives directly constrains the loyalist reading's emergence as institutional authority. The ε values differ substantially: the bakufu reading shows high extraction and suppression (0.68, 0.71) because it requires active institutional suppression of the alternative; the loyalist reading would show lower suppression (enforcement would be normal governance) but also lower coordination function (direct imperial rule does not solve the succession/institutional-continuity problem the bakufu reading addresses). These are not the same constraint viewed from different angles—they have different structural relationships, different beneficiaries, different victims, and different ε properties. They are two distinct constraints that jointly instantiate the mandate kernel's contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imperial_mandate__bakufu_delegation_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
