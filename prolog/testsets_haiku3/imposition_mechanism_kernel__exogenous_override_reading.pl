% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: imposition_mechanism_kernel__exogenous_override_reading
 *   human_readable: State-Coerced Norm Imposition (Exogenous Override Reading)
 *   domain: political/sociological
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous override reading of the
 *   imposition_mechanism_kernel. The reading asserts that new behavioral
 *   norms imposed by state authority derive their persistence NOT from
 *   cultural acceptance or endogenous adoption, but from the state's monopoly
 *   on violence and coercive capacity. Compliance is conditional on
 *   enforcement; resistance suppresses where monitoring weakens. The state
 *   authority benefits from behavioral standardization that serves
 *   administrative or military objectives. Populations and cultural
 *   practitioners bear the cost of suppressed cultural practice and the
 *   constant threat of punishment. This reading treats legitimacy as
 *   overridden rather than earned—the state declares legitimacy through fiat
 *   and enforces it through threat, not through cultural consensus.
 *
 * KEY AGENTS:
 *   - state_authority: institutional agenda-setter, monopoly on violence, collects compliance through coercion
 *   - norm_target_populations: powerless, trapped, pay through behavioral conformity under threat
 *   - cultural_practitioners: moderate power, constrained exit, lose moral authority within communities
 *   - enforcement_apparatus: institutional beneficiary (gains budget and authority from enforcement)
 *   - norm_resisters: identity-locked, excluded from legitimacy determination, experience highest personal cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, 0.79).
domain_priors:suppression_score(imposition_mechanism_kernel__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(imposition_mechanism_kernel__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__exogenous_override_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_mechanism_kernel__exogenous_override_reading, "State-Coerced Norm Imposition (Exogenous Override Reading)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__exogenous_override_reading, "political/sociological").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__exogenous_override_reading, 'c7b9c68a-4fc5-4ded-9076-2d055f95b92d').
narrative_ontology:cs_kernel_codification('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', formalized).
narrative_ontology:cs_authority_grounding('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', extraction).
narrative_ontology:cs_interpretation_layer_present('c7b9c68a-4fc5-4ded-9076-2d055f95b92d').
narrative_ontology:cs_reading_relation('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', imposition_mechanism_kernel__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', foundational, legitimacy_derives_from_monopoly_on_violence).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_monopoly_on_violence, holdable).
narrative_ontology:cs_axiom_grounding('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', legitimacy_derives_from_monopoly_on_violence, empirically_contingent).
narrative_ontology:cs_axiom('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', foundational, compliance_conditional_on_enforcement_threat).
narrative_ontology:cs_axiom_status(compliance_conditional_on_enforcement_threat, holdable).
narrative_ontology:cs_axiom_grounding('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', compliance_conditional_on_enforcement_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', violent_monopoly_legitimacy).
narrative_ontology:cs_drift_state('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', contemporary_cultural_contestation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c7b9c68a-4fc5-4ded-9076-2d055f95b92d', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, state_authority).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, norm_target_populations).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__exogenous_override_reading, cultural_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally declares and enforces new behavioral norms through legal prohibition, administrative decree, or military enforcement. Claims legitimacy derives from legal sovereignty and monopoly on violence. Maintains the norm through inspection, punishment, and suppression of contrary practice. Benefits from behavioral standardization that serves state interests (taxation, military recruitment, administrative efficiency, social control). Does not require cultural acceptance—coercion itself IS the enforcement mechanism.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to the new norm. Compliance is demanded under threat of punishment (fines, imprisonment, physical punishment, land seizure, conscription). Exit from the jurisdiction is blocked by borders, kinship obligations, or lack of resources. Compliance is conditional on state monitoring and threat—resistance persists where monitoring is weak. The norm contradicts existing cultural practices or beliefs, creating constant friction between private preference and public requirement.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, norm_target_populations, payer,
    powerless, biographical, trapped, national).

% Holders of prior cultural norms that the state mandate supersedes (e.g., religious authorities, traditional leaders, craft guilds, family patriarchs). Their authority to interpret and enforce norms within their domain is overridden by state decree. They must either enforce the state norm against their own adherents or face punishment themselves. They bear the cost of legitimacy collapse within their communities—loss of moral authority, community fracture, demands from followers to resist.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, cultural_practitioners, payer,
    moderate, generational, constrained, regional).

% Police, military, courts, and administrative inspectors gain expanded authority, budget, and institutional power from norm enforcement. The need for constant monitoring and punishment creates career advancement and resource justification. The apparatus executes the imposition but does not set the norm—it operates under state command. Its beneficiary status is instrumental: it gains power as a means to enforce the agenda, not as an end it sought.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, enforcement_apparatus, beneficiary,
    institutional, biographical, constrained, national).

% Community members or subgroups whose identity is constituted through the prohibited practice (religious practice, artistic tradition, kinship obligation, occupational identity). They face the highest cost to comply—compliance means severing identity bonds. They are excluded from the formal legitimacy conversation; the state does not consult them or treat their norms as valid alternatives. Their resistance is treated as deviance rather than as a legitimate alternative framework.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, norm_resisters, excluded,
    powerless, biographical, identity_locked, regional).

% The abstract question of what grounds legitimate authority. This reading instantiates the claim that legitimacy derives from monopoly on violence and coercive capacity rather than from cultural consensus or procedural fairness. Other readings contest this claim.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__exogenous_override_reading, competing_legitimacy_claims, observer,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(imposition_mechanism_kernel__exogenous_override_reading, competing_legitimacy_claims).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__exogenous_override_reading, state_authority).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes behavior across a jurisdiction to serve administrative, military, or economic state objectives (e.g., uniform language for tax collection, standardized measurement for commerce, conscription for military service). The coordination function is real but serves the state's operational capacity, not the population's preference.
% TRANSFER_FUNCTION: Transfers behavioral conformity from the population (in the form of suppressed alternative practice and compliance labor) to the state authority, which gains the standardized, controllable population it requires for governance. The transfer is enforced, not negotiated.
% ABSENT_VOICES: Communities and individuals whose cultural norms are overridden are excluded from the legitimacy determination. Religious authorities, traditional leaders, and cultural practitioners are not consulted on whether the new norm is justified—they are informed of it through enforcement. Resistance is treated as deviance rather than as a voice in a legitimate contest.
% DISAPPEARANCE_RATIONALE: If state enforcement of the norm vanished overnight, suppressed cultural practices would resurface within weeks in unmonitored spaces and would persist indefinitely where enforcement capacity remained weak. The population would revert to prior norms that carry actual cultural legitimacy. The norm persists only through continuous threat, not through internalized acceptance.
% FOUNDING_PROBLEM: The state requires behavioral standardization (unified language, uniform weights and measures, military conscription, tax collection procedures) that prior cultural heterogeneity made impossible. Decentralized cultural norms did not provide the uniformity the state's administrative machinery demands.
% FOUNDING_PROBLEM_CORROBORATION: The state authority and its administrative allies attest the founding problem is live and ongoing—standardization is necessary for governance. Cultural practitioners and populations under the norm attest the problem was artificially constructed—the state created a need for uniformity in order to extend its control, not because heterogeneity posed a genuine coordination failure. Historians studying norm imposition (outside the state authority's commission) document cases where prior heterogeneous arrangements functioned adequately until state centralization demanded uniformity as an explicit goal of consolidation.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__exogenous_override_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.79 because the constraint transfers behavioral conformity from a population that does not consent and would not choose compliance absent coercion. Suppression is high (0.87) because enforcement capacity and threat are the ONLY mechanism maintaining compliance; if coercion were removed, the norm would collapse immediately in all unmonitored spaces. Theater ratio is high (0.62) because as the norm persists over decades, the state develops ceremonial justifications (celebration of the norm as 'tradition,' official histories teaching the norm as culturally rooted) that obscure the naked coercive origin. The measurement trajectory shows extractiveness rising as the state invests in enforcement capacity to suppress emergent resistance; suppression also rises as the apparatus hardens. Theater rises fastest—by year 50, the state has invested substantially in theatrical legitimation (monument building, curriculum insertion, official narratives) to create the appearance of cultural acceptance it cannot achieve through actual consent.
 *
 * PERSPECTIVAL GAP:
 *   The state authority and its enforcement apparatus perceive the constraint as a coordination mechanism that solved a real problem (administrative standardization). The populations and cultural practitioners perceive it as pure extraction maintained by threat. The engine computes different type classifications from the two seats: from the agenda-setter's seat, the coordination function is real and the enforcement is justified; from the payer seats, the coordination function is a cover story and the arrangement is a snare. This divergence is the measurement the reading exists to expose—it is not an error in the authored claim, but the central finding.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority is the full beneficiary (d = 0.0): it gains behavioral standardization, population controllability, and simplified administration. It has maximum exit freedom (arbitrage: it can change the norm anytime, can exit the jurisdiction whenever it chooses, faces no cost for non-compliance). Norm-target populations are full targets (d = 1.0): they pay compliance labor, face punishment for non-compliance, cannot exit the jurisdiction, and have their own cultural norms suppressed. Cultural practitioners sit between (d = 0.7-0.8): they are partly victimized (loss of moral authority, community fracture) and partly agents of the imposition (forced to enforce the norm on their own communities). Enforcement apparatus is a secondary beneficiary (d = 0.2): it gains power and budget but operates under state command and has constrained exit (institutional role dependence). Norm resisters are full targets (d = 1.0): they face the highest personal cost (identity severance) and are most forcefully suppressed. The directionality derivation from beneficiary/victim + exit options produces these values automatically; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The exogenous override reading declares this constraint a snare: pure extraction (behavioral conformity transferred to state authority) justified by a coordination story (standardization solves administrative problems) that is itself contested. The mandatrophy test asks: does the constraint's mandate persist beyond its function? The founding_problem_status = 'contested' and the high theater_ratio (0.62) indicate the mandate IS persisting beyond function—the state has invested in theatrical legitimation precisely because the real coordination function is insufficient to maintain compliance. If the administrative problem the norm was built to solve were truly live and urgent, populations would need less theatrical persuasion to accept it. The rising theater_ratio over the interval (0.38 → 0.62) is a symptom of mandatrophy: as actual compliance pressures ease (extractiveness stable at 0.78-0.79 in years 35-50), the state increases theatrical activity (monuments, curriculum, official histories) to create the appearance of legitimacy it lost when the original administrative crisis passed. This is Piton-adjacent, but the constraint is correctly classified as Snare because resistance remains high (0.74) and extraction is still substantial—the theater is masking continued extraction, not masking atrophied function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resistance_suppression_internalization,
    'As suppression persists over decades (interval 0-50), does resistance decline because external coercive pressure forces genuine internalization, or does resistance remain high and only its expression becomes hidden?',
    'Post-enforcement observation: if the norm persists after state monitoring is suddenly removed (e.g., state collapse, regime change, geographic escape), the suppression was structural. If the norm persists even after enforcement ceases, suppression was internalized. Historical cases of regime change provide natural experiments: do populations revert to suppressed norms when the threat is gone?',
    'If suppression remains structural (external), the constraint is correctly classified as Snare—extraction maintained by coercion. If suppression becomes internalized, parts of the constraint migrate toward Rope or Tangled Rope (cultural acceptance eventually achieved). If mixed (internalized in some populations, structural in others), the constraint exhibits seat-dependent types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_suppression_internalization, empirical, 'Whether suppression that persists for 50 years becomes internalized cultural acceptance or remains conditional on external threat.').

omega_variable(
    administrative_problem_artificiality,
    'Was the administrative standardization problem the state invoked as justification for norm imposition a genuine coordination failure in prior heterogeneous arrangements, or was it an artificial problem created as justification for centralization that was desired for other reasons (military power, tax revenue, territorial control)?',
    'Comparative historical analysis: examine cases where heterogeneous cultural norms persisted without state standardization and ask whether administrative/commercial/military functions failed. Compare pre-standardization periods with post-standardization periods on metrics of governance efficiency, revenue collection, military effectiveness. Examine state archives for evidence of whether standardization was pursued as a solution to a recognized problem or as an explicit goal to extend control.',
    'If the problem was genuine, the coordination function is real and the constraint may be correctly classified as Tangled Rope (real coordination + extraction). If the problem was artificial, the coordination function is a cover story and the constraint is correctly classified as Snare (pure extraction with a justification narrative). If mixed, the constraint reflects the committer''s framing choice—the exogenous reading instantiates the ''artificial problem'' framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(administrative_problem_artificiality, conceptual, 'Whether the state''s stated administrative problem was a genuine coordination failure or a constructed justification for centralization desired for independent reasons.').

omega_variable(
    kernel_alternative_readings_mutual_exclusivity,
    'Is the exogenous override reading''s core claim—that legitimacy derives from monopoly on violence rather than cultural acceptance—logically incompatible with the endogenous climb reading''s core claim—that norms achieve legitimacy through bottom-up adoption? Or can both readings coexist for different populations or different norms?',
    'Logical analysis: the claims appear contradictory (legitimacy comes FROM coercion vs. FROM cultural acceptance). Test via natural experiments: can a single norm be exogenously imposed in one population while simultaneously adopted endogenously in another? Or does the exogenous imposition in the first population preclude and suppress endogenous adoption in the second? The mechanism of suppression will answer the question—if exogenous imposition actively excludes endogenous alternatives, the readings foreclose each other. If exogenous imposition overlays but does not prevent endogenous adoption, they coexist.',
    'If exogenous and endogenous foreclose each other, cs_structure.reading_relations declares forecloses. If they coexist (different populations, different norms, different time periods), declare coexists_with. If the hybrid reading is structurally intermediate, it may influence both or coexist with both depending on the dominant mechanism in specific cases. The reading_relations for this story will determine how the engine treats the kernel contest during constraint family analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_alternative_readings_mutual_exclusivity, conceptual, 'Whether the exogenous override and endogenous climb readings are logically incompatible (one true, other false) or can both be true for different populations/norms/time periods.').

omega_variable(
    behavioral_vs_belief_constraint_boundary,
    'This story measures extractiveness on behavioral conformity (populations comply under threat). But does genuine cultural norm internalization require belief agreement, or is behavioral compliance sufficient to establish a ''norm'' in the analytical sense? Is this constraint measuring norm imposition or behavioral regulation?',
    'Disciplinary analysis: sociology, anthropology, and ethics define ''norm'' differently. Some traditions define norms behaviorally (action regularities enforced by sanctions); others define norms as beliefs that actions are appropriate and should be performed. If behavioral compliance without belief is sufficient, the constraint succeeds at norm imposition (though legitimacy remains contested). If belief change is required, the constraint fails at norm imposition and is instead behavioral regulation maintained by coercion.',
    'If behavioral compliance is sufficient, the constraint classification (Snare) stands; the ''norm'' is established even without cultural acceptance. If belief is required, the story is measuring the FAILURE of norm imposition—the constraint maintains behavior but not the norm itself. The distinction affects how the constraint is read in policy contexts: can you impose a ''norm'' by enforcement alone, or does enforcement-only produce transient behavioral change that collapses when enforcement is removed?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_belief_constraint_boundary, conceptual, 'Whether norm imposition requires behavioral compliance alone or also requires belief change and cultural internalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(impo_tr_t8, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(impo_tr_t16, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement(impo_tr_t25, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(impo_tr_t35, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 35, 0.61).
narrative_ontology:measurement(impo_tr_t50, imposition_mechanism_kernel__exogenous_override_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(impo_be_t8, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement(impo_be_t16, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(impo_be_t25, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(impo_be_t35, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 35, 0.78).
narrative_ontology:measurement(impo_be_t50, imposition_mechanism_kernel__exogenous_override_reading, base_extractiveness, 50, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(impo_su_t8, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(impo_su_t16, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 16, 0.8).
narrative_ontology:measurement(impo_su_t25, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 25, 0.84).
narrative_ontology:measurement(impo_su_t35, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 35, 0.86).
narrative_ontology:measurement(impo_su_t50, imposition_mechanism_kernel__exogenous_override_reading, suppression_requirement, 50, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(imposition_mechanism_kernel__exogenous_override_reading, 0.18).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__exogenous_override_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel_constraint family decomposes a single sociohistorical question into three structurally distinct claims about how norms become established and acquire legitimacy. The exogenous_override_reading asserts legitimacy derives from monopoly on violence and coercive capacity. The endogenous_climb_reading asserts legitimacy derives from bottom-up cultural adoption. The hybrid_legitimation_reading asserts legitimacy derives from symbolic authority transfer and institutional incentive design. Each reading produces a different ε value, different victim/beneficiary structure, and different classification. They are linked via the kernel contest, not as perspectives on a single mechanism, but as alternative mechanisms competing for explanatory power. The exogenous reading influences both siblings by constraining the legitimacy conditions available to them—if the state successfully suppresses alternative norm-formation mechanisms, it reduces the empirical possibility of endogenous or hybrid processes. Conversely, evidence of hybrid or endogenous processes in the historical record challenges the exogenous reading's claim to be THE mechanism of norm establishment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
