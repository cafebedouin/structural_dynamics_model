% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (1949) — Conditional Reciprocity Reading
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   The 1949 Geneva Conventions instantiate a framework of reciprocal
 *   restraint: belligerents who organize forces under unified command, wear
 *   distinctive insignia, carry arms openly, and respect the laws of war
 *   receive full protections (POW status, immunity from prosecution for
 *   lawful combat acts). This reading of the conventions — the
 *   conditional_reciprocity reading — treats protections as contingent on
 *   compliance with Article 4's organizational criteria. When belligerents
 *   cannot or will not meet these criteria (irregular forces, insurgencies,
 *   guerrillas), the framework permits degradation of protections: irregular
 *   combatants may be classified as unlawful combatants, detained without POW
 *   status, interrogated without safeguards, and subjected to broader force
 *   application justified by proportionality doctrine. This reading is
 *   structurally distinct from two sibling readings: the humanitarian_ceiling
 *   reading (protections should apply to all combatants regardless of
 *   organizational form, with only civilian immunity as the limiting
 *   principle) and the security_maximization reading (states should retain
 *   maximum flexibility to calibrate force based on threat assessment, with
 *   reciprocity as merely one policy option). The conditional_reciprocity
 *   reading occupies the middle position: it constrains state violence
 *   against organized opponents but permits controlled escalation against
 *   irregular ones. The constraint exhibits tangled_rope properties: genuine
 *   coordination function (predictable restraint among organized militaries
 *   reduces casualties and builds reciprocal expectation), but asymmetric
 *   extraction (irregular combatants and civilians in their vicinity bear the
 *   cost of the reciprocity gate, while organized states benefit from its
 *   protections).
 *
 * KEY AGENTS:
 *   - Organized state militaries meeting Article 4 criteria (institutional/arbitrage): Primary beneficiaries — receive full POW protections and reciprocal restraint from adversaries. Net position: beneficiary receiving protections commensurate with compliance.
 *   - Irregular combatants (powerless/trapped): Primary victims — unable to meet Article 4 criteria due to operational necessity or organizational form; classified as unlawful combatants with diminished protections. No exit from classification.
 *   - Civilian populations in conflict zones (moderate/constrained): Secondary victims — general prohibition on direct targeting (coordination benefit) but vulnerability to proportionality calculations and force escalation when irregular combatants operate nearby. Civilan immunity narrowed by circumstances.
 *   - Counterinsurgent states (institutional/constrained): Beneficiary and intermediate victim — benefit from reciprocal restraint against organized opponents but constrained by international law when facing irregulars; proportionality doctrine permits extraction of freedom from restraint.
 *   - ICRC and humanitarian law advocacy community (organized/constrained): Organized actors attempting to shift the constraint toward humanitarian_ceiling reading. Constrained by state sovereignty but applying pressure via customary law evolution, protocol adoption, and accountability mechanisms.
 *   - Analytical observer at civilizational level (analytical/analytical): Risks naturalizing the contingent reciprocity gate as a discovered law of armed conflict rather than a chosen institutional arrangement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.48).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.62).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (1949) — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '7f139ce3-0cdf-4565-8489-1009764b5ca9').
narrative_ontology:cs_kernel_codification('7f139ce3-0cdf-4565-8489-1009764b5ca9', formalized).
narrative_ontology:cs_authority_grounding('7f139ce3-0cdf-4565-8489-1009764b5ca9', lineage).
narrative_ontology:cs_interpretation_layer_present('7f139ce3-0cdf-4565-8489-1009764b5ca9').
narrative_ontology:cs_reading_relation('7f139ce3-0cdf-4565-8489-1009764b5ca9', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('7f139ce3-0cdf-4565-8489-1009764b5ca9', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('7f139ce3-0cdf-4565-8489-1009764b5ca9', foundational, organizational_discipline_prerequisite_for_full_protections).
narrative_ontology:cs_axiom_status(organizational_discipline_prerequisite_for_full_protections, holdable).
narrative_ontology:cs_axiom_grounding('7f139ce3-0cdf-4565-8489-1009764b5ca9', organizational_discipline_prerequisite_for_full_protections, empirically_contingent).
narrative_ontology:cs_axiom('7f139ce3-0cdf-4565-8489-1009764b5ca9', foundational, conditional_protections_preserve_reciprocal_incentives).
narrative_ontology:cs_axiom_status(conditional_protections_preserve_reciprocal_incentives, holdable).
narrative_ontology:cs_axiom_grounding('7f139ce3-0cdf-4565-8489-1009764b5ca9', conditional_protections_preserve_reciprocal_incentives, instrumental).
narrative_ontology:cs_reference_frame('7f139ce3-0cdf-4565-8489-1009764b5ca9', reciprocal_organizational_restraint).
narrative_ontology:cs_drift_state('7f139ce3-0cdf-4565-8489-1009764b5ca9', contemporary_post_2001_asymmetric_conflict, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7f139ce3-0cdf-4565-8489-1009764b5ca9', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries_meeting_article_4_criteria).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, organized_armed_forces_with_command_structure).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants_without_article_4_compliance).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_territories_with_irregular_force_presence).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, detained_persons_classified_as_unlawful_combatants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRREGULAR COMBATANT (SNARE) — Combatants unable to meet Article 4 criteria (organized command, distinctive insignia, carrying arms openly) or unable to comply due to operational necessity in asymmetric warfare face total loss of POW protections. No exit from the constraint. Maximum extraction: classified as unlawful combatant, detained indefinitely, subject to interrogation without safeguards, denied protections that organized militaries receive. Theater ratio reflects gap between humanitarian language and actual protections — the conventions promise restraint but operative framework permits degradation.
constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATION (TANGLED ROPE) — Civilians benefit from general prohibition on direct targeting (coordination function: all parties foreclose unlimited violence against non-combatants). But this protection is conditioned on the presence of organized opposition — if irregular forces operate among civilians, state forces can invoke proportionality exemptions and expand targeting justification. Civilan immunity is preserved in principle but narrowed by calculus of military advantage. Constrained by geography and inability to filter out irregular combatant presence. Mixed coordination (restraint on direct targeting) and extraction (vulnerability to collateral damage justification).
constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE MILITARY (ROPE) — States that maintain organized command structures, distinctive insignia, and open carrying of arms benefit from full POW protections and the credibility of reciprocal restraint. The constraint functions as pure coordination: agreement on the four-point Article 4 test enables predictable mutual restraint. State militaries experience the conventions as a coordination mechanism reducing uncertainty about treatment of their own prisoners. Arbitrage exit option: states can choose compliance level and adjust based on opponent behavior. Net beneficiary — receives protections commensurate with organizational compliance.
constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNTERINSURGENT STATE (TANGLED ROPE) — States fighting irregular opponents face operational pressure to relax constraints. The conditional reciprocity framework permits proportionality reasoning that degrades protections when facing non-uniformed combatants. Constrained by international law but with exits via proportionality doctrine and unlawful combatant classification. Coordination function: the state maintains restraint against organized enemies (Rope from Perspective 3) while extracting freedom from restraint against irregular ones. Mixed: the constraint both enables reciprocal restraint (beneficiary position toward organized opponents) and permits controlled escalation (beneficiary position toward irregular opponents).
constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IHL COMMUNITY (SCAFFOLD) — Organized advocates (ICRC, humanitarian NGOs, human rights bodies) view the conditional reciprocity framework as a transitional scaffolding that should sunset as international capacity for accountability grows. The framework restricts protections based on organizational form, but newer protocols (AP I and AP II) and customary law have shifted toward universal protection standards (combatant/civilian distinction independent of Article 4 compliance, protections for irregular combatants as long as they carry arms openly and respect distinction principle). The scaffold is eroding; the sunset is enforcement pressure via prosecution of violations and institutional accountability mechanisms that bypass the reciprocity gate.
constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, reciprocal restraint is presented as a natural feature of military organization: states that maintain discipline and uniform compliance can expect mutual restraint; forces that do not maintain organizational discipline cannot. This reading naturalizes the reciprocity gate as an inevitable law of armed conflict — organizational form maps to restraint eligibility as a matter of structural necessity, not contingent policy choice. However, the structural data indicates this is a false summit: the conventions explicitly codify reciprocity as a legal framework (chosen restraint), not as a discovered natural law. The engine's false summit detector will identify the naturalization of what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geneva_conventions_1949__conditional_reciprocity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The conditional reciprocity framework provides genuine coordination among organized militaries (lowering extractiveness from their perspective) but creates a systematic gate that excludes irregular forces from protections (raising extractiveness for those trapped outside the gate). The overall value reflects asymmetric benefit: organized states extract freedom from restraint against irregular opponents while coordinating restraint with organized peers. Extractiveness has risen over the interval (0.35 → 0.48) as asymmetric warfare has become dominant and the reciprocity gate has tightened in application. Suppression (0.62): Moderate-high. The frame is enforced through military law, tribunal jurisdiction, and credible threat of prosecution, but the operative mechanism is also internalized — irregular combatants often accept the classification as inevitable rather than challengeable. Suppression reflects both structural barriers (military enforcement, difficulty appealing classification) and cognitive capture (irregular forces internalize the narrative that non-compliance with Article 4 is their choice, not their circumstance). Theater ratio (0.58): Moderate. The conventions employ humanitarian language (protection of the wounded, humane treatment, distinction between combatants and civilians) but the operative gate (Article 4 compliance) is formalistic and permits degradation through proportionality reasoning. The gap between humanitarian promise and operational reality has widened as modern asymmetric conflicts have proliferated, making the theater dimension more pronounced. Theater_ratio increase (0.45 → 0.58) reflects this gap widening.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates strong perspectival divergence across power and exit dimensions. Organized state militaries (institutional/arbitrage) see a pure coordination mechanism (Rope) — their compliance with Article 4 brings predictable reciprocal restraint. Irregular combatants (powerless/trapped) see pure extraction (Snare) — no path to protections, no recognized combatant status. Civilian populations see mixed coordination and extraction (Tangled Rope) — general restraint on targeting balanced against proportionality exceptions. Counterinsurgent states see mixed benefit and constraint (Tangled Rope) — they benefit from reciprocal restraint against organized opponents but leverage proportionality to relax constraints against irregulars. The humanitarian law community sees a temporary arrangement requiring transition to humanitarian_ceiling standards (Scaffold) — newer protocols and customary law development are eroding the reciprocity gate. The civilizational analytical observer risks naturalizing the gate as immutable (Mountain), but the structural data reveals it as a chosen institutional arrangement. The perspectival gaps reveal the core tension: does reciprocal restraint require organizational discipline as a practical matter, or does insisting on Article 4 compliance simply exclude resistance movements from the constraints that bind organized states?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the extraction flow. Beneficiary-status agents (organized states) with arbitrage exit receive low d → negative or minimal chi (they see coordination, not extraction). Victim-status agents (irregular combatants) with trapped exit receive high d → high chi (they experience maximum extraction). Intermediate agents (civilians, counterinsurgent states) with constrained exit receive moderate d → moderate chi. The identity_locked exit option appears implicitly for some agents: irregular forces that have internalized the narrative that non-compliance with Article 4 is inevitable or justified may be structurally mobile (could reorganize, wear insignia, open-carry weapons) but cognitively captured by the framing that such compliance would make them vulnerable to targeting. This identity lock — the internalized acceptance of exclusion from protections — adds to suppression through cognitive capture rather than pure structural barriers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_4_structural_necessity_vs_contingency,
    'Does Article 4''s four-point test (organized command, distinctive insignia, carrying arms openly, respecting laws of war) represent structural necessities for effective restraint, or contingent institutional choices that exclude legitimate resistance forms?',
    'Comparative analysis of restraint effectiveness in wars with/without Article 4 compliance; examination of whether insurgencies meeting three Article 4 criteria but not one specific criterion (e.g., distinctive insignia in occupied territory with civilian clothing norms) could receive full protections without degrading restraint.',
    'If structural: conditional reciprocity reading is justified by necessity. If contingent: protections should be decoupled from Article 4 formalism, and humanitarian_ceiling_reading moves upstream. Classification shifts from tangled_rope toward piton (artifactual gatekeeping).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_structural_necessity_vs_contingency, conceptual, 'Whether Article 4 criteria are natural necessities or contingent institutional choices').

omega_variable(
    proportionality_calculation_opacity,
    'Does proportionality doctrine in the conditional reciprocity framework function as a principled restraint mechanism or as an undefined extraction escape hatch that permits force escalation?',
    'Analysis of proportionality calculations in tribunal decisions (ICTY, ICC); measurement of force escalation correlation with proportionality invocations; comparative case studies of proportionality reasoning in conflicts with/without organized opposition.',
    'If principled: proportionality is a genuine coordination mechanism and suppression score should be lower (0.45). If opacity enables escalation: proportionality is a theater mechanism and suppression score should be higher (0.72), pushing classification toward snare. The theater_ratio will rise if proportionality is systematically invoked but rarely challenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_calculation_opacity, empirical, 'Whether proportionality operates as restraint or permits escalation').

omega_variable(
    irregular_force_organizational_capacity,
    'In contemporary asymmetric conflicts, can irregular forces realistically achieve Article 4 compliance (organized command structure, distinctive insignia, openly carrying arms) without operational annihilation?',
    'Case analysis of insurgencies that attempted Article 4 compliance vs those that did not; correlation between compliance attempts and military survival; comparison of force preservation costs for compliance vs non-compliance strategies.',
    'If realistic: Article 4 test is achievable and conditional reciprocity provides pathway to protections. If unrealistic: Article 4 is a structural barrier to protections (not a gate but a wall), irregular forces are systematically excluded by design, and the framework is an extraction mechanism, not coordination. Classification would shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irregular_force_organizational_capacity, empirical, 'Whether irregular forces can realistically achieve Article 4 compliance').

omega_variable(
    humanitarian_ceiling_sibling_foreclosure,
    'Does the conditional reciprocity reading logically foreclose the humanitarian_ceiling reading (non-negotiable floor on protections regardless of opponent compliance)?',
    'Analysis of whether a single legal framework can hold both conditional_reciprocity and humanitarian_ceiling as simultaneous commitments; examination of treaty language and jurisprudence (AP I, customary law evolution, ICC statute) for evidence of foreclosure or coexistence.',
    'If foreclosed: the two readings cannot coexist in a single binding authority framework; one must dominate (binary choice at the state legal commitment level). If coexistent: both readings persist as live options (different states adopt different approaches), and the constraint family structure captures an ongoing dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_ceiling_sibling_foreclosure, conceptual, 'Whether conditional_reciprocity forecloses humanitarian_ceiling reading').

omega_variable(
    unlawful_combatant_classification_stability,
    'Is the ''unlawful combatant'' category a stable legal construct with consistent application, or does it function as an ad hoc extraction mechanism with shifting criteria?',
    'Corpus analysis of unlawful combatant designations in conflict zones post-2001; examination of consistency of criteria application across different states and conflicts; assessment of whether designation is reversible (can unlawful combatant status be challenged or disputed in binding proceedings).',
    'If stable: classification functions as a principled gate and suppression is partially offset by legal recourse. If ad hoc: the classification is theater (appears principled but lacks real constraint), theater_ratio rises, and suppression increases because designations are unreviewable. Impacts snare vs tangled_rope boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unlawful_combatant_classification_stability, empirical, 'Whether unlawful combatant classification is stable or ad hoc').

omega_variable(
    conditional_reciprocity_kernel_reading_status,
    'Is conditional_reciprocity one of three equally live readings of the 1949 Geneva Conventions kernel, or has authoritative interpretation migrated toward humanitarian_ceiling reading?',
    'Analysis of ICRC interpretation (official custodian), tribunal jurisprudence (ICTY, ICAD, ICC), state practice declarations, and customary law evolution post-1990. Measurement of which reading is endorsed by majority state practice and binding authority.',
    'If conditional_reciprocity is overridden by humanitarian_ceiling in binding authority: the reading''s status shifts from holdable to overridden axiom. If all three readings remain live: the sibling relations remain coexists_with. This determines the cs_structure.axioms status field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_reciprocity_kernel_reading_status, conceptual, 'Whether conditional_reciprocity reading remains live or has been overridden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geneva_1949_theater_initial, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(geneva_1990_theater_post_ap1, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(geneva_2000_theater_modern_asymmetric, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(geneva_1949_extractiveness_post_adoption, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geneva_1990_extractiveness_post_ap1_adoption, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(geneva_2000_extractiveness_post_modern_asymmetric_conflict, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(geneva_1949_suppression_initial, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(geneva_1990_suppression_post_ap1, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(geneva_2000_suppression_modern_asymmetric, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, unlawful_combatant_classification_regime).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, proportionality_doctrine_in_armed_conflict).

% DUAL FORMULATION NOTE:
% The 1949 Geneva Conventions kernel admits three structurally distinct readings with different extractiveness values. conditional_reciprocity_reading (ε=0.48, Tangled Rope) models restraint conditional on compliance. humanitarian_ceiling_reading (ε would be lower, closer to Rope) models protections as non-negotiable floor. security_maximization_reading (ε would be higher, closer to Snare) models maximum state flexibility. Each reading has distinct beneficiary/victim structures and distinct perspectives. They are linked via network.affects_constraints to show kernel family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__conditional_reciprocity_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
