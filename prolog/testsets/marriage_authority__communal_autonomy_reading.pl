% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Marriage Authority Grounded in Communal Religious Tradition (Autonomy Reading)
 *   domain: legal_pluralism/constitutional_law
 *
 * SUMMARY:
 *   A constitutionally pluralist state recognizes that minority religious
 *   communities have authority to govern family law (marriage, divorce,
 *   succession, guardianship) according to their own legal traditions. The
 *   state enforces these community norms through its courts but does not
 *   author them — it delegates the substantive rule-making to religious
 *   leadership. This constraint instantiates the communal autonomy reading of
 *   the contested marriage authority kernel: the reading that prioritizes
 *   community self-governance and religious freedom, and frames personal law
 *   pluralism as a consociational mechanism preventing majoritarian cultural
 *   domination. The sibling readings (secularist, gender-rights,
 *   federalist-millet, judicial-harmonization) contest whether this
 *   delegation is legitimate, whether it protects minorities or entrenches
 *   community elites, and whether the state's enforcement power neutrally
 *   supplements community autonomy or actively amplifies hierarchy within it.
 *
 * KEY AGENTS:
 *   - Religious community leadership: sets and administers family law norms; collects institutional legitimacy and enforcement power from state recognition
 *   - State judiciary: enforces community norms without authoring them; frames this as respecting pluralism and community autonomy
 *   - Intra-community dissenters: contest religious authority's interpretation; bound by identity fusion and social costs of exit
 *   - Gender equality advocates: push for norm reform through legislative amendment or judicial review; constrained by community consent requirements
 *   - Constitutional courts: review whether state deference violates constitutional rights; operate under pressure from competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.62).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.71).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority Grounded in Communal Religious Tradition (Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '463848a4-aee3-4dc3-b823-59d6a29c1ad0').
narrative_ontology:cs_kernel_codification('463848a4-aee3-4dc3-b823-59d6a29c1ad0', distributed).
narrative_ontology:cs_authority_grounding('463848a4-aee3-4dc3-b823-59d6a29c1ad0', lineage).
narrative_ontology:cs_interpretation_layer_present('463848a4-aee3-4dc3-b823-59d6a29c1ad0').
narrative_ontology:cs_reading_relation('463848a4-aee3-4dc3-b823-59d6a29c1ad0', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('463848a4-aee3-4dc3-b823-59d6a29c1ad0', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('463848a4-aee3-4dc3-b823-59d6a29c1ad0', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('463848a4-aee3-4dc3-b823-59d6a29c1ad0', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('463848a4-aee3-4dc3-b823-59d6a29c1ad0', foundational, religious_community_primary_authority).
narrative_ontology:cs_axiom_status(religious_community_primary_authority, holdable).
narrative_ontology:cs_axiom_grounding('463848a4-aee3-4dc3-b823-59d6a29c1ad0', religious_community_primary_authority, deontological).
narrative_ontology:cs_axiom('463848a4-aee3-4dc3-b823-59d6a29c1ad0', foundational, identity_constitutive_autonomy).
narrative_ontology:cs_axiom_status(identity_constitutive_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('463848a4-aee3-4dc3-b823-59d6a29c1ad0', identity_constitutive_autonomy, deontological).
narrative_ontology:cs_reference_frame('463848a4-aee3-4dc3-b823-59d6a29c1ad0', community_religious_authority_intact).
narrative_ontology:cs_drift_state('463848a4-aee3-4dc3-b823-59d6a29c1ad0', contemporary_constitutional_equality_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('463848a4-aee3-4dc3-b823-59d6a29c1ad0', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, gender_equality_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 by t=40) because community leadership benefits from state enforcement and community members bear the cost of norm compliance through identity-locked exit options. Suppression is high (0.71) because the constraint's persistence depends on enforcement of community norms against internal dissent, and dissent must be suppressed (through social/religious mechanisms backed by state law enforcement) for the autonomy to function. Theater is low-moderate (0.28): the 'community autonomy' rationale is genuine — there is real coordination value in allowing communities to govern intimate life — but that coordination function is increasingly used to justify resisting gender-equality reforms and maintaining hierarchical authority structures. The measurement series tracks a slow rise in extractiveness (t0=0.48 to t40=0.62) as judicial review intensifies and the constraint must work harder to suppress intra-community challenges, with theater rising as well (defensive framing about 'tradition' and 'community autonomy' becomes more prominent). The trajectory plateaus around t=25, suggesting the constraint has settled into a stable high-extraction equilibrium: leadership has adapted to resist reform pressure, and dissenters have adapted to constrained options.
 *
 * PERSPECTIVAL GAP:
 *   The community leadership and the state judiciary would compute this as a rope or even mountain (natural coordination) from their seats; intra-community dissenters and gender-equality advocates would compute it as a snare (pure extraction using autonomy framing as cover). The gap is structural, not empirical: the same constraint looks coordinative from the agenda-setter's position and extractive from the target's position because the beneficiary/victim asymmetry is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Community leadership collects authority and legitimacy without bearing the cost of norm enforcement against dissenters — that cost is borne by dissenters and the state apparatus. The leadership's exit option (arbitrage: threatening to withdraw cooperation if the state doesn't grant autonomy) is strong relative to their power level. Intra-community dissenters are trapped by identity fusion: their religious identity, family structure, social standing, and economic livelihoods are constituted through community membership; exit means losing all of these simultaneously. This is the deepest form of suppression — not external barriers but the dissenter's own identity forbidding exit. Gender-equality advocates are constrained but more mobile: they can litigate, organize, seek legislative remedy; the constraint's persistence requires their advocacy to be suppressed (through appeals to community autonomy and religious freedom that legitimize non-engagement). The asymmetry in power and exit is the source of the extraction — leadership benefits from a structure that dissenters cannot leave.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as pure extraction by carrying a genuine coordination function: multireligious democracies do have a real problem (majoritarian cultural domination of minorities), and personal law pluralism is one structural solution to it. What mandatrophy resolution reveals is that this function has become attenuated — the constraint is now used primarily to resist intra-community reform (gender equality, religious freedom for dissenters) and to entrench leadership authority. The founding problem (majoritarian domination) is increasingly contested: gender-equality advocates and secular constitutionalists argue that the problem is already solved by constitutional equal-protection guarantees, and that personal law autonomy now functions as an obstacle to those guarantees, not as their protector. This reading instantiates communal autonomy as the authoritative framing, but mandatrophy analysis clarifies that the constraint is being converted from a solution to majoritarian overreach into a mechanism for protecting community elites from equality review.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_mechanism_internalization,
    'Is the measured suppression of intra-community dissenters structural (legal barriers to exit, economic dependence) or substantially internalized (dissenters believe they deserve the treatment, have fused their identity with compliance, cannot imagine exit)?',
    'Post-exit trajectory analysis: if dissenters who physically leave the community report persistent suppression (continuing sense of shame, obligation, family rupture) years after exit, suppression is partially internalized. If exit eliminates suppression quickly, suppression is primarily structural. Interview studies with community leavers and longitudinal anthropological data.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure — the dissenter carries the suppression with them after exit, limiting the effectiveness of emigration as an exit option. This would raise the true target-ness of the dissenter''s position and raise the classification''s extraction score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mechanism_internalization, empirical, 'Whether suppression of dissenters is structural or internalized (identity-fusion mechanism)').

omega_variable(
    community_monolith_assumption,
    'Does the state treat ''the community'' as a monolithic entity with a single authentic voice (religious leadership), or does it recognize internal pluralism within the community (divergent theological schools, reformist movements, lay dissent)?',
    'Examination of state court judgments: do courts treat religious leaders as sole representatives of community interests, or do they permit intra-community litigation and recognize that communities contain competing voices? Comparison with jurisdictions (some Indian states) that have moved toward more pluralistic recognition of community diversity.',
    'If the state locks in the monolith assumption, it actively amplifies community leadership authority by denying judicial recognition to internal dissent. If the state treats communities as internally plural, dissenters gain some exit route (judicial appeal to state courts on grounds of intra-community cultural-rights violation). The constraint''s extractiveness and suppression would be lower under pluralistic recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_monolith_assumption, conceptual, 'Whether ''community autonomy'' presupposes a monolithic community or recognizes internal pluralism').

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (majoritarian cultural domination of minorities) still live and urgent, or has it been substantially solved by constitutional equal-protection guarantees and can now be treated as resolved?',
    'Historical analysis of legislative majoritarian overreach targeting minority communities'' family law; contemporary assessment of majoritarian threat vs. constitutional protection. Comparative study of jurisdictions with and without personal law pluralism to assess whether pluralism prevents majoritarian erasure or enables elite entrenchment.',
    'If the founding problem is dead, personal law pluralism shifts from a protective mechanism to a mechanism for leadership to resist equality review. The constraint would reclassify from rope (solving a real coordination problem) toward snare (using a defunct coordination rationale to justify extraction). If the founding problem is live, the coordination function remains central to the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem (majoritarian cultural domination) is still live or has been superseded by constitutional guarantees').

omega_variable(
    reading_identity_fusion_variant,
    'This reading (communal autonomy) depends on framing ''community'' as a locus of identity and belonging. But does the communal autonomy reading itself become a mechanism for fusing individual identity with community compliance, thereby internationalizing suppression?',
    'Discourse analysis of autonomy-framing rhetoric in state courts and community leadership. Interviews with dissenters about whether they experience their dissent as incompatible with their identity to the community. Comparison with the federalist-millet reading, which justifies pluralism on structural anti-tyranny grounds rather than identity grounds — does that reading produce less identity-internalized suppression?',
    'If the autonomy reading itself (by emphasizing identity and belonging) amplifies the internalization of suppression, then the reading is not merely describing a constraint but constitutively changing it — embedding the extraction more deeply in the dissenter''s self-concept. The constraint would have higher effective suppression under the autonomy reading than under alternative readings of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_fusion_variant, conceptual, 'Whether the autonomy reading itself amplifies identity-fusion suppression mechanisms').

omega_variable(
    sibling_reading_presence,
    'This constraint instantiates one reading of the marriage_authority kernel. What observable differences in ε, suppression, and type would appear if another reading (secularist, gender-rights, federalist-millet, or judicial-harmonization) were instantiated instead in this same constitutional context?',
    'Jurisdictional comparison: study cases where different readings dominate (India for autonomy reading, Tunisia or Turkey for secularist, Israel for judicial-harmonization, Bosnia for federalist-millet) and measure the constraint''s extractiveness and suppression under each. Not resolvable within one reading but observable via cross-reading comparison.',
    'Each sibling reading would show different ε values: secularist reading would eliminate personal law autonomy (eliminating this constraint entirely); gender-rights reading would maintain personal law but subject it to judicial review of equality compliance (lowering extractiveness and suppression); federalist-millet reading would maintain pluralism but legitimize it on anti-tyranny grounds rather than autonomy grounds (potentially lowering identity-fusion suppression). This omega documents the under-determination of this constraint by the kernel alone — the sibling readings exist to disambiguate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_presence, conceptual, 'Sibling readings of the marriage_authority kernel and their structural implications (reading under-determination)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__communal_autonomy_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__communal_autonomy_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__communal_autonomy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(marr_be_t5, marriage_authority__communal_autonomy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(marr_be_t15, marriage_authority__communal_autonomy_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(marr_be_t25, marriage_authority__communal_autonomy_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(marr_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(marr_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(marr_su_t5, marriage_authority__communal_autonomy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(marr_su_t15, marriage_authority__communal_autonomy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(marr_su_t25, marriage_authority__communal_autonomy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(marr_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(marr_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority kernel is contested across five readings, each instantiating a different constraint with different ε values, beneficiary structures, and type classifications. All five readings are linked in a kernel family: communal_autonomy_reading (this file) affects all sibling readings because it frames the kernel in terms of community autonomy and identity protection, which creates structural pressure on how sibling readings must position themselves (as either defending or reforming autonomy). The secularist_reading eliminates this constraint entirely (personal law abolished). The gender_rights_reading maintains personal law but subjects it to judicial equality review. The federalist_millet_reading maintains pluralism but justifies it on consociational anti-tyranny grounds rather than community autonomy. The judicial_harmonization_reading maintains plural authority but subjects it to incremental constitutional review. All five are one constraint family decomposed by the ε-invariance principle: each reading has different empirical consequences (different victim sets, different enforcement mechanisms, different founding-problem assumptions) and thus different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
