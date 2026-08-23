% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Colorblind Equal Protection — No State Racial Classification
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause holds that the
 *   Constitution forbids all state use of racial classification, rooted in
 *   Justice Harlan's Plessy dissent: 'Our Constitution is color-blind, and
 *   neither knows nor tolerates classes among citizens.' This reading treats
 *   the prohibition on racial classification as a structural feature of
 *   constitutional law — a mountain — that would persist regardless of who
 *   defends it. However, identifiable beneficiaries exist (advocates of
 *   formal equality, Asian and white applicants disadvantaged by
 *   race-conscious admissions) and victims (race-conscious programs,
 *   institutions seeking to use racial classification for diversity or
 *   remediation). The constraint's extractiveness has risen from negligible
 *   (0.15 in 1896, when Plessy upheld segregation) to moderate-high (0.42 in
 *   2023, after SFFA v. Harvard/UNC), as the colorblind principle was
 *   redeployed against remedial measures. Suppression requirement has grown
 *   as courts actively strike down race-conscious programs. Theater ratio
 *   remains low but rising: the formalist rhetoric persists while the
 *   constraint's actual operation increasingly blocks remedial measures.
 *
 * KEY AGENTS:
 *   - colorblind_principle_advocates: Primary beneficiary (organized/arbitrage) — advances formal equality principle, collects doctrinal victories
 *   - asian_white_applicants: Primary target/beneficiary (moderate/constrained) — denied admission under race-conscious policies; benefits from colorblind enforcement
 *   - implementing_institutions: Agenda setter/payer (institutional/constrained) — universities, employers, government agencies that want to use racial classification; bear compliance costs
 *   - race_conscious_programs: Victim (organized/trapped) — affirmative action, diversity initiatives, remedial programs; structurally suppressed by the constraint
 *   - diversity_remedial_advocates: Excluded (powerful/trapped) — civil rights organizations, legal scholars; would object but are structurally excluded from the colorblind framework
 *   - supreme_court: Observer/agenda setter (institutional/analytical) — adjudicates the constraint, authoritatively instantiates the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.58).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, mountain).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Colorblind Equal Protection — No State Racial Classification").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).
domain_priors:emerges_naturally(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '715e4f6d-fee2-4867-8855-a55b3a79d9ed').
narrative_ontology:cs_kernel_codification('715e4f6d-fee2-4867-8855-a55b3a79d9ed', fixed_text).
narrative_ontology:cs_authority_grounding('715e4f6d-fee2-4867-8855-a55b3a79d9ed', lineage).
narrative_ontology:cs_interpretation_layer_present('715e4f6d-fee2-4867-8855-a55b3a79d9ed').
narrative_ontology:cs_reading_relation('715e4f6d-fee2-4867-8855-a55b3a79d9ed', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('715e4f6d-fee2-4867-8855-a55b3a79d9ed', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('715e4f6d-fee2-4867-8855-a55b3a79d9ed', foundational, all_racial_classification_presumptively_invalid).
narrative_ontology:cs_axiom_status(all_racial_classification_presumptively_invalid, holdable).
narrative_ontology:cs_axiom_grounding('715e4f6d-fee2-4867-8855-a55b3a79d9ed', all_racial_classification_presumptively_invalid, deontological).
narrative_ontology:cs_axiom('715e4f6d-fee2-4867-8855-a55b3a79d9ed', foundational, strict_scrutiny_applies_symmetrically).
narrative_ontology:cs_axiom_status(strict_scrutiny_applies_symmetrically, holdable).
narrative_ontology:cs_axiom_grounding('715e4f6d-fee2-4867-8855-a55b3a79d9ed', strict_scrutiny_applies_symmetrically, deontological).
narrative_ontology:cs_axiom('715e4f6d-fee2-4867-8855-a55b3a79d9ed', secondary, remedial_purpose_not_compelling).
narrative_ontology:cs_axiom_status(remedial_purpose_not_compelling, holdable).
narrative_ontology:cs_axiom_grounding('715e4f6d-fee2-4867-8855-a55b3a79d9ed', remedial_purpose_not_compelling, deontological).
narrative_ontology:cs_reference_frame('715e4f6d-fee2-4867-8855-a55b3a79d9ed', harlan_plessy_dissent_principle).
narrative_ontology:cs_drift_state('715e4f6d-fee2-4867-8855-a55b3a79d9ed', post_sffa_2023, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('715e4f6d-fee2-4867-8855-a55b3a79d9ed', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, colorblind_principle_advocates).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, asian_white_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_conscious_programs).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, implementing_institutions).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, diversity_remedial_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, asian_white_applicants).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, constitutional_colorblindness_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, harlan_plessy_dissent_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, strict_scrutiny_all_racial_classifications).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for formal colorblind equality as constitutional command. Collect doctrinal victories (strict scrutiny, bans on racial preferences) and institutional influence. Can shift between originalist, textualist, and liberal-formalist frameworks — arbitrage-grade exit across interpretive communities.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, colorblind_principle_advocates, beneficiary,
    organized, generational, arbitrage, national).

% Denied admission or employment under race-conscious programs; benefit when colorblind enforcement removes those programs. But cannot exit the broader structural inequities (legacy admissions, test bias, resource disparities) that the colorblind principle leaves untouched. Exit is constrained: they can choose different institutions but not different structural conditions.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_white_applicants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, asian_white_applicants, payer).

% Universities, employers, government agencies that set and administer race-conscious programs. They want discretion to use racial classification for diversity, remediation, or inclusion. Bear compliance costs of colorblind mandates (litigation, program redesign, lost discretion). Exit is constrained: they must comply with constitutional rulings but can redesign programs within narrowing boundaries.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, implementing_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, implementing_institutions, payer).

% Affirmative action admissions, diversity hiring, set-asides, remedial programs. Structurally suppressed by the colorblind constraint — each doctrinal tightening (strict scrutiny, narrow tailoring, SFFA's effective ban) reduces their operational space. No exit within the framework: they cannot become race-neutral and remain the same programs.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_programs, payer,
    organized, biographical, trapped, national).

% Civil rights organizations, legal scholars, community groups advancing remedial and diversity rationales. Would object to colorblind constraint but are structurally excluded: the colorblind framework treats their arguments as categorically impermissible (race-conscious = unconstitutional). Their exit is trapped — they must argue within the constraint's terms or be ruled out of order.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, diversity_remedial_advocates, excluded,
    powerful, generational, trapped, national).

% Authoritatively instantiates the colorblind reading through precedent (Bakke, Croson, Adarand, Parents Involved, SFFA). Sits as analytical observer of the constraint's operation but also as agenda setter whose rulings constitute the constraint's enforcement. Exit is analytical: it can change doctrine but only through its own institutional processes.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, supreme_court, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, supreme_court, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, colorblind_principle_advocates).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, administrable rule: the state may not classify by race. Solves the coordination problem of preventing caste legislation and arbitrary racial sorting by establishing a bright-line prohibition.
% TRANSFER_FUNCTION: Moves discretionary authority over racial classification from implementing institutions (universities, employers, legislatures) to the colorblind principle's beneficiaries (formal equality advocates, applicants who would benefit from race-neutral competition). The transfer is the foreclosure of race-conscious tools.
% ABSENT_VOICES: Historically subordinated racial groups who would argue that colorblindness locks in pre-existing disparities; remedial policymakers who would use racial classification to dismantle caste effects; international human rights bodies that recognize special measures as permitted. They are excluded because the colorblind framework treats their preferred measures as the very evil the Constitution forbids.
% DISAPPEARANCE_RATIONALE: If the colorblind constraint vanished overnight, universities and employers would immediately adopt race-conscious admissions and hiring; legislative bodies would enact targeted remedial measures; the doctrinal architecture of strict scrutiny for all racial classifications would collapse. The world of race-conscious remediation would return — a substantial rearrangement.
% FOUNDING_PROBLEM: The Equal Protection Clause was enacted to prevent states from enacting Black Codes and other caste legislation that sorted citizens by race into inferior legal status. The colorblind principle (Harlan's dissent) emerged as the interpretive rule: the Constitution forbids all racial classification to prevent the state from ever recreating caste.
% FOUNDING_PROBLEM_CORROBORATION: Colorblind advocates (beneficiaries) attest the problem is live: any racial classification risks caste. Remedial advocates (victims) and historians outside the beneficiary set attest the problem is dead: the original caste legislation is gone; today's racial hierarchy is structural, not classificatory. No neutral arbiter corroborates either side — the dispute is the kernel contest itself.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, ExtMetricName, E),
    domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equal_protection_commitment__colorblind_reading),
    narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the constraint now actively blocks race-conscious programs that institutions would otherwise adopt — a transfer from remedial/diversity goals to formal equality. The rise from 0.15 (1896) to 0.42 (2023) tracks the principle's redeployment from a dissent against segregation to a weapon against remediation. Suppression (0.58) is moderate: the constraint requires active judicial enforcement to invalidate race-conscious programs; it does not self-execute. Theater ratio (0.22) is low but rising: the formalist rhetoric ('the way to stop discrimination on the basis of race is to stop discriminating on the basis of race') increasingly covers a constraint that blocks remedial measures while leaving structural inequities untouched. Accessibility collapse (0.78) is high: once the colorblind principle is accepted, alternatives (race-conscious remediation) appear categorically forbidden — but resistance (0.71) is also high, reflecting sustained contestation from remedial and diversity readings.
 *
 * PERSPECTIVAL GAP:
 *   From the colorblind advocate's seat (beneficiary, organized, arbitrage exit), the constraint is genuine coordination: a neutral principle that prevents the state from sorting citizens by race. From the implementing institution's seat (agenda setter/payer, institutional, constrained exit), it is extraction: a constraint that removes tools the institution judges necessary for its mission. From the race-conscious program's seat (victim, organized, trapped), it is a snare: active suppression of remedial measures. The engine computes this divergence from the structural data — the claimed mountain type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Colorblind principle advocates are structural beneficiaries: they advance a principle that collects doctrinal victories and forecloses remedial alternatives (d ~ 0.15). Asian/white applicants are beneficiaries but also constrained: they gain admission slots but cannot exit the broader structural inequities the constraint leaves intact (d ~ 0.35). Implementing institutions are payers: they lose discretionary tools and bear compliance costs (d ~ 0.65). Race-conscious programs are victims: they are structurally suppressed, with no exit within the framework (d ~ 0.85). Diversity/remedial advocates are excluded: they would contest the constraint but have no standing in the colorblind framework (d ~ 0.90). The Supreme Court as institutional observer sits near analytical (d ~ 0.50) but its authoritative instantiation pulls it toward agenda setter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing caste legislation like Black Codes) is dead — the original target of the Equal Protection Clause no longer exists in its historical form. Yet the constraint persists and has expanded to block remedial measures. This is classic mandatrophy: the constraint's mandate has outlived its function, but the colorblind principle has been repurposed to serve new beneficiaries. The founding problem status is contested: colorblind advocates claim the problem is live (any racial classification risks caste); remedial advocates say it is dead (caste is now structural, not classificatory). Corroboration from outside the beneficiary set is thin: the formalist reading relies largely on its own internal coherence and Harlan's dissent, not on external historical validation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the colorblind principle a genuine natural law of constitutional structure, or a constructed constraint that benefits identifiable agents by foreclosing race-conscious remediation?',
    'Historical-genealogical analysis of the principle''s emergence: whether it functioned as a neutral constraint from Reconstruction or was selectively deployed against remedial measures after Brown. Structural test: if the principle''s beneficiaries shift when the demographic composition of racial preferences changes, it is constructed.',
    'If constructed, the constraint is a false summit mountain triggering FSM reclassification to tangled_rope; if natural law, it remains mountain with negligible extraction from any seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Natural-law vs constructed status of colorblind constitutional principle').

omega_variable(
    classification_harm_vs_effects_ambiguity,
    'Does the harm of racial classification inhere in the classification act itself (formalist), or in the subordinate caste effects it produces (effects-based)?',
    'Doctrinal trace: whether strict scrutiny applies symmetrically to all racial classifications regardless of direction (formalist) or only to those burdening historically subordinated groups (effects-based). The Supreme Court''s trajectory from Bakke through SFFA indicates formalist drift.',
    'If harm is formal, extractiveness is symmetric (all racial classifications equally constrained). If harm is effects-based, the constraint extracts asymmetrically from remedial programs only — changing victim set and ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_harm_vs_effects_ambiguity, conceptual, 'Whether racial classification harm is formal or effects-based').

omega_variable(
    kernel_reading_committer_structure,
    'How does this colorblind reading relate to the equal_protection_commitment kernel and its sibling readings (remedial_reading, diversity_reading)?',
    'Structural comparison of victim sets, beneficiary sets, and ε values across the three readings. This reading forecloses remedial_reading in any single framework; coexists_with diversity_reading as live public dispute; influences remedial_reading by shifting legitimacy conditions.',
    'Documents the kernel family structure for contamination analysis. If this reading''s formalist premise forecloses remedial reading within a single legal framework, the engine''s foreclosure computation will detect it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer frame: this reading''s structural relations to sibling readings of equal_protection_commitment kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1896, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1896, equal_protection_commitment__colorblind_reading, theater_ratio, 1896, 0.05).
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__colorblind_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1896, equal_protection_commitment__colorblind_reading, base_extractiveness, 1896, 0.15).
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__colorblind_reading, base_extractiveness, 1954, 0.25).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.38).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.41).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1896, equal_protection_commitment__colorblind_reading, suppression_requirement, 1896, 0.2).
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__colorblind_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.52).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.56).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__colorblind_reading, 0.1).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, affirmative_action_admissions).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, voting_rights_act_section2).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, disparate_impact_doctrine).

% DUAL FORMULATION NOTE:
% Equal protection commitment kernel decomposes into three readings with different ε and victim sets: colorblind (ε~0.42, victims=race-conscious programs), remedial (ε~0.15, victims=subordinated groups), diversity (ε~0.25, victims=institutions denied diversity rationale). This reading forecloses remedial; coexists with diversity; influences both by raising doctrinal barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, institutional, 0.55).
constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
