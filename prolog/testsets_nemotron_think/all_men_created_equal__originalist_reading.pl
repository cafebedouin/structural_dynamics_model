% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Originalist Reading of "All Men Are Created Equal" — Equality Bounded by 18th-Century Social Taxonomy
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   The originalist reading of "all men are created equal" treats the
 *   Declaration's universal language as fixed at the 18th-century social
 *   taxonomy of the founding generation — white, male, property-holding,
 *   Protestant. This reading governs constitutional equality doctrine by
 *   tethering the scope of equal protection to founder intent and original
 *   public meaning, which at the founding excluded enslaved persons, women,
 *   Indigenous nations, and non-propertied men. The constraint operates as a
 *   tangled rope: it provides genuine coordination (stable interpretive
 *   anchor, democratic legitimacy claim) while extracting asymmetrically
 *   (excluding groups from equal citizenship protections). Active enforcement
 *   is required through judicial doctrine, originalist methodology, and the
 *   institutional architecture of federal courts. The claimed type is
 *   tangled_rope; the metrics reflect high extractiveness (0.78), substantial
 *   suppression (0.72), and moderate theater (0.38) — the coordination
 *   function is real but the extraction is structural.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.78).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.72).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Originalist Reading of \"All Men Are Created Equal\" — Equality Bounded by 18th-Century Social Taxonomy").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '44b92628-f7f9-45aa-b37c-e3360884742f').
narrative_ontology:cs_kernel_codification('44b92628-f7f9-45aa-b37c-e3360884742f', fixed_text).
narrative_ontology:cs_authority_grounding('44b92628-f7f9-45aa-b37c-e3360884742f', lineage).
narrative_ontology:cs_interpretation_layer_present('44b92628-f7f9-45aa-b37c-e3360884742f').
narrative_ontology:cs_reading_relation('44b92628-f7f9-45aa-b37c-e3360884742f', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('44b92628-f7f9-45aa-b37c-e3360884742f', all_men_created_equal__textualist_paradox_reading, influences).
narrative_ontology:cs_axiom('44b92628-f7f9-45aa-b37c-e3360884742f', foundational, original_public_meaning_fixed_at_enactment).
narrative_ontology:cs_axiom_status(original_public_meaning_fixed_at_enactment, holdable).
narrative_ontology:cs_axiom_grounding('44b92628-f7f9-45aa-b37c-e3360884742f', original_public_meaning_fixed_at_enactment, conventional).
narrative_ontology:cs_axiom('44b92628-f7f9-45aa-b37c-e3360884742f', foundational, founder_intent_governs_constitutional_scope_of_equality).
narrative_ontology:cs_axiom_status(founder_intent_governs_constitutional_scope_of_equality, holdable).
narrative_ontology:cs_axiom_grounding('44b92628-f7f9-45aa-b37c-e3360884742f', founder_intent_governs_constitutional_scope_of_equality, conventional).
narrative_ontology:cs_axiom('44b92628-f7f9-45aa-b37c-e3360884742f', secondary, universal_language_as_aspirational_not_operative).
narrative_ontology:cs_axiom_status(universal_language_as_aspirational_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('44b92628-f7f9-45aa-b37c-e3360884742f', universal_language_as_aspirational_not_operative, conventional).
narrative_ontology:cs_reference_frame('44b92628-f7f9-45aa-b37c-e3360884742f', founding_era_constitutional_settlement).
narrative_ontology:cs_drift_state('44b92628-f7f9-45aa-b37c-e3360884742f', contemporary_originalist_supermajority, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('44b92628-f7f9-45aa-b37c-e3360884742f', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_institutional_heirs).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_judicial_network).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_persons_and_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women_excluded_at_founding).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_nations_excluded_at_founding).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, non_propertied_white_men_excluded_at_founding).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, founder_intent_governs_constitutional_scope).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, fixed_meaning_at_enactment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutional descendants of the founding elite (white male property-holding class) whose structural position is insulated by a constitutional reading that fixes equality's scope at 1776/1787. They benefit from the stability this reading provides to property rights, federalism doctrines, and hierarchical social arrangements traced to the founding. Exit is arbitrage-grade: they can invoke originalism when it protects their interests and pivot to living constitutionalism when it does not.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_institutional_heirs, beneficiary,
    institutional, generational, arbitrage, national).

% Judges, legal scholars, and movement lawyers who administer and develop originalist methodology. They set the interpretive agenda for constitutional equality cases, determining which historical sources count and how founder intent is reconstructed. They benefit professionally and ideologically from the methodology's dominance in federal courts. Exit is mobile: they could adopt other interpretive methods but have invested careers in this one.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_judicial_network, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(all_men_created_equal__originalist_reading, originalist_judicial_network, beneficiary).

% Historically enslaved persons and their descendants who bear the extractive force of a reading that treats the Declaration's equality language as inapplicable to them at the founding and whose legal consequences (Dred Scott, post-Reconstruction rollback, contemporary voting rights dilution) are justified by originalist fidelity to that exclusion. Exit is trapped: the constraint is embedded in the supreme law they cannot amend and the courts they cannot easily leave.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_persons_and_descendants, payer,
    powerless, generational, trapped, national).

% Women excluded from the founding's political community whose equal citizenship claims (suffrage, reproductive autonomy, anti-discrimination) must overcome originalist barriers that the founding generation did not recognize women as rights-bearers. Exit is constrained: they have won formal amendments (19th) and statutory protections, but originalist doctrine still limits the scope of equal protection for sex-based classifications.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women_excluded_at_founding, payer,
    moderate, generational, constrained, national).

% Indigenous nations whose sovereignty and territorial rights were denied by the founding taxonomy ("merciless Indian savages" in the Declaration, no constitutional personhood). Originalist reading treats tribal sovereignty as a congressional plenary power gift, not a retained inherent right. Exit is constrained: they operate in a dual sovereign framework but originalist jurisprudence steadily narrows retained sovereignty.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_nations_excluded_at_founding, payer,
    moderate, generational, constrained, national).

% White men without property who were excluded from the founding's political equality (property qualifications for suffrage, office-holding). While later amendments expanded formal inclusion, originalist doctrine's deference to founding-era structures still shapes voting rights, campaign finance, and economic regulation doctrines that disadvantage this group. Exit is constrained: formal political equality exists but structural power remains mediated by founding-era property logic.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, non_propertied_white_men_excluded_at_founding, payer,
    moderate, biographical, constrained, national).

% Civil rights organizations, progressive legal scholars, and movement lawyers who argue for iterative expansion of equality regardless of founder intent. They are structurally excluded from the originalist interpretive community — their arguments are treated as illegitimate within that methodology's terms. Exit is mobile: they operate in parallel interpretive communities (living constitutionalism, common law constitutionalism) and contest originalism in public and judicial discourse.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_constitutional_advocates, excluded,
    organized, biographical, mobile, national).

% External observer analyzing the constraint's structural operation across all seats. Sees the full beneficiary/victim asymmetry, the methodological closure that protects the reading from internal critique, and the historical contingency that originalism treats as fixed. No stake in the constraint's persistence or dissolution.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, analytical_constitutional_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, determinate interpretive anchor for constitutional equality doctrine by fixing meaning at the founding moment, resolving indeterminacy that would otherwise require continuous judicial discretion.
% TRANSFER_FUNCTION: Moves interpretive authority and the legal protections of equal citizenship from historically excluded groups (enslaved persons, women, Indigenous nations, non-propertied men) to the founding-era power holders and their institutional heirs, mediated by the originalist judicial network that administers the reading.
% ABSENT_VOICES: The enslaved persons, women, Indigenous nations, and non-propertied men who were present at the founding but excluded from the political community that authored the Declaration and Constitution. Their descendants remain structurally excluded from the originalist interpretive community, which treats their exclusion as a fixed historical fact rather than a continuing injustice.
% DISAPPEARANCE_RATIONALE: If the originalist reading vanished overnight, constitutional equality doctrine would shift to living constitutionalist or common-law frameworks that have iteratively expanded protection to excluded groups. The legal architecture of voting rights, reproductive rights, tribal sovereignty, and anti-discrimination law would reorganize around iterative expansion rather than founding-era fixation. The originalist judicial network would lose its dominant methodological position.
% FOUNDING_PROBLEM: How to legitimate a new republic built on chattel slavery, Indigenous dispossession, and gender/property hierarchy while declaring universal equality in its founding charter. The originalist reading solves this by treating the universal language as aspirational rhetoric bounded by the founders' actual social taxonomy, making the contradiction a feature of historical fidelity rather than a defect requiring correction.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the originalist tradition (Paul Finkelman on slavery and the founding, Annette Gordon-Reed on Jefferson's contradiction, Woody Holton on founder economic interests, Gerald Leonard on the Constitution's pro-slavery structure) document that the founding problem was precisely the tension between universal language and particular exclusion, and that originalism's resolution — fixing equality at the founding taxonomy — preserves the exclusionary structure. No corroboration from within the originalist beneficiary set is treated as independent.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the reading systematically denies equal protection to groups excluded at the founding, and this denial is not incidental but structurally necessary to the reading's coherence. Suppression is substantial because the constraint's persistence depends on active judicial enforcement (originalist majorities on courts), methodological closure (originalism's internal criteria exclude universalist arguments), and institutional entrenchment (Federalist Society pipeline, law school curricula). Theater ratio is moderate: the coordination function (determinate meaning, democratic legitimacy) is genuine but a growing share of the reading's operation defends exclusionary outcomes rather than interpretive stability. Accessibility collapse (0.65) reflects that alternatives (living constitutionalism, common law constitutionalism) remain conceptually available but are structurally marginalized in the current judicial regime. Resistance (0.58) is significant: universalist advocates, civil rights movements, and progressive legal scholars mount sustained opposition, but the constraint's institutional position makes resistance costly.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judicial network's seat, the constraint is genuine coordination: it solves the indeterminacy problem of constitutional interpretation and claims democratic legitimacy through fidelity to enacted law. From the excluded groups' seats, the same structure operates as enforced extraction: their equal citizenship is subordinated to a historical taxonomy they had no hand in creating. The engine computes this divergence from the structural data — the authored claim (tangled_rope) acknowledges both functions exist simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding elite institutional heirs and the originalist judicial network are structural beneficiaries (d near 0.0-0.2): they collect interpretive authority, legal stability for hierarchical arrangements, and professional/ideological returns. Historically excluded groups are structural victims (d near 0.8-1.0): they bear the denial of equal protection, trapped or constrained by the constraint's embedding in supreme law. Universalist advocates are excluded from the interpretive community (d ~0.5 but structurally blocked from participation). The analytical observer sits at d=0.5. Power and exit differentiation drive the per-seat divergence the engine will compute: institutional beneficiaries with arbitrage exit vs. powerless victims with trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating a slaveholding republic with universal language) is dead — slavery abolished, formal suffrage expanded — but the arrangement persists and has intensified in the current originalist supermajority. The constraint has not transitioned to a scaffold (no sunset clause, no declared transitional purpose). It operates as a tangled rope whose coordination function (interpretive stability) is real but whose extraction function (maintaining founding-era exclusions) has been reactivated and expanded. Mandatrophy is unresolved: the arrangement's original justificatory problem is gone, but the constraint persists and extracts anew.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_as_constraint_vs_methodology,
    'Is originalism a substantive constraint on constitutional equality (fixing outcomes) or a methodological constraint on interpretation (fixing process)?',
    'Track whether originalist judges reach outcomes that contradict originalist methodology when the methodology would produce politically undesirable results (e.g., Brown v. Board, Loving v. Virginia, Bolling v. Sharpe). If methodology is routinely overridden, it functions as outcome-constraint rather than process-constraint.',
    'If outcome-constraint, the reading is a snare using methodology as cover; if process-constraint, it is a genuine (if contested) coordination mechanism. Affects claimed_type assessment and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_constraint_vs_methodology, empirical, 'Whether originalism''s methodological commitments are structurally binding or selectively deployed.').

omega_variable(
    founder_intent_recoverability,
    'Can founder intent regarding equality''s scope be recovered with sufficient determinacy to operate as a legal rule, or is the historical record irreducibly indeterminate?',
    'Assess whether originalist scholarship converges on a single account of founder intent on equality, or whether the historical record supports multiple, conflicting reconstructions that originalists selectively deploy.',
    'If irreducibly indeterminate, the reading''s coordination function collapses — it coordinates on a fiction of determinacy. The constraint becomes a snare (methodology as cover for judicial discretion). If recoverable, the coordination function stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_intent_recoverability, conceptual, 'Whether the historical record supports the determinacy originalism claims.').

omega_variable(
    kernel_reading_foreclosure_dynamics,
    'Does the originalist reading''s core premise (fixed meaning at founding) logically foreclose the universalist reading within a single constitutional framework, or do they coexist as competing interpretive communities?',
    'Analyze whether a single legal system can simultaneously hold that equality''s meaning is fixed at founding AND that equality''s meaning expands iteratively. If the premises are mutually exclusive in practice (judges must choose one), forecloses; if different actors hold each without systemic contradiction, coexists_with.',
    'Determines reading_relations classification and whether the kernel exhibits genuine structural pluralism or zero-sum interpretive conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_dynamics, conceptual, 'Structural relationship between originalist and universalist readings within one constitutional order.').

omega_variable(
    suppression_mechanism_judicial_vs_social,
    'Is the measured suppression primarily judicial (court enforcement of originalist doctrine) or social (originalism''s cultural legitimacy marginalizing universalist arguments)?',
    'Compare suppression trajectory during periods of originalist judicial dominance vs. periods of originalist cultural dominance without judicial control. If suppression tracks judicial control, it''s judicial; if it persists without judicial control, it''s social/internalized.',
    'If social/internalized, the constraint''s effective suppression is higher than structural measures suggest — excluded groups internalize the exclusion as legitimate. If judicial, suppression is contingent on court composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_vs_social, empirical, 'Whether suppression operates through state enforcement or internalized legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ameoq_tr_t1787, all_men_created_equal__originalist_reading, theater_ratio, 1787, 0.25).
narrative_ontology:measurement(ameoq_tr_t1857, all_men_created_equal__originalist_reading, theater_ratio, 1857, 0.45).
narrative_ontology:measurement(ameoq_tr_t1896, all_men_created_equal__originalist_reading, theater_ratio, 1896, 0.42).
narrative_ontology:measurement(ameoq_tr_t1954, all_men_created_equal__originalist_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(ameoq_tr_t1986, all_men_created_equal__originalist_reading, theater_ratio, 1986, 0.28).
narrative_ontology:measurement(ameoq_tr_t2008, all_men_created_equal__originalist_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(ameoq_tr_t2022, all_men_created_equal__originalist_reading, theater_ratio, 2022, 0.38).
narrative_ontology:measurement(ameoq_tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(ameoq_be_t1787, all_men_created_equal__originalist_reading, base_extractiveness, 1787, 0.65).
narrative_ontology:measurement(ameoq_be_t1857, all_men_created_equal__originalist_reading, base_extractiveness, 1857, 0.82).
narrative_ontology:measurement(ameoq_be_t1896, all_men_created_equal__originalist_reading, base_extractiveness, 1896, 0.78).
narrative_ontology:measurement(ameoq_be_t1954, all_men_created_equal__originalist_reading, base_extractiveness, 1954, 0.62).
narrative_ontology:measurement(ameoq_be_t1986, all_men_created_equal__originalist_reading, base_extractiveness, 1986, 0.58).
narrative_ontology:measurement(ameoq_be_t2008, all_men_created_equal__originalist_reading, base_extractiveness, 2008, 0.71).
narrative_ontology:measurement(ameoq_be_t2022, all_men_created_equal__originalist_reading, base_extractiveness, 2022, 0.78).
narrative_ontology:measurement(ameoq_be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ameoq_su_t1787, all_men_created_equal__originalist_reading, suppression_requirement, 1787, 0.55).
narrative_ontology:measurement(ameoq_su_t1857, all_men_created_equal__originalist_reading, suppression_requirement, 1857, 0.85).
narrative_ontology:measurement(ameoq_su_t1896, all_men_created_equal__originalist_reading, suppression_requirement, 1896, 0.78).
narrative_ontology:measurement(ameoq_su_t1954, all_men_created_equal__originalist_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(ameoq_su_t1986, all_men_created_equal__originalist_reading, suppression_requirement, 1986, 0.52).
narrative_ontology:measurement(ameoq_su_t2008, all_men_created_equal__originalist_reading, suppression_requirement, 2008, 0.65).
narrative_ontology:measurement(ameoq_su_t2022, all_men_created_equal__originalist_reading, suppression_requirement, 2022, 0.72).
narrative_ontology:measurement(ameoq_su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__originalist_reading, 0.08).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, equal_protection_clause__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, due_process_clause__substantive_originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, voting_rights_act__originalist_shelby_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the 'all_men_created_equal' kernel family. The universalist_reading (equality as iterative expansion) and textualist_paradox_reading (universal language vs. restricted application as performative contradiction) are sibling constraints with different ε values, beneficiary/victim structures, and claimed types. This originalist_reading has high ε (0.78) because it fixes equality at an exclusionary founding taxonomy; the universalist_reading has lower ε because its coordination function (iterative inclusion) does not systematically extract from excluded groups. The textualist_paradox_reading has moderate ε because it exposes the contradiction without resolving it. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, powerless, 0.95).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, moderate, 0.75).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, organized, 0.45).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
