% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Remedial Mandate: Race-Conscious Remediation for Historical Subordination
 *   domain: constitutional/political/educational
 *
 * SUMMARY:
 *   The Equal Protection Clause is a contested kernel grounding multiple
 *   readings of how the Constitution addresses racial inequality. This
 *   constraint story instantiates the REMEDIAL READING: equal protection
 *   requires race-conscious remediation of historical group subordination to
 *   achieve substantive equality. Under this reading, the clause mandates
 *   temporary, structured preferences for historically marginalized racial
 *   groups (primarily Black Americans, Native Americans, and other
 *   subordinated minorities) in educational admissions, employment, and
 *   contracting, to counteract the structural inheritance of slavery,
 *   segregation, and Jim Crow exclusion. Individual members of non-preferred
 *   groups bear diffuse competitive costs in specific contests. The remedial
 *   reading justifies extraction (high ε at 0.68) as the necessary price of
 *   structural remedy: individual costs are temporary, group-level benefits
 *   are structural and permanent. The constraint is authored as SCAFFOLD
 *   (temporary, with sunset clause) because the reading's own logic assumes
 *   remediation will eventually complete and the preference structure will no
 *   longer be needed.
 *
 * KEY AGENTS:
 *   - Historically marginalized minorities (descendant groups of slavery, Jim Crow): structural beneficiaries; benefits accrue as group access to gatekept opportunity expands
 *   - Non-preferred group members (individuals excluded by race from preferred consideration): diffuse payers; bear individual costs across multiple competitive contests
 *   - Educational institutions and public employers (agenda-setters): implement race-conscious policies; bear litigation and regulatory costs
 *   - Courts/Constitutional authority: interpret and enforce the remedial mandate; can reverse it through reinterpretation
 *   - Competing readings (colorblind, diversity): excluded from this reading's framework but remain live litigants and institutional actors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.68).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.42).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Remedial Mandate: Race-Conscious Remediation for Historical Subordination").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional/political/educational").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, 'f080f515-24ee-4b1b-aa87-702417514819').
narrative_ontology:cs_kernel_codification('f080f515-24ee-4b1b-aa87-702417514819', formalized).
narrative_ontology:cs_authority_grounding('f080f515-24ee-4b1b-aa87-702417514819', lineage).
narrative_ontology:cs_interpretation_layer_present('f080f515-24ee-4b1b-aa87-702417514819').
narrative_ontology:cs_reading_relation('f080f515-24ee-4b1b-aa87-702417514819', equal_protection_clause__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('f080f515-24ee-4b1b-aa87-702417514819', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('f080f515-24ee-4b1b-aa87-702417514819', foundational, historical_subordination_requires_structural_remedy).
narrative_ontology:cs_axiom_status(historical_subordination_requires_structural_remedy, holdable).
narrative_ontology:cs_axiom_grounding('f080f515-24ee-4b1b-aa87-702417514819', historical_subordination_requires_structural_remedy, empirically_contingent).
narrative_ontology:cs_axiom('f080f515-24ee-4b1b-aa87-702417514819', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('f080f515-24ee-4b1b-aa87-702417514819', substantive_equality_mandate, deontological).
narrative_ontology:cs_reference_frame('f080f515-24ee-4b1b-aa87-702417514819', fourteenth_amendment_substantive_equality).
narrative_ontology:cs_drift_state('f080f515-24ee-4b1b-aa87-702417514819', contemporary_post_colorblind_dominance, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f080f515-24ee-4b1b-aa87-702417514819', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_marginalized_minorities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, descendant_groups_of_slavery_jim_crow).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_preferred_group_members).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_applicants_excluded_by_race).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of racial groups historically subordinated through slavery, segregation, and Jim Crow laws, and their descendants. The remedial mandate provides preferential consideration in admissions, hiring, and contracting to counteract accumulated structural disadvantage. Benefits accrue to groups as collective entities, though individual members gain or lose based on remedial application. Cannot exit the group identity itself; can exit individual competitive processes but must remain within a system that recognizes historical subordination as a fact shaping opportunity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_marginalized_minorities, beneficiary,
    organized, generational, constrained, national).

% Individual applicants to selective educational institutions, employment, or contracting opportunities who do not belong to the historical-subordination group, or who belong but are not classified as preferred for remedial purposes. May be excluded or disadvantaged in individual competitive processes due to the remedial mandate's race-conscious allocation. Can pursue alternative institutions, employers, or regions; can challenge the mandate through litigation; face diffuse individual costs across multiple contests rather than systematic subordination.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_preferred_group_members, payer,
    powerful, biographical, mobile, national).

% Selective universities and professional schools administering admissions under remedial equal-protection doctrine. Must author and defend race-conscious policies; bear litigation costs and regulatory scrutiny; justify remedial thresholds and sunset terms. Their discretion is constrained by constitutional boundaries and legislative reversal risk.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Government agencies and firms administering hiring and procurement under remedial equal-protection doctrine. Implement race-conscious hiring and contracting targets; bear compliance costs, litigation exposure, and political pressure; must document historical subordination and remedial necessity.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, public_employers_and_contractors, agenda_setter,
    institutional, generational, constrained, national).

% Federal judiciary, especially the Supreme Court, as the authoritative interpreter of the Equal Protection Clause. Draws the boundaries of permissible race-consciousness, defines remedial justification, sets sunset terms, evaluates individual vs. group remediation, and can reverse the remedial doctrine entirely through reinterpretation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, courts_and_constitutional_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Adherents of the colorblind reading (race-neutrality mandate) and the diversity reading (compelling-interest frame) are excluded from the remedial reading's framework in the sense that each reading defines the constitutional meaning of equal protection differently. They would argue the remedial reading overreaches or misconstrues the clause; they remain litigants, advocates, and sometimes judges contesting the meaning.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, competing_equal_protection_readings, excluded,
    institutional, civilizational, analytical, national).

% Congress and state legislatures could reverse or modify the remedial mandate through legislation, but constitutional authority rests primarily with courts. Legislatures are excluded from the immediate governance of the equal-protection rule itself, though they can change institutions' mandates through statutory amendment or appropriations control.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, legislative_bodies, excluded,
    institutional, generational, constrained, national).

% Sees the full structure: a remedial mandate extracting diffuse costs from non-preferred individuals to benefit historically subordinated groups, justified by a theory of structural inequality requiring structural remedy, temporarily authorized until remediation is complete. Observes the contested nature of whether remediation is complete, who decides, and when the sunset fires.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, analyst_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the structural inequality coordination problem: individual, case-by-case decisions under color-blindness would preserve accumulated subordination indefinitely; a collective, group-remedial approach targets the root cause — structural disadvantage embedded in institutional gatekeeping. The coordination problem is: how to dismantle systems of subordination when individual merit cannot be separated from group-based opportunity deprivation.
% TRANSFER_FUNCTION: Transfers competitive advantage and opportunity from non-preferred individuals to historically subordinated group members via preferential consideration in admissions, hiring, and contracting. Moves scarce slots, jobs, contracts, and their associated social mobility toward historically marginalized groups and away from others. The transfer is temporary, justified as the cost of remediation, and framed as carrying individual costs to achieve group-level structural change.
% ABSENT_VOICES: Colorblind-reading advocates (including some conservative jurists, some individual-rights libertarians, and some members of non-preferred groups) argue the remedial reading violates the individual-rights core of equal protection and should not be in the room at all — their exclusion from the remedial reading's framework (though they remain active litigants and legislators) is structural. Newly arrived immigrant groups debate whether the historical subordination remedied includes their members or runs backward-looking only to slavery and Jim Crow, their absence from the subordination narrative being itself contested ground.
% DISAPPEARANCE_RATIONALE: If the remedial mandate disappeared overnight, selective institutions would revert to applicant evaluation without race-consciousness; the demographic composition of students, professionals, and contractors would shift toward pre-remedial patterns; accumulated structural advantage of non-preferred groups would compound across another generation; access to gatekept opportunity would narrow for historically marginalized groups unless alternative remedial mechanisms (socioeconomic affirmative action, targeted outreach, alternative admissions paths) were put in place to restore structural correction. The remedial constraint is not background law — it is an active intervention whose removal would trigger measurable institutional reorganization.
% FOUNDING_PROBLEM: Historical chattel slavery, segregation, and Jim Crow laws systematically excluded Black Americans (and other subordinated groups) from education, employment, property ownership, and wealth accumulation for centuries. Post-Civil Rights legal color-blindness failed to remedy the accumulated structural disadvantage: intergenerational poverty, segregated schooling, institutional gatekeeping, and systemic exclusion from economic opportunity persisted. The founding problem is: how can equal protection ever be achieved when non-discrimination alone leaves the structural inheritance of subordination in place?
% FOUNDING_PROBLEM_CORROBORATION: Historical-subordination fact: extensively documented by historians, demographers, and economists outside the benefiting parties (Douglass, Frazier, Myrdal, modern economic historians). Persistence-of-structural-disadvantage fact: documented in peer-reviewed scholarship on educational opportunity gaps, wealth inequality, employment discrimination, health disparities (independent researchers, government statistical agencies, civil-rights organizations that advocate for remediation but also non-partisan demographic analysis). Effectiveness-of-color-blindness fact: contested — remedial-reading advocates argue color-blindness perpetuates structural inequality; colorblind-reading advocates argue it prevents further group-based harm. The founding problem (historical subordination) is not contested; its structural persistence and the remedy question are.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as high (0.68 by interval end) because the remedial mandate structurally extracts individual opportunity from non-preferred applicants to benefit group-remediation targets. The extraction is not coercion in a legal or force sense; it is competitive disadvantage baked into gatekeeping rules. Suppression is moderate (0.42) because the mandate operates through public law and institutional policy, not through coercion of resistance — individuals lose competitive contests, not through legal prohibition of their action, but through institutional preference rules. The mandate faces real resistance (0.79) from colorblind-reading advocates, non-preferred individuals, and conservative legal scholars who argue it violates individual rights. Accessibility of alternatives is moderately collapsed (0.71) because selective educational and employment gatekeeping is itself limited — once excluded from a preferred institution, alternatives exist (other schools, employers, regions), but the scarce resource (selective-institution access) is not easily replaced. Theater ratio is low (0.18) because the remedial mandate operates as stated — the performance of justification is real, but the underlying mechanism is also real: race-conscious allocation happens, preferences matter, extraction occurs. The measurement series tracks rising extractiveness and rising suppression_requirement over the interval, plateauing in the later period (suggesting the constraint reaches a stable institutional configuration), and theater_ratio remains low and stable (the constraint is not primarily theatrical; it is a genuine policy mechanism). Measurement points are authored on a single shared grid so every metric has a value at every time point examined.
 *
 * PERSPECTIVAL GAP:
 *   The remedial-reading beneficiary (historically marginalized group member) would compute a Rope or Rope-hybrid classification from their seat: genuine coordination problem solved (how to dismantle structural inequality), net benefit (access to opportunity), enforceable rules that work as intended. The non-preferred-group victim would compute a Snare classification: extraction justified by abstract principles they did not choose, active enforcement of their exclusion, suppression of their meritocratic claim, no exit except leaving the high-stakes competitive arena entirely. The courts/constitutional authority would compute a Scaffold classification: temporary mandate with a real sunset (or so the reading claims), serving transitional remedial purpose, justifiable on equality grounds but constitutionally contestable. The colorblind-reading institutional competitor would compute a Snare classification: the remedial mandate itself is the extraction, imposing race-consciousness in violation of equal protection, enforcing group-based hierarchy. These divergences flow directly from structural differences in benefit, extraction, and exit — not from measurement error or misunderstanding, but from genuinely asymmetric situations.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized minorities are classified as beneficiaries (d near 0.0): they receive improved access to gatekept opportunity, face no legal prohibition (institutional preference is not coercion), and have substantial exit options if they choose not to compete (they can pursue alternative paths, different regions, etc.). However, they are also identity-locked (their group status is the basis of their benefit, they cannot exit their identity to lose the benefit, the benefit is conditional on group membership). This produces a complex d: low on pure beneficiary calculation, but higher when identity-lock is factored in (they cannot freely choose whether to accept the benefit because it is conditional on being in the group). Non-preferred individuals are classified as payers (d near 1.0): they face extraction in the form of competitive disadvantage, the extraction is active (institutional preference rules are enforced), their exit options are mobile but costly (they can pursue alternative institutions, but selective-institution access is scarce). The engine derives d from these structural facts — the authored directionality values emerge from beneficiary/victim designation plus the power/exit atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading's mandatrophy question: is the foundational problem (historical subordination and its structural persistence) still live, or has it been substantially solved such that the remedial mandate should sunset? The reading is authored as SCAFFOLD explicitly to acknowledge that mandatrophy is structural to the constraint — if remediation succeeds, the mandate becomes obsolete. However, the remediation completeness question (omega_1) is radically indeterminate: no canonical metrics, timelines, or institutional milestones exist to signal 'remediation complete.' This creates a risk of mandatrophy drift: the mandate persists beyond its justifying purpose, becomes a permanent feature of competitive gatekeeping, and transforms from temporary remedy into permanent group-preference regime (piton or snare). The measurement series shows extractiveness and suppression plateauing in the later interval, suggesting the constraint reaches a stable institutional configuration that does not self-correct toward remediation completion. This plateau is precisely the mandatrophy signal: if extractiveness stays high and suppression stays moderate indefinitely, the temporary constraint risks becoming permanent, and the theater_ratio should rise to reflect growing misalignment between remedial justification and operational reality. A robust mandatrophy-detection system would monitor for: (1) plateau in extractiveness/suppression above founding-problem thresholds, (2) growth in theater_ratio (justification rhetoric exceeding operational change), (3) conflict between sunset-clause language and actual institutional persistence. The remedial reading assumes this detection system works; empirically, it is contested whether courts and institutions will apply remediation-completion verdicts to sunset the mandate or will instead perpetuate it indefinitely under ever-refreshed remedial justifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completeness_indeterminacy,
    'When is historical subordination sufficiently remediated such that the race-conscious mandate can sunset? What metrics, timelines, or institutional milestones signal remediation completion?',
    'Court opinions, legislative debates, and academic scholarship attempting to define remedial endpoints: demographic parity at professional thresholds? Intergenerational wealth convergence? Institutional integration stability? No canonical endpoint exists; the remedial reading assumes temporality but does not specify termination conditions.',
    'If remediation is indefinitely deferred, the temporary constraint becomes permanent extraction wearing a temporary label. If endpoints are set arbitrarily, the mandate loses legitimacy and risks sunset before remediation takes structural root. The constraint''s type (scaffold) depends critically on this unresolved question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remediation_completeness_indeterminacy, conceptual, 'The sunset term is authored but the firing conditions are contested — a foundational indeterminacy for any temporary constraint.').

omega_variable(
    group_vs_individual_remediation,
    'Does the remedial mandate benefit historically marginalized groups as collective entities, or does it target individuals descended from the subordinated group? How does group identity interact with individual benefit, especially for mixed-race, immigrant, or newly-identified-group members?',
    'Case law on individual affirmative-action applicants who do not personally face subordination but satisfy group classification; legislative redefinition of beneficiary groups; changing demographics and census categories that shift group boundaries.',
    'If group remediation is coherent and bounded, the constraint''s victim set (non-preferred individuals) is determinate. If individual remediation is required, the group-based preference structure collapses and the mandate must shift to socioeconomic targeting. If boundaries blur, the constraint''s extraction becomes increasingly arbitrary (higher theater_ratio, lower accessibility_collapse).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_vs_individual_remediation, conceptual, 'The beneficiary definition depends on whether remediation targets groups or individuals; this boundary is contested across readings.').

omega_variable(
    structural_inequality_counterfactual,
    'If color-blindness had been enforced from 1865 forward, would structural inequality have persisted? Or would post-Civil-Rights individual freedom have generated sufficient opportunity convergence without race-conscious remedy?',
    'Counterfactual economic modeling; comparison with other nations'' post-subordination transitions; long-term data on opportunity gaps under color-blind regimes.',
    'If color-blindness would have naturally dissolved subordination, the remedial mandate is over-corrective extraction. If structural inequality persists under color-blindness (the remedial reading''s premise), the mandate is necessary coordination. This is the foundational disagreement between the remedial and colorblind readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_inequality_counterfactual, empirical, 'Whether structural inequality persists without race-conscious remedy is the empirical crux separating the remedial and colorblind readings.').

omega_variable(
    reading_foreclosure_and_coexistence,
    'Does the remedial reading logically foreclose the colorblind reading, or do they coexist as competing live frameworks held by different institutional seats?',
    'Constitutional doctrine interpretation: if the remedial reading''s core (race-consciousness is constitutionally required for substantive equality) is adopted, does it logically rule out the colorblind reading''s core (race-consciousness violates equal protection)? Or can courts, legislatures, and different institutions hold both simultaneously?',
    'If foreclosure is real, the constraint''s termination depends on which reading wins permanently (a zero-sum contest). If coexistence is the case, both readings persist as live commitments held by different parties, and the remedial mandate operates in a contested field where future reversal remains plausible. The network relationship to the colorblind_reading constraint depends on this determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_and_coexistence, conceptual, 'The logical relationship between this reading and the colorblind reading (kernel_committer factor).').

omega_variable(
    non_preferred_group_status_shift,
    'As demographic composition changes (majority to minority shifts, multiracial identity expansion, immigrant-group incorporation), which groups count as non-preferred victims, and does the remedial mandate track or resist these shifts?',
    'Census data and institutional admissions/hiring data tracking preferred and non-preferred group composition over time; litigation and policy documents where the beneficiary/victim boundary shifts.',
    'If the mandate rigidly preserves original group definitions, it increasingly extracts from newly-arrived groups who did not participate in historical subordination. If it flexibly recalibrates, the constraint''s extraction migrates across populations (higher theater_ratio, churning victim set). Either way, the constraint''s fairness and legitimacy depend on group-boundary choices that are themselves contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_preferred_group_status_shift, empirical, 'How demographic and social change reshapes the victim set and constraint''s extraction profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_clause__remedial_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(equa_tr_t6, equal_protection_clause__remedial_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(equa_tr_t12, equal_protection_clause__remedial_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(equa_tr_t25, equal_protection_clause__remedial_reading, theater_ratio, 25, 0.17).
narrative_ontology:measurement(equa_tr_t37, equal_protection_clause__remedial_reading, theater_ratio, 37, 0.18).
narrative_ontology:measurement(equa_tr_t50, equal_protection_clause__remedial_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_clause__remedial_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(equa_be_t6, equal_protection_clause__remedial_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(equa_be_t12, equal_protection_clause__remedial_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(equa_be_t25, equal_protection_clause__remedial_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(equa_be_t37, equal_protection_clause__remedial_reading, base_extractiveness, 37, 0.68).
narrative_ontology:measurement(equa_be_t50, equal_protection_clause__remedial_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_clause__remedial_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(equa_su_t6, equal_protection_clause__remedial_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement(equa_su_t12, equal_protection_clause__remedial_reading, suppression_requirement, 12, 0.39).
narrative_ontology:measurement(equa_su_t25, equal_protection_clause__remedial_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(equa_su_t37, equal_protection_clause__remedial_reading, suppression_requirement, 37, 0.42).
narrative_ontology:measurement(equa_su_t50, equal_protection_clause__remedial_reading, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_clause__remedial_reading, 0.18).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% The EQUAL_PROTECTION_CLAUSE kernel decomposes into three structurally distinct constraints corresponding to three live interpretive readings held by different institutional actors. Each reading produces a different constraint_id with different beneficiary/victim structures and different ε values. The remedial_reading (this file) requires race-consciousness for structural remedy and carries high ε from extraction of individual opportunity. The colorblind_reading (sibling constraint) forbids racial classifications and carries high ε from what it views as the remedial extraction itself. The diversity_reading (sibling constraint) permits race-consciousness for educational interest and carries moderate ε from competitive disadvantage justified by shared benefit. All three are live positions in American constitutional law; no single reading has foreclosed the others (though momentary dominance by one or another occurs as courts change composition). The three constraints are linked via network.affects_constraints to enable contamination-propagation analysis: if one reading's legitimacy erodes (e.g., through empirical refutation of a key premise), downstream effects ripple through the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
