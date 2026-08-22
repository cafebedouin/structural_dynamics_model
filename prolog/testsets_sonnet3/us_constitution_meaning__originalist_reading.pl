% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Meaning (Fixed at Ratification)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story authors the originalist reading of the contested US
 *   constitutional-meaning kernel: constitutional text has a semantic content
 *   fixed at the moment of ratification (or, for amendments, at the
 *   amendment's ratification date), and judges are bound by evidence of that
 *   historical public meaning rather than by contemporary moral consensus or
 *   evolving social attitudes, though contemporary facts may bear on how a
 *   fixed meaning applies to new circumstances. This is generated as a
 *   single, ε-invariant constraint distinct from the living-constitutionalist
 *   reading (evolving-application constraint, different beneficiary/victim
 *   structure, likely lower measured suppression against rights claimants and
 *   higher against legislative majorities) and the positivist reading
 *   (validity from enactment procedure, indifferent to either fixed-meaning
 *   or evolving-application content). The three readings are not the same
 *   constraint measured three ways — they have different ε, different
 *   victims, and are linked here only through network.affects_constraints and
 *   cs_structure.reading_relations, per the ε-invariance and kernel-reading
 *   rules.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: administers the interpretive method, selects historical sources, institutional power, arbitrage exit (career and doctrinal legacy insulated from any single case)
 *   - counter_majoritarian_constraint_advocates: organized beneficiary, receives durable substantive wins framed as methodologically compelled
 *   - rights_claimants_without_18th_century_analogue: powerless payer, trapped exit (constitutional forum is the only forum with supremacy over legislation), bears the cost when no ratification-era analogue exists
 *   - marginalized_groups_excluded_from_ratification_era_polity: powerless payer, generational time horizon, structurally absent from the very historical record used to adjudicate their present claims
 *   - constitutional_law_scholarship: analytical observer, documents indeterminacy in the historical record the method claims is determinate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.71).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Reading of Constitutional Meaning (Fixed at Ratification)").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '099030d5-d8df-4c00-998a-db9e467ca188').
narrative_ontology:cs_kernel_codification('099030d5-d8df-4c00-998a-db9e467ca188', fixed_text).
narrative_ontology:cs_authority_grounding('099030d5-d8df-4c00-998a-db9e467ca188', lineage).
narrative_ontology:cs_interpretation_layer_present('099030d5-d8df-4c00-998a-db9e467ca188').
narrative_ontology:cs_reading_relation('099030d5-d8df-4c00-998a-db9e467ca188', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('099030d5-d8df-4c00-998a-db9e467ca188', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('099030d5-d8df-4c00-998a-db9e467ca188', foundational, semantic_content_fixed_at_ratification).
narrative_ontology:cs_axiom_status(semantic_content_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('099030d5-d8df-4c00-998a-db9e467ca188', semantic_content_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('099030d5-d8df-4c00-998a-db9e467ca188', secondary, contemporary_moral_consensus_irrelevant_to_meaning).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_irrelevant_to_meaning, holdable).
narrative_ontology:cs_axiom_grounding('099030d5-d8df-4c00-998a-db9e467ca188', contemporary_moral_consensus_irrelevant_to_meaning, deontological).
narrative_ontology:cs_reference_frame('099030d5-d8df-4c00-998a-db9e467ca188', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('099030d5-d8df-4c00-998a-db9e467ca188', post_1970s_originalist_institutionalization, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('099030d5-d8df-4c00-998a-db9e467ca188', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, federalist_society_aligned_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, settled_property_and_contract_interests).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_18th_century_analogue).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, marginalized_groups_excluded_from_ratification_era_polity).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, reproductive_and_lgbtq_rights_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges and justices who apply historical-public-meaning methodology to resolve constitutional disputes, treating 1788 (or amendment-ratification-date) semantic content as dispositive and framing contemporary moral or social consensus as irrelevant to what the text means, though sometimes relevant to how a fixed meaning applies to new facts. They administer the interpretive method, select which historical sources count as evidence, and can shift outcomes by how they characterize the historical record.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Legal movements, scholars, and political coalitions who value a judiciary insulated from shifting popular majorities and value the predictability and legitimacy narrative originalism supplies. They receive a durable interpretive framework that locks in outcomes favorable to their substantive commitments (limited federal power, gun rights, property protections) by presenting those outcomes as required by fixed historical meaning rather than contestable policy choices.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, civilizational, arbitrage, national).

% Holders of established economic and property arrangements benefit from an interpretive method that treats 18th-century assumptions about property, contract, and limited government as the semantic baseline, which tends to disfavor redistributive or regulatory innovations framed as constitutionally suspect departures from original meaning.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, settled_property_and_contract_interests, beneficiary,
    powerful, generational, arbitrage, national).

% Litigants asserting rights or protections (novel privacy claims, certain reproductive autonomy claims, some anti-discrimination theories) for which historical ratification-era sources offer no clear textual or traditional analogue. Their claims are evaluated against a historical record compiled by a polity that did not contemplate their situation; when no 1788 analogue is found, the claim fails as a matter of asserted constitutional meaning regardless of contemporary moral consensus. They cannot exit the constitutional forum — it is the only forum with supremacy over contrary legislation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_18th_century_analogue, payer,
    powerless, biographical, trapped, national).

% Groups who had no voice in 1788 ratification or in the drafting of most amendments (women, enslaved and formerly enslaved people prior to the 13th/14th/15th Amendments, Indigenous nations, non-property-holding classes) find that the 'public meaning' the method privileges is the meaning as understood by an electorate that structurally excluded them. Their present-day constitutional claims are tested against a historical understanding they had no part in forming.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, marginalized_groups_excluded_from_ratification_era_polity, payer,
    powerless, generational, trapped, national).

% Litigants seeking constitutional protection for autonomy and relationship rights encounter a method that asks whether the specific right was 'deeply rooted in this Nation's history and tradition' as of the relevant fixation date. Where the historical record instead documents criminalization or silence, the right does not survive as a matter of original meaning; legislative or political recourse remains the only channel, and that channel is itself majoritarian, which is the condition the method was partly designed to bypass for other claims but not for theirs.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, reproductive_and_lgbtq_rights_litigants, payer,
    moderate, biographical, constrained, national).

% Jurists and scholars who hold that constitutional principles endure while application evolves are institutionally present on the bench and in the academy but are treated, within the originalist reading's own framework, as applying an illegitimate method rather than as offering a competing account of the same kernel. Their objections are litigated in dissents and law review articles but do not alter the fixed-meaning premise from inside this reading.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, living_constitutionalist_judges_and_scholars, excluded,
    organized, civilizational, constrained, national).

% Historians, legal scholars, and empirical corpus-linguistics researchers who examine whether the historical record the method relies on actually yields the determinate answers the method claims, and who document the selection and construction problems inherent in reconstructing 'public meaning' from incomplete 18th-century sources.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, constitutional_law_scholarship, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, allegedly determinate baseline for resolving constitutional disputes across a diverse and disagreeing polity, aiming to reduce the appearance (and some of the reality) of judges substituting current policy preferences for law, and to anchor judicial legitimacy in something other than the judges' own values.
% TRANSFER_FUNCTION: Moves outcome-determining power from contemporary democratic majorities and from rights claimants asserting novel or evolving protections, to historical evidentiary reconstruction controlled by judges who select, weight, and characterize ratification-era sources — and correspondingly moves substantive victories toward litigants whose claims map onto 18th/19th-century analogues.
% ABSENT_VOICES: The persons who were excluded from the ratifying and amending polities — women, enslaved people, Indigenous nations, non-propertied classes — are not present to give historical evidence of what public meaning was for them, because the political community from which 'the public' is drawn did not include them; their absence from the historical record is treated as absence of a claim rather than as evidence of exclusion.
% DISAPPEARANCE_RATIONALE: If originalist methodology vanished overnight, a substantial body of case law resolved by appeal to fixed historical meaning (gun rights doctrine, administrative law limits, certain federalism holdings, the overruling of substantive due process precedents) would lose its stated justificatory basis; litigation strategy, judicial confirmation politics, and law school curricula built around historical-meaning argumentation would have to reorganize around a different interpretive currency.
% FOUNDING_PROBLEM: The problem the modern originalist movement was built to solve (from the mid-20th century onward, though it claims continuity with founding-era interpretive practice) was perceived judicial activism: the Warren and Burger Courts' willingness to derive new rights and limit legislative power based on evolving standards, which originalists characterized as judges legislating from the bench under cover of constitutional interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and the Federalist Society-aligned legal movement attest the problem (unconstrained judicial policymaking) remains live and that fixed meaning is the only principled check. Legal historians and corpus linguists outside that movement — including originalism-sympathetic scholars who have documented indeterminacy in the historical record — attest that the 'fixed, determinate, discoverable' premise is itself frequently unsupported by the actual historical evidence, and that the method's constraint is often illusory at the moments of genuine indeterminacy, functioning instead as a legitimating vocabulary for judicially selected outcomes.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (rising over the measured interval from 0.22 in 1971) reflecting increasing reliance on historical-meaning argumentation to foreclose rights claims and regulatory outcomes that lack 18th/19th-century analogues, without the extraction depending on any single doctrinal outcome — it is a property of the method's operation over time. Suppression (0.71) is authored high and rising (0.35 to 0.71) because the method's force depends on treating non-originalist interpretive results as illegitimate rather than merely mistaken, which forecloses argumentative space for claimants whose rights have no period analogue, independent of scope or power scaling (suppression is unscaled per the framework's rule). Theater ratio is modest (0.28) and rising slowly — the historical-research function is often genuinely performed (amicus historian briefs, corpus linguistics submissions) even where scholars dispute whether the record actually yields the claimed determinacy; this is a real interpretive practice with a growing theatrical residue, not a pure performance. Accessibility collapse (0.62) and resistance (0.68) are both substantial: alternatives (competing interpretive methods) remain live and contested in the academy and on multi-member courts, so collapse is not mountain-level, but resistance from living-constitutionalist and positivist camps, and from rights litigants, is real and organized, not negligible.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (originalist judiciary) and the beneficiary seat (counter-majoritarian advocates), the constraint is coordination: a shared, principled baseline that disciplines judicial discretion and protects long-run institutional legitimacy. From the payer seats (rights claimants without historical analogue, excluded marginalized groups), the same fixed-meaning requirement operates as an enforced foreclosure — their present-day claim is tested against a historical record from a polity that did not include them, and failure to find an 18th-century analogue is treated as dispositive of the constitutional question rather than as an artifact of who got to speak in 1788. The engine should compute a tangled-rope or more extractive classification from these payer seats even where the agenda_setter seat's own framing would compute closer to coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Counter-majoritarian-constraint advocates and settled-property/contract interests are declared beneficiaries: the method's operation is structured to deliver outcomes congenial to their substantive commitments while offering a legitimating vocabulary (fixed meaning, judicial restraint) rather than an admission of policy preference — low d, benefit-side. Rights claimants without 18th-century analogues and marginalized groups excluded from the ratifying polity are declared victims: trapped exit options (no forum above the constitutional one), and the very historical record used against them was constructed without their participation — high d, target-side. The originalist judiciary sits as agenda_setter with institutional power and arbitrage exit: individual judges face limited personal cost regardless of outcome, and control which historical sources and framings count, which is why the seat administering the method and the seats bearing its costs compute so differently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the modern originalist movement claims to solve — unconstrained mid-20th-century judicial policymaking — is contested as to whether it remains live: originalist advocates hold judges still substitute preference for law absent the constraint; outside scholars (including some originalism-sympathetic historians) document that the 'fixed and discoverable meaning' premise is frequently indeterminate at exactly the contested margins, meaning the constraint may be performing a legitimating function (making judicially chosen outcomes look historically compelled) rather than the constraining function it claims. This mismatch — founding_problem_status: contested, disappearance_verdict: world_rearranges — is the signal the mandatrophy analysis is built to surface: the arrangement plainly organizes real stakes (world_rearranges if it vanished) while its own claimed justification is disputed by parties outside its beneficiary set, which is exactly the configuration that should route to a capture/legitimation flag rather than a clean coordination reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determinacy,
    'Does the historical record from 1788 (or the relevant amendment date) actually yield a determinate, discoverable public meaning for the contested clauses, or is the appearance of determinacy itself constructed by selective sourcing and framing?',
    'Corpus-linguistics and historical-methods scholarship (including work by originalism-sympathetic historians) comparing independently reconstructed historical understandings across contested clauses; convergence would support determinacy, persistent scholarly disagreement about what the sources show would support constructed indeterminacy.',
    'If the historical record is genuinely determinate, the method functions closer to a constraining rope that binds the judiciary''s own discretion; if the record is substantially indeterminate at the contested margins, the appearance of constraint is doing legitimating work for otherwise-discretionary judicial choices, supporting the tangled_rope/higher-extraction reading authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Whether ratification-era historical meaning is genuinely determinate or constructed-as-determinate.').

omega_variable(
    excluded_polity_evidentiary_problem,
    'How should the method treat the fact that the ''public'' whose meaning is being reconstructed structurally excluded the groups whose present-day rights claims the method now adjudicates?',
    'Comparative doctrinal analysis of how courts applying originalist method have treated claims by groups absent from the ratifying polity (women pre-19th Amendment, formerly enslaved people pre-Reconstruction Amendments) versus claims by groups present in it, controlling for claim type.',
    'If outcomes systematically disfavor claims by historically excluded groups independent of claim merit, this strengthens the victim-side directionality and the tangled_rope classification; if outcomes are unaffected by this exclusion pattern once controlling for other factors, the extraction is more diffuse than group-targeted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_polity_evidentiary_problem, conceptual, 'Whether the ratifying polity''s historical exclusions structurally disadvantage present-day claims by descendant groups.').

omega_variable(
    sibling_reading_framing_choice,
    'Given that the same constitutional text supports at least three structurally distinct readings (originalist, living-constitutionalist, positivist) with different beneficiary/victim structures, what signals guided classifying THIS constellation as tangled_rope rather than snare or rope for the originalist reading specifically?',
    'Cross-check against the disappearance_verdict (world_rearranges) and the presence of a genuine, non-pretextual coordination function (reducing unconstrained judicial discretion) alongside a documented, asymmetric cost to historically excluded claimants — both required for tangled_rope. If the coordination function is shown to be pretextual (per the historical_meaning_determinacy omega), the classification should move toward snare.',
    'Resolving this in favor of pretextual coordination would shift the story from tangled_rope to snare, removing the beneficiary-side legitimacy the current classification grants the counter-majoritarian-constraint function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_choice, conceptual, 'Documents the framing choice between rope-like, tangled-rope, and snare readings of the same structural data, per the CS-framing under-determination guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1971, us_constitution_meaning__originalist_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_meaning__originalist_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(us_c_tr_t1995, us_constitution_meaning__originalist_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(us_c_tr_t2008, us_constitution_meaning__originalist_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_meaning__originalist_reading, theater_ratio, 2016, 0.26).
narrative_ontology:measurement(us_c_tr_t2022, us_constitution_meaning__originalist_reading, theater_ratio, 2022, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1971, us_constitution_meaning__originalist_reading, base_extractiveness, 1971, 0.22).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_meaning__originalist_reading, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(us_c_be_t1995, us_constitution_meaning__originalist_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(us_c_be_t2008, us_constitution_meaning__originalist_reading, base_extractiveness, 2008, 0.45).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_meaning__originalist_reading, base_extractiveness, 2016, 0.51).
narrative_ontology:measurement(us_c_be_t2022, us_constitution_meaning__originalist_reading, base_extractiveness, 2022, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1971, us_constitution_meaning__originalist_reading, suppression_requirement, 1971, 0.35).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_meaning__originalist_reading, suppression_requirement, 1985, 0.44).
narrative_ontology:measurement(us_c_su_t1995, us_constitution_meaning__originalist_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(us_c_su_t2008, us_constitution_meaning__originalist_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_meaning__originalist_reading, suppression_requirement, 2016, 0.64).
narrative_ontology:measurement(us_c_su_t2022, us_constitution_meaning__originalist_reading, suppression_requirement, 2022, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the meaning of the US Constitution' per the ε-invariance principle. Each reading (originalist, living-constitutionalist, positivist) instantiates a structurally distinct constraint with its own ε, beneficiary/victim structure, and classification, linked here rather than folded into a single observer-relative story. The originalist reading is authored with rising extraction and suppression over the 1971-2022 interval as the movement institutionalized; the sibling stories should not be assumed to share this trajectory.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
