% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story models the originalist reading of constitutional interpretive
 *   authority: the claim that constitutional meaning was fixed at
 *   ratification (or, for post-Civil War amendments, at their own
 *   ratification) and that legitimate interpretive authority derives from
 *   fidelity to that fixed meaning rather than contemporary judicial
 *   judgment. This is ONE of three structurally distinct readings of a single
 *   contested kernel — the living_constitution_reading and
 *   popular_constitutionalism_reading are separate constraint stories with
 *   their own ε, beneficiaries, and victims, linked here via
 *   network.affects_constraints. Do not read this story as covering the whole
 *   originalism/living-constitution debate; it covers only the structural
 *   consequences of adopting the originalist premise as controlling
 *   interpretive method.
 *
 * KEY AGENTS:
 *   - originalist_judiciary: agenda_setter (institutional/analytical) — administers the interpretive method and its evidentiary standards
 *   - federalism_advocates, religious_liberty_claimants_original_understanding, property_rights_defenders: beneficiaries (organized-powerful/mobile) — gain favorable doctrinal outcomes when courts anchor meaning to 1787-1868 understanding
 *   - unenumerated_rights_claimants, federal_regulatory_expansion_advocates: payers (powerless-moderate/trapped-constrained) — bear the cost of claims that cannot ground themselves in historical practice
 *   - constitutional_historians: excluded — professional expertise on genuine historical indeterminacy is selectively engaged with, not fully incorporated
 *   - legal_academy_observers: analytical observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.48).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '4448f2ad-506a-43dc-86ed-39804769012b').
narrative_ontology:cs_kernel_codification('4448f2ad-506a-43dc-86ed-39804769012b', fixed_text).
narrative_ontology:cs_authority_grounding('4448f2ad-506a-43dc-86ed-39804769012b', lineage).
narrative_ontology:cs_interpretation_layer_present('4448f2ad-506a-43dc-86ed-39804769012b').
narrative_ontology:cs_reading_relation('4448f2ad-506a-43dc-86ed-39804769012b', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('4448f2ad-506a-43dc-86ed-39804769012b', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('4448f2ad-506a-43dc-86ed-39804769012b', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('4448f2ad-506a-43dc-86ed-39804769012b', meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('4448f2ad-506a-43dc-86ed-39804769012b', foundational, judicial_legitimacy_requires_historical_fidelity).
narrative_ontology:cs_axiom_status(judicial_legitimacy_requires_historical_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('4448f2ad-506a-43dc-86ed-39804769012b', judicial_legitimacy_requires_historical_fidelity, instrumental).
narrative_ontology:cs_reference_frame('4448f2ad-506a-43dc-86ed-39804769012b', ratification_era_public_meaning).
narrative_ontology:cs_drift_state('4448f2ad-506a-43dc-86ed-39804769012b', contemporary_judicial_practice, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4448f2ad-506a-43dc-86ed-39804769012b', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal judges and justices who adjudicate cases by reconstructing 1787-1791 (or 1868, for Reconstruction Amendments) understanding of contested text. They administer the interpretive method itself, deciding which historical sources count as evidence of original meaning and how disputes at the margins of that evidence are resolved. Their authority rests on the claim that this method constrains judicial discretion rather than exercising it.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, originalist_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% State governments, states-rights litigants, and political coalitions favoring devolution of federal power. The originalist reading of enumerated powers and the Tenth Amendment narrows the scope of federal regulatory authority, returning contested policy domains to state legislatures where these actors have more direct access and influence.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, generational, mobile, national).

% Religious institutions and individuals whose free-exercise and establishment-clause claims are read favorably when courts anchor Religion Clause meaning to founding-era practice, which often permitted more public religious expression than mid-20th-century doctrine allowed. They gain expanded protection when historical practice is read as the controlling baseline.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding, beneficiary,
    organized, generational, mobile, national).

% Landowners, developers, and takings-clause litigants who benefit when courts read property protections at their broadest historical scope and read federal regulatory takings narrowly against 1787-era common-law property conceptions, limiting the reach of environmental and land-use regulation.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    powerful, generational, mobile, national).

% Individuals asserting rights not enumerated in the constitutional text and not clearly recognized at ratification or incorporation-era understanding — reproductive autonomy claimants, LGBTQ rights litigants, and privacy-interest holders whose claims depend on a living or evolving reading of liberty. Under the originalist reading, their claims must find footing in historical practice that frequently did not contemplate or affirmatively excluded them; they cannot exit the jurisdiction of the interpretive method and cannot litigate their way around it once it is entrenched in controlling precedent.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Administrative agencies, regulatory reform coalitions, and legislators seeking to address novel problems (financial systemic risk, environmental harms, digital-era commerce) through federal action. Originalist Commerce Clause and nondelegation readings narrow the available constitutional basis for such action, forcing reliance on state-by-state patchwork solutions or constitutionally vulnerable federal statutes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    moderate, biographical, constrained, national).

% Professional historians who study the actual complexity, contradiction, and indeterminacy of founding-era public meaning often find that the historical record is more contested and ambiguous than the interpretive method's confident reconstructions suggest. Their scholarly findings on genuine indeterminacy are frequently absent from, or subordinated within, judicial opinions that need a determinate answer to decide a case.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, constitutional_historians, excluded,
    moderate, generational, analytical, national).

% Scholars who study the originalist method's internal consistency, its selective use of historical evidence, and its outcomes across doctrinal areas, without a direct stake in any particular case's outcome.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, legal_academy_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, precedent-stabilizing decision procedure for resolving constitutional disputes by anchoring interpretation to a fixed historical referent, reducing (in principle) the discretion of unelected judges to substitute contemporary policy preference for legal meaning.
% TRANSFER_FUNCTION: Shifts the burden of constitutional proof: claims that map onto 1787-1868 era understanding are validated with reduced scrutiny of contemporary consequence, while claims resting on rights or federal powers unrecognized at that time face a heightened, often decisive, evidentiary and historical burden. This moves litigation outcomes systematically toward parties whose claims track historical practice and away from parties whose claims arise from post-ratification social change.
% ABSENT_VOICES: Constitutional historians whose scholarship documents genuine indeterminacy, contradiction, and contestation within the founding-era record are frequently cited selectively rather than engaged with fully; communities excluded from full political participation at ratification (enslaved people, women, non-property-holders) have no voice in the 'original public meaning' the method reconstructs, an absence built into the method's own historical object.
% DISAPPEARANCE_RATIONALE: If originalism disappeared as the controlling interpretive method overnight, federal power under the Commerce Clause and other enumerated powers would likely expand, unenumerated liberty and privacy claims would gain firmer doctrinal footing, and religious-liberty and property-rights doctrine would shift toward balancing tests rather than historical-scope tests. Decades of precedent built on originalist reasoning would require re-litigation across constitutional law.
% FOUNDING_PROBLEM: The perceived problem, as articulated by the modern originalist movement (chiefly from the 1970s-1980s), was that mid-20th-century judicial decisions (particularly the Warren and early Burger Courts) had substituted judges' contemporary policy preferences for constitutional text, making constitutional law unpredictable and democratically illegitimate. Originalism was proposed as a discipline to constrain judicial discretion and restore legislative primacy over policy questions not textually assigned to courts.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the originalist movement (and outside the rights-claimant groups who bear its costs) have documented that originalist method exhibits substantial judicial discretion in selecting among competing historical narratives, meaning the 'discretion-constraining' function is itself empirically contested rather than settled. Some conservative legal scholars affiliated with the movement acknowledge the historical record is frequently indeterminate but argue determinacy is not required for legitimacy. No corroboration exists from a source that is simultaneously outside the beneficiary coalition (federalism/religious-liberty/property-rights advocates) and unaffiliated with the interpretive method's academic architects that confirms the discretion-constraint function operates as claimed in practice.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects that the originalist method systematically reallocates litigation outcomes toward historically-groundable claims and away from claims arising from post-ratification social change, without this being a neutral or symmetric filter — the exclusion of enslaved people, women, and non-property-holders from the founding-era political community that generated 'original public meaning' is baked into the reconstructed historical object itself. Suppression (0.48) is moderate: the method does not physically coerce, but it forecloses litigation strategies once entrenched in controlling precedent, and dissenting historical scholarship faces genuine difficulty being heard on its own terms within a mode of reasoning that requires a determinate answer. Theater ratio (0.28) is low-moderate: the historical research underlying originalist opinions is often genuine scholarly effort, but a meaningful share of the 'original meaning' finding is reverse-engineered to reach an outcome, which is why it is above negligible. Accessibility collapse (0.40) and resistance (0.62) reflect that this is a contested political-legal method, not settled law — competing methodologies remain fully live in courts, legislatures, and legal scholarship, and mount active, organized resistance (the living constitution and popular constitutionalism traditions are precisely that resistance, formalized as sibling readings).
 *
 * DIRECTIONALITY LOGIC:
 *   Federalism advocates, religious liberty claimants under original understanding, and property rights defenders are beneficiaries because the originalist method's structural bias toward historically-recognized state and property prerogatives directly serves their litigation and political goals; their exit options are mobile because they can pursue parallel political and legislative strategies alongside litigation. Unenumerated rights claimants sit at the powerless end with trapped exit because their claims are structurally dependent on the very interpretive regime that disfavors them — there is no alternative forum once originalist precedent controls a doctrinal area. Federal regulatory expansion advocates have moderate power and constrained exit: they retain some legislative and political recourse but face a narrower constitutional runway for federal action.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unconstrained judicial policy-making in the mid-20th century — is contested rather than resolved or clearly still-live: originalism's proponents maintain the discipline is still needed as backstop; its critics and outside historians document that the method itself exercises comparable interpretive discretion in selecting among competing historical narratives, which would make the arrangement partially a zombie mandate maintained by its own beneficiary coalition rather than a functioning discretion-constraint. This story does not resolve that dispute; it flags the founding_problem_status as contested with corroboration explicitly noted as absent from any source both outside the beneficiary coalition and unaffiliated with the method's academic architects — a genuine data gap, not an oversight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalism_kernel_reading_alternative,
    'Is the originalist reading''s claim to discretion-minimization empirically distinguishable from the living_constitution and popular_constitutionalism readings, or does every reading of the kernel exercise comparable interpretive discretion under a different vocabulary?',
    'Comparative empirical study of inter-judge and inter-era outcome variance under originalist versus non-originalist controlling doctrine, holding case type constant; convergent findings across multiple independent research teams outside all three interpretive traditions would resolve this.',
    'If discretion is comparable across readings, the originalist reading''s central legitimacy claim (constraining judicial policy-making) collapses and the reading functions primarily as a vehicle for the substantive outcomes its beneficiary coalition favors rather than as a genuine discretion-minimizing procedure — strengthening a tangled_rope-to-snare drift assessment. If originalism genuinely reduces outcome variance relative to the alternatives, its coordination claim is stronger than critics allow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_kernel_reading_alternative, empirical, 'Whether originalism''s discretion-constraining function is real or a vocabulary difference among equally discretionary methods.').

omega_variable(
    founding_era_meaning_indeterminacy,
    'Is ''original public meaning'' at ratification sufficiently determinate on the contested doctrinal questions (Commerce Clause scope, incorporation, unenumerated rights) to ground the reading''s coordination claim, or is the historical record indeterminate enough that originalist opinions substantially construct rather than discover the meaning they claim to find?',
    'Systematic historiographical review by professional historians (outside constitutional law as a discipline) of the founding-era record on specific contested clauses, assessing consensus versus contestation among historians who are not themselves constitutional-law academics with a stake in the debate.',
    'High indeterminacy would support classifying this reading''s coordination function as substantially cover for a results-oriented method (pushing toward snare); genuine determinacy on most contested questions would support the coordination claim as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_era_meaning_indeterminacy, empirical, 'Whether founding-era constitutional meaning is determinate enough to bear the interpretive weight originalism places on it.').

omega_variable(
    exclusion_of_founding_era_nonparticipants,
    'Does the structural exclusion of enslaved people, women, and non-property-holders from the political community that generated ''original public meaning'' constitute an irreducible defect in the reading''s legitimacy claim, or is it a historical fact that can be bracketed from the interpretive method''s present-day operation?',
    'This is fundamentally a normative/conceptual question about what counts as a legitimate source of constitutional authority, not resolvable by further empirical historical research alone — though historical research on how exclusion shaped the specific textual and doctrinal content in question is relevant evidence.',
    'If treated as an irreducible defect, it substantially raises the reading''s effective extraction from groups whose current claims trace to interests unrepresented in the original meaning-making community; if bracketed as historically contingent but methodologically irrelevant, the extractiveness measure would be lower than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_of_founding_era_nonparticipants, conceptual, 'Whether founding-era political exclusion is a live defect in or a bracketable feature of the originalist legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_interpretive__originalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_interpretive__originalist_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_interpretive__originalist_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_interpretive__originalist_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(us_c_tr_t2018, us_constitution_interpretive__originalist_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_interpretive__originalist_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1980, us_constitution_interpretive__originalist_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_interpretive__originalist_reading, base_extractiveness, 1990, 0.34).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_interpretive__originalist_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_interpretive__originalist_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(us_c_be_t2018, us_constitution_interpretive__originalist_reading, base_extractiveness, 2018, 0.47).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_interpretive__originalist_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1980, us_constitution_interpretive__originalist_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_interpretive__originalist_reading, suppression_requirement, 1990, 0.33).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_interpretive__originalist_reading, suppression_requirement, 2000, 0.36).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_interpretive__originalist_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(us_c_su_t2018, us_constitution_interpretive__originalist_reading, suppression_requirement, 2018, 0.44).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_interpretive__originalist_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__originalist_reading, 0.1).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the us_constitution_interpretive kernel. living_constitution_reading and popular_constitutionalism_reading are separate constraint files with independently authored ε, beneficiaries, and victims — they are not alternative measurements of this same constraint but structurally distinct constraints sharing a contested kernel (the Constitution's meaning and the source of interpretive legitimacy). Each reading reallocates a different set of beneficiaries and victims; per DP-001 (ε-invariance), they must not be merged or averaged into a single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
