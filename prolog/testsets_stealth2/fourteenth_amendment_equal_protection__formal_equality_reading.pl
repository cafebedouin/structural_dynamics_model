% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection — Formal Equality Reading (Colorblind Classification Bar)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story authors the formal-equality reading of the Fourteenth
 *   Amendment's Equal Protection Clause as a single ε-invariant constraint:
 *   the operative rule that a state may not expressly classify persons by
 *   race or analogous status absent a compelling justification, enforced by
 *   federal judicial review. The standing arrangement under contest — the
 *   referent for ε — is the doctrine as actually administered from the
 *   wartime articulation of scrutiny through the 2023 closure of the
 *   diversity window, assessed by this reading's own lights: the reading
 *   regards the rule as the Constitution's fulfillment, protecting every
 *   person from caste legislation, and it counts the rule's binding of
 *   remedial programs as the rule succeeding rather than as a cost — which is
 *   why its reading-indexed ε stays low even as the doctrine's operative
 *   burden shifted onto state corrective action. Constraint-family note: the
 *   colloquial label 'what Equal Protection requires' decomposes into this
 *   story and its sibling, the anti-caste reading (linked via
 *   network.affects_constraints); the sibling shares this referent and
 *   authors high ε over it, locating the disagreement in whether the clause
 *   mandates race-conscious dismantling or forbids all express
 *   classification. Per the claim/metric independence rule, claimed_type
 *   records this author's structural judgment while the metrics record
 *   descriptive operation; where they diverge, that divergence is the
 *   measurement.
 *
 * KEY AGENTS:
 *   - - supreme_court_equal_protection_interpreter: agenda-setter (institutional/identity_locked) — administers the rule and writes its meaning; collects doctrinal authority
 *   - - historically_discriminated_racial_minorities: declared protected class (organized/trapped) — shielded from hostile classification; barred from remedial instruments
 *   - - asian_american_college_applicants: declared protected class (moderate/mobile) — the litigating class of the anti-balancing suits
 *   - - underrepresented_minority_applicants: bearer of the rule's remedial-side costs (moderate/constrained)
 *   - - public_university_admissions_authorities: regulated institution (institutional/constrained) — redraws admissions under the rule
 *   - - minority_business_contractors: bearer of invalidated set-aside losses (moderate/constrained)
 *   - - structural_inequality_reparations_advocates: excluded voice (organized/trapped) — outside the rule's construction
 *   - - constitutional_law_scholars: analytical observer — maps the doctrine and supplies both coalitions' arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.34).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.72).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection — Formal Equality Reading (Colorblind Classification Bar)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '06775d8e-15b0-40ca-a320-0042f43298b9').
narrative_ontology:cs_kernel_codification('06775d8e-15b0-40ca-a320-0042f43298b9', fixed_text).
narrative_ontology:cs_authority_grounding('06775d8e-15b0-40ca-a320-0042f43298b9', lineage).
narrative_ontology:cs_interpretation_layer_present('06775d8e-15b0-40ca-a320-0042f43298b9').
narrative_ontology:cs_reading_relation('06775d8e-15b0-40ca-a320-0042f43298b9', fourteenth_amendment_equal_protection__anti_caste_reading, forecloses).
narrative_ontology:cs_axiom('06775d8e-15b0-40ca-a320-0042f43298b9', foundational, express_state_racial_classification_presumptively_unconstitutional).
narrative_ontology:cs_axiom_status(express_state_racial_classification_presumptively_unconstitutional, holdable).
narrative_ontology:cs_axiom_grounding('06775d8e-15b0-40ca-a320-0042f43298b9', express_state_racial_classification_presumptively_unconstitutional, deontological).
narrative_ontology:cs_axiom('06775d8e-15b0-40ca-a320-0042f43298b9', secondary, structural_inequality_is_preconstitutional_background).
narrative_ontology:cs_axiom_status(structural_inequality_is_preconstitutional_background, holdable).
narrative_ontology:cs_axiom_grounding('06775d8e-15b0-40ca-a320-0042f43298b9', structural_inequality_is_preconstitutional_background, conventional).
narrative_ontology:cs_reference_frame('06775d8e-15b0-40ca-a320-0042f43298b9', colorblind_formal_symmetry).
narrative_ontology:cs_drift_state('06775d8e-15b0-40ca-a320-0042f43298b9', post_sffa_2023, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('06775d8e-15b0-40ca-a320-0042f43298b9', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, historically_discriminated_racial_minorities).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, asian_american_college_applicants).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, underrepresented_minority_applicants).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, public_university_admissions_authorities).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, minority_business_contractors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, public_university_admissions_authorities).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, historically_discriminated_racial_minorities).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_for_racial_classifications).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, state_action_doctrine_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which state racial classifications stand and which fall, and in doing so writes the clause's operative meaning. Its institutional authority consists in this interpretive office: it can shift between ways of reading the text — and has, several times across the interval — but it cannot delegate or abandon the interpretive function without dissolving the basis of its own power. Each landmark ruling concentrates further authority over the text's meaning in this seat.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court_equal_protection_interpreter, agenda_setter,
    institutional, generational, identity_locked, national).

% Draw on the rule as a shield: jury exclusion, school segregation, and vote dilution have all fallen to it. The same rule forecloses race-conscious programs designed for their advancement, so the seat is doubly placed — protected against hostile classification by the very standard that blocks remedial classification on their behalf. Exit from the constitutional order is not available; advocacy organizations give the seat organized voice.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, historically_discriminated_racial_minorities, beneficiary,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, historically_discriminated_racial_minorities, payer).

% Litigated as the class said to be disadvantaged when selective institutions weigh race in admissions. Wherever race-conscious review is absent, the claimed injury does not arise; applicants can and do apply across many systems, including ones that never weighed race, so the seat's exposure is partial and portable.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, asian_american_college_applicants, beneficiary,
    moderate, biographical, mobile, national).

% Lost the weighted-review pathway at selective public institutions when the doctrine closed the diversity window. Remaining routes are race-neutral proxies — percentage plans, socioeconomic weighting — of uncertain reach into the same pools. The seat retains the rule's protection against hostile classification while bearing the removal of the remedial instrument.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, underrepresented_minority_applicants, payer,
    moderate, biographical, constrained, national).

% Must assemble entering classes without racial weighing, redesigning processes, absorbing litigation exposure, and living with the enrollment-composition consequences. The seat also draws benefits: bright-line rules are easier to administer and defend, and no applicant can demand a racial preference as of right. Leaving the constitutional order is not an option; compliance is the only lane.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, public_university_admissions_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, public_university_admissions_authorities, beneficiary).

% Competed for set-aside and price-preference programs that the doctrine invalidated beginning with municipal contracting in 1989. Now bid in open competitions whose field is shaped by decades of prior exclusion; firm capacity and balance sheets reflect the programs' absence.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, minority_business_contractors, payer,
    moderate, biographical, constrained, regional).

% Seek legislation that classifies by race for corrective purposes — reparations, targeted investment, set-asides. Their proposals reach the courts only to be measured against the compelling-interest standard they reject as the wrong test entirely. They organize, publish, and lobby, but hold no seat in the rule's construction other than as recurring losing litigants.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, structural_inequality_reparations_advocates, excluded,
    organized, generational, trapped, national).

% Map the doctrine's evolution, reconstruct the founding record, and supply the competing arguments both litigation coalitions deploy. The seat observes and argues but neither administers the rule nor bears its direct incidence.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, supreme_court_equal_protection_interpreter).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one uniform, judicially administrable rule that binds every state simultaneously against express racial classification, replacing case-by-case political balancing in fifty legislatures with a single federal standard, and giving every person a predictable claim against state-imposed racial disability.
% TRANSFER_FUNCTION: Moves discretion over racial categorization from state legislatures, universities, and agencies to federal courts; and, in its modern operation, preserves existing distributions of selective-education access and public contracting from race-conscious reallocation by withdrawing the instruments that would reallocate them.
% ABSENT_VOICES: Advocates of structural and subordination-centered understandings — reparations proponents, critical-race scholars, community organizations seeking race-conscious remedy — object that the symmetry premise treats unequal starting points as background noise. Inside the arrangement they appear only as litigants asking the compelling-interest window to open, a window the doctrine presumes shut; they hold no seat in the rule's construction.
% DISAPPEARANCE_RATIONALE: Overnight repeal would return racial classification to ordinary legislative competence: states could immediately re-enact both hostile exclusions and remedial preferences, selective institutions would rebuild race-conscious review within an admissions cycle, set-aside programs would return, and a wave of litigation would sort favored from disfavored classifications — the allocation of educational and contractual opportunity would visibly reorganize.
% FOUNDING_PROBLEM: The clause was ratified to destroy the slave-power caste system: to place the civil rights of the formerly enslaved and their descendants beyond state legislative discretion, so that no state could again create a hereditary racial caste by law.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside this reading's benefiting parties: anti-caste advocates and critical-race scholars reject this reading's remedy precisely because they take the founding problem to be unresolved; Reconstruction historiography documents the framers' caste-destroying intent; and the continuing docket of hostile-classification cases (jury exclusion, vote dilution, discriminatory enforcement) attests that state caste-making remains a live risk no party disputes.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.34 — low, and deliberately reading-indexed: the formal-equality reading assesses the arrangement it instantiates as protective, and it discounts the burden the rule places on race-conscious remedies as the rule operating correctly rather than as extraction; the residual ε reflects costs the reading itself concedes — litigation burden, chilling of beneficial programs, and the narrowing of the compelling-justification window toward practical nullity. The temporal series shows that residual rising as the doctrine's center of gravity moved from shielding minorities against hostile classification (1944–1968) to striking down remedial classification (1978 through 2023) — extraction accumulation in the T17 sense, though modest by this reading's lights. Suppression (0.72) is a raw structural property, unscaled by power or scope: compliance is compulsory through judicial invalidation and preemptive institutional redesign, and no lawful alternative path exists for the specific instrument the rule forecloses. Theater_ratio (0.45) tracks the growing gap between balancing language (compelling interest, narrow tailoring) and near-categorical outcomes — strict in theory, fatal in fact — while the rule's core work against genuinely hostile classification remains functional. Accessibility_collapse (0.55): race-neutral substitutes (percentage plans, socioeconomic weighting) remain lawful and are pursued, so alternatives contract without vanishing. Resistance (0.65): five decades of constitutional litigation, scholarly opposition, and repeated state-level contests. All three series share one time grid; the 2003 dip reflects the temporary widening of the justification window that year, a doctrinal event, not an oscillation mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the interpreter's seat the arrangement is a coherent, principled symmetry it has refined for eighty years; from the regulated institutions' seat it is a hard compliance border that redrew admissions and contracting practice; from the underrepresented-minority and contractor seats it removes an instrument they could previously invoke while retaining protections they still need; from the declared-protected-class seats it is chiefly a shield. The engine computes these divergent per-seat types from power, exit, and role data; the divergence between the interpreter's experience and the payers' experience is the perspectival fact this story is built to expose.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (historically_discriminated_racial_minorities, asian_american_college_applicants) drive those seats toward the beneficiary end; the dual-positioned minority seat carries a payer secondary role because the same rule forecloses remedial instruments aimed at it. Victim declarations (underrepresented_minority_applicants, public_university_admissions_authorities, minority_business_contractors) drive those seats toward the target end; the university seat's constrained exit — it cannot leave the constitutional order — pushes it toward full-target more strongly than the applicants' partially mobile exits. The interpreter seat is neither subsidized nor billed; it administers, collecting doctrinal authority — structurally nearer neutral, which is why gain_flow names it: the arrangement's gains demonstrably accrue to the seat that controls the kernel's meaning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing state-created racial caste — is live, so no mandatrophy is declared: the arrangement has not outlived its function. What has changed is the function's center of gravity, visible in the temporal series as rising theater and rising suppression pointed at a new object (remedial rather than hostile classification). The classification guards against two mislabels: calling the arrangement pure extraction would erase the genuine, still-operative protection against hostile classification that all seats continue to draw on; calling it pure coordination would erase the asymmetric burden the same structure now places on corrective action and its intended beneficiaries. The honest middle is real coordination with real asymmetry, held together by active enforcement — which is what the claimed type asserts and what the metrics independently describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the fourteenth_amendment_equal_protection kernel (formal_equality_reading). What would change structurally if the anti_caste_reading were adopted instead?',
    'Authorship of the sibling story: the anti-caste file re-declares the victim set (structural hierarchy itself becomes the object of state duty), moves state corrective action from the constrained set to the mandated set, and re-authors ε high over the same referent.',
    'Classification flips from this story''s profile to the sibling''s: the same clause text yields opposite victim sets, opposite ε, and opposite per-seat directionalities; cross-reading comparison is valid only because both stories fix the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what the sibling changes.').

omega_variable(
    compelling_interest_window_nullity,
    'Does the compelling-justification escape valve ever open in practice for remedial racial classifications, or does strict scrutiny operate as a categorical bar?',
    'Compile post-1978 survival rates of race-conscious state programs under strict scrutiny, controlling for program type; compare outcomes before and after the 2023 closure of the diversity window.',
    'If the window is practically null, the arrangement''s effective suppression exceeds its balancing self-description and the theater_ratio understates performance; if it opens for well-built records, the arrangement is genuine balancing and ε sits lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compelling_interest_window_nullity, empirical, 'Whether the doctrine''s balancing language masks a categorical rule.').

omega_variable(
    original_meaning_genealogy_contest,
    'Does the 1866–1868 original meaning support formal symmetry (the colorblind dissent tradition) or anti-caste remedialism (the race-conscious Freedmen''s Bureau legislation enacted alongside the Amendment)?',
    'Reconstruction historiography and close reading of the 39th Congress record; adjudication is unlikely — the ambiguity appears structural to the text.',
    'If remedialism is the better genealogy, this reading''s lineage warrant weakens and the sibling''s claim to the founding problem strengthens; if symmetry, the reverse. Either way the founding problem''s liveness is unaffected.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_meaning_genealogy_contest, conceptual, 'Contested genealogical warrant for the symmetry premise.').

omega_variable(
    symmetry_premise_empirical_warrant,
    'The reading''s justification structure leans on empirical claims — that race-conscious measures stigmatize their beneficiaries, impair matching, and damage social cohesion. Do those claims survive the outcome evidence from race-neutral regimes?',
    'Compare long-run outcomes (enrollment composition, completion, earnings, integration measures) across systems that abandoned race-conscious review versus comparable systems that retained it.',
    'Refutation would push the reading back onto its bare deontological axiom — sustainable but thinner — and raise the reading-indexed ε by removing the benefit-side offsets it currently counts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_premise_empirical_warrant, empirical, 'Empirical load-bearing wall under the symmetry premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1944, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(formal_equality_reading_tr_t1944, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1944, 0.22).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t1944, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1954, 0.17).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t1954, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t1964, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1964, 0.14).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t1964, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.3).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t1978, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t1989, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1989, 0.36).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t1989, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1995, 0.4).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t1995, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t2003, observed).
narrative_ontology:measurement(formal_equality_reading_tr_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2023, 0.45).
narrative_ontology:measurement_basis(formal_equality_reading_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(formal_equality_reading_be_t1944, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1944, 0.08).
narrative_ontology:measurement_basis(formal_equality_reading_be_t1944, observed).
narrative_ontology:measurement(formal_equality_reading_be_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1954, 0.1).
narrative_ontology:measurement_basis(formal_equality_reading_be_t1954, observed).
narrative_ontology:measurement(formal_equality_reading_be_t1964, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1964, 0.13).
narrative_ontology:measurement_basis(formal_equality_reading_be_t1964, observed).
narrative_ontology:measurement(formal_equality_reading_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.21).
narrative_ontology:measurement_basis(formal_equality_reading_be_t1978, observed).
narrative_ontology:measurement(formal_equality_reading_be_t1989, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1989, 0.27).
narrative_ontology:measurement_basis(formal_equality_reading_be_t1989, observed).
narrative_ontology:measurement(formal_equality_reading_be_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement_basis(formal_equality_reading_be_t1995, observed).
narrative_ontology:measurement(formal_equality_reading_be_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2003, 0.31).
narrative_ontology:measurement_basis(formal_equality_reading_be_t2003, observed).
narrative_ontology:measurement(formal_equality_reading_be_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2023, 0.34).
narrative_ontology:measurement_basis(formal_equality_reading_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(formal_equality_reading_su_t1944, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1944, 0.35).
narrative_ontology:measurement_basis(formal_equality_reading_su_t1944, observed).
narrative_ontology:measurement(formal_equality_reading_su_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1954, 0.48).
narrative_ontology:measurement_basis(formal_equality_reading_su_t1954, observed).
narrative_ontology:measurement(formal_equality_reading_su_t1964, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1964, 0.56).
narrative_ontology:measurement_basis(formal_equality_reading_su_t1964, observed).
narrative_ontology:measurement(formal_equality_reading_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.54).
narrative_ontology:measurement_basis(formal_equality_reading_su_t1978, observed).
narrative_ontology:measurement(formal_equality_reading_su_t1989, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1989, 0.63).
narrative_ontology:measurement_basis(formal_equality_reading_su_t1989, observed).
narrative_ontology:measurement(formal_equality_reading_su_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(formal_equality_reading_su_t1995, observed).
narrative_ontology:measurement(formal_equality_reading_su_t2003, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2003, 0.61).
narrative_ontology:measurement_basis(formal_equality_reading_su_t2003, observed).
narrative_ontology:measurement(formal_equality_reading_su_t2023, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2023, 0.72).
narrative_ontology:measurement_basis(formal_equality_reading_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% 'What Equal Protection requires' is a colloquial label covering two structurally distinct constraints: this file's formal-equality rule (symmetry prohibition; low reading-indexed ε; victims are state corrective actors and intended remedial beneficiaries) and the anti-caste reading's dismantling mandate (separate file; high ε over the same referent; the object of state duty is hierarchy itself). The label confusion, not the constitutional text, created the appearance of observable-dependent ε; per the ε-invariance principle the family is split and linked here. The influence edge runs both ways historically: each reading cites the founding record against the other, and each ruling for one reading raises the stakes of the other's next litigation campaign.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
