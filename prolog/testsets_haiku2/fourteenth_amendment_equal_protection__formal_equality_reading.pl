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
 *   human_readable: Formal Equality Doctrine: Colorblindness Standard
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Equal Protection Clause is contested. This
 *   story instantiates the formal_equality_reading: Equal Protection
 *   prohibits explicit state racial and status classifications absent
 *   compelling justification, and the state's proper role is to enforce
 *   colorblindness — to treat all citizens without regard to race, achieving
 *   equality by refusing to acknowledge group differences. Under this
 *   reading, the state is prohibited from race-conscious remedies,
 *   affirmative action, or active dismantling of hierarchical structures,
 *   because doing so would constitute the impermissible classification the
 *   Amendment forbids. The sibling reading (anti_caste_reading) claims the
 *   Amendment mandates active correction of racial and status hierarchy,
 *   treating colorblindness as a cover for preserving caste structure. This
 *   story generates the formal equality reading as a clean, ε-invariant
 *   constraint with its own beneficiary/victim structure, independent of the
 *   sibling reading's account.
 *
 * KEY AGENTS:
 *   - State enforcement apparatus: interprets and enforces colorblindness through strict scrutiny review
 *   - Dominant racial group: benefits from unmarked default status, protection of accumulated advantages
 *   - Beneficiaries of historical hierarchy: benefit from institutions structured by prior explicit preference, now invisible under colorblindness
 *   - Subordinated racial groups: pay through inability to access race-conscious remedies, constrained competition under formal equality
 *   - Historical injury bearers: pay through intergenerational costs of prior classifications, treated as background not subject to correction
 *   - Courts and judges: agenda-setters enforcing the doctrine, treating colorblindness as the constitutional mandate
 *   - Civil rights advocates: excluded from the enforcement seat, their arguments for substantive equality reframed as impermissible race-consciousness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.62).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.71).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Formal Equality Doctrine: Colorblindness Standard").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '0668120d-f1ea-4641-96f4-9f343bb683de').
narrative_ontology:cs_kernel_codification('0668120d-f1ea-4641-96f4-9f343bb683de', fixed_text).
narrative_ontology:cs_authority_grounding('0668120d-f1ea-4641-96f4-9f343bb683de', lineage).
narrative_ontology:cs_interpretation_layer_present('0668120d-f1ea-4641-96f4-9f343bb683de').
narrative_ontology:cs_reading_relation('0668120d-f1ea-4641-96f4-9f343bb683de', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('0668120d-f1ea-4641-96f4-9f343bb683de', foundational, state_colorblindness_constitutive).
narrative_ontology:cs_axiom_status(state_colorblindness_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('0668120d-f1ea-4641-96f4-9f343bb683de', state_colorblindness_constitutive, deontological).
narrative_ontology:cs_axiom('0668120d-f1ea-4641-96f4-9f343bb683de', foundational, explicit_classification_impermissible_absent_compelling_state_interest).
narrative_ontology:cs_axiom_status(explicit_classification_impermissible_absent_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('0668120d-f1ea-4641-96f4-9f343bb683de', explicit_classification_impermissible_absent_compelling_state_interest, deontological).
narrative_ontology:cs_axiom('0668120d-f1ea-4641-96f4-9f343bb683de', secondary, structural_inequality_as_pre_constitutional_background).
narrative_ontology:cs_axiom_status(structural_inequality_as_pre_constitutional_background, holdable).
narrative_ontology:cs_axiom_grounding('0668120d-f1ea-4641-96f4-9f343bb683de', structural_inequality_as_pre_constitutional_background, conventional).
narrative_ontology:cs_reference_frame('0668120d-f1ea-4641-96f4-9f343bb683de', reconstruction_era_explicit_classification_prohibition).
narrative_ontology:cs_drift_state('0668120d-f1ea-4641-96f4-9f343bb683de', contemporary_structural_inequality_persistence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0668120d-f1ea-4641-96f4-9f343bb683de', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, dominant_racial_group).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_historical_hierarchy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, subordinated_racial_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, historical_injury_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, legislative_bodies).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, legislative_bodies).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_colorblindness_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, neutral_state_abstraction_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implements and enforces the colorblindness doctrine through judicial review of legislation and administrative action. Rejects race-conscious remedies and affirmative action programs under strict scrutiny, treating them as suspect classifications equivalent to discriminatory laws. Maintains the formal framework by policing state classification language, not structural outcomes.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the colorblindness doctrine by being treated as the unmarked default category against which race-consciousness is measured; maintains structural advantages (wealth, institutional access, social networks) accumulated through centuries of explicit legal preference without triggering equal protection scrutiny under the formal framework, since the doctrine treats pre-constitutional inequalities as background conditions, not state-created classifications.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, dominant_racial_group, beneficiary,
    powerful, generational, arbitrage, national).

% Benefit from institutions and resource flows structured by prior eras of explicit racial preference; the formal equality doctrine does not require dismantling these structures, only preventing NEW explicit classifications, so accumulated advantages in real estate, educational access, and capital formation persist without triggering corrective state action.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_historical_hierarchy, beneficiary,
    powerful, generational, mobile, national).

% Carry the costs of the formal equality doctrine by being treated as legally equal to the dominant group despite structural inequality; cannot access race-conscious remedies that would address group-level disadvantage accumulated through explicit state classification in prior eras; must compete as individuals against competitors with inherited structural advantages while those advantages remain constitutionally invisible.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, subordinated_racial_groups, payer,
    organized, generational, constrained, national).

% Bear the intergenerational costs of slavery, segregation, and systematic exclusion; the colorblindness doctrine treats these injuries as historical facts external to the constitutional order, not as ongoing state-maintained conditions; they are barred from claiming race-conscious remedies because to do so would require the state to acknowledge ongoing classification, which the doctrine treats as impermissible.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, historical_injury_bearers, payer,
    moderate, generational, identity_locked, national).

% Would argue the formal equality doctrine masks structural inequality and prevents effective remediation; argue for dismantling hierarchical structures as a constitutional mandate, not merely preventing new classifications; are structurally excluded from the enforcement seat because the doctrine's framework treats their substantive equality arguments as race-consciousness (and therefore suspect), not as equal protection claims.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, civil_rights_advocates, excluded,
    organized, generational, constrained, national).

% Interpret and apply the formal equality doctrine through strict scrutiny review of state classifications; have the authority to invalidate race-conscious remedies and affirmative action on colorblindness grounds; maintain the doctrine's authority by treating it as the constitutionally correct reading of the Equal Protection Clause, resisting alternative readings that would demand active dismantling of hierarchy.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, courts_and_judges, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, courts_and_judges, observer).

% Are constrained by the formal equality doctrine from enacting race-conscious remedies, but benefit insofar as the doctrine prevents political mobilization around group-based injury claims; can maintain existing structural inequalities without triggering strict scrutiny, since the doctrine treats these as background conditions rather than state classifications.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, legislative_bodies, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, legislative_bodies, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, beneficiaries_of_historical_hierarchy).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform, classification-blind decision procedure for state action: all agents are evaluated on ostensibly neutral criteria, not on group status. This solves a coordination problem about what it means for the state to treat citizens equally — the answer is: by refusing to see and act on group-based differences.
% TRANSFER_FUNCTION: Transfers the burden of addressing group-level inequality from the state (which the doctrine constrains from remedial action) to individuals competing under formally equal rules but materially unequal starting conditions; transfers the authority to define equality from legislative bodies (who might recognize group injury) to courts interpreting colorblindness as the constitutional mandate.
% ABSENT_VOICES: Civil rights advocates and representatives of historical injury bearers who would argue that formal equality masks structural inequality and that the constitutional mandate is to actively dismantle racial hierarchy, not merely to stop new classifications. These voices are excluded because to make their claims requires treating ongoing inequality as state-maintained (requiring state correction), which the formal equality doctrine treats as impermissible race-consciousness rather than equal protection argument.
% DISAPPEARANCE_RATIONALE: If the formal equality doctrine vanished and were replaced by one that permitted (or mandated) race-conscious remedies, the stakes of political contestation would shift immediately. Affirmative action, reparations programs, anti-caste initiatives, and wealth redistribution targeted to injury bearers would become constitutionally permissible. The arrangement of structural advantages accumulated under prior explicit classifications would become subject to state corrective action. The doctrine's disappearance would reorganize the terrain of constitutional constraint on state remedial power.
% FOUNDING_PROBLEM: The founding problem was to eliminate explicit state racial classification and discrimination. The Reconstruction Amendments intended to wipe away slavery's legal apparatus and guarantee equal citizenship. In the formal equality reading, this problem is interpreted narrowly as: prevent state actors from explicitly naming racial categories in law.
% FOUNDING_PROBLEM_CORROBORATION: The formal equality advocates (courts, originalist scholars) attest that the founding problem remains live: race-conscious state action is impermissible because it violates the colorblindness principle. Critics (civil rights scholars, historians of Reconstruction, contemporary advocates for subordinated groups) attest that the founding problem has been reframed: the original intent was to dismantle racial caste and hierarchy, not merely to prevent explicit naming. Legislative testimony and scholarly historical analysis from outside the doctrine's beneficiaries support the broader interpretation of the founding mandate.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.62) because the doctrine prevents race-conscious remedies while permitting the state to leave in place structures built by prior explicit classifications. It is not higher (0.75+) because the doctrine is not pure extraction: genuine equal-treatment principles are operative, and the constraint does prevent some forms of explicit discrimination going forward. However, the constraint's primary function has shifted from addressing racial classification to maintaining structural advantage while appearing neutral. Suppression is high (0.71) because enforcing colorblindness requires suppressing the argument that structural inequality is state-maintained and requires correction — that argument is legally foreclosed as 'race-consciousness.' Theater is moderate (0.28) because significant doctrinal energy goes into reviewing classifications (genuine function), but an increasing share maintains a particular reading of what equality means rather than addressing actual disparate outcomes. The measurement series tracks the doctrine's drift from Reconstruction (when it could plausibly be read as preventing explicit discrimination) through Brown (when its limitations become visible) to contemporary law (where it primarily defends against remediation). Extracted from formal_equality_reading's perspective: the doctrine's extractiveness grows as structural inequality persists and the doctrine prevents its correction.
 *
 * PERSPECTIVAL GAP:
 *   The formal equality reading authorizes courts to see the doctrine as neutral, legitimate equal protection. From the seats of subordinated groups, it authorizes courts to refuse to see structure: to treat as background what prior law made explicit, to prevent remediation by calling it impermissible classification. This divergence is precisely what the reading accomplishes — a unified doctrine producing opposite directionalities depending on seat. The reading does not hide this: it argues colorblindness is the correct principle. But the structural consequence is asymmetry: beneficiaries get to keep advantages without naming them, while targets are prevented from addressing them by naming them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations feed directionality derivation directly: beneficiaries (dominant racial group, beneficiaries of historical hierarchy) derive d toward 0.0 (coordinate, do not bear extraction costs); victims (subordinated racial groups, historical injury bearers) derive d toward 1.0 (targets, cannot access remedies). The exit options modulate: beneficiaries have mobile or arbitrage-grade exit (can relocate, change status, opt into different institutions), so d stays low; subordinated groups have identity-locked or constrained exit (cannot exit racial status, face systemic barriers), so d stays high. Power atoms matter: institutional power (courts) set the doctrine; organized power (subordinated groups) can mobilize politically but are legally foreclosed from remedial arguments; powerless status locks into the identity-locked exit that prevents exit from the constraint's scope. The directionality is robust across these variables because the doctrine's structure produces the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's classification as tangled_rope depends on sustaining the claim that preventing race-conscious remedies is genuine equal protection, not extraction masked as coordination. The mandatrophy resolution is: if the founding mandate was to dismantle racial caste (anti-caste reading), then colorblindness doctrine violates the mandate by preventing its implementation. If the mandate was to prevent explicit classification (formal equality reading, the one this story instantiates), then the doctrine fulfills the mandate. The ambiguity is not in the metrics but in the reading itself — which is why it belongs in an omega variable, not resolved here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_vs_alternative,
    'Is formal equality (colorblindness) the constitutionally correct reading of the Fourteenth Amendment, or does the Amendment mandate active dismantling of racial hierarchy as the anti-caste reading claims?',
    'Historical analysis of Reconstruction-era legislative intent, comparative constitutional jurisprudence (other democracies'' remedial equality frameworks), and empirical assessment of whether colorblindness achieves or prevents equal protection outcomes.',
    'If the anti-caste reading is correct, the entire doctrinal structure reverses: race-conscious remedies become constitutionally required rather than prohibited; structural inequality becomes a state-maintained classification requiring correction; the victim set expands to include all bearers of caste injury; extractiveness drops to near zero for justice-oriented remedies and rises sharply for colorblind maintenance of hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_vs_alternative, conceptual, 'Whether formal equality or anti-caste constitutes the true equal protection mandate.').

omega_variable(
    extraction_mechanism_obscured_by_neutrality,
    'Does the colorblindness doctrine extract by virtue of treating structural inequality as background (non-classifiable) rather than as state-maintained discrimination requiring remediation?',
    'Structural analysis: trace the lineage of current inequality to prior explicit state classifications (slavery, Jim Crow, redlining, segregation); measure the correlation between historical explicit classification and contemporary disadvantage; assess whether the doctrine''s refusal to acknowledge this lineage permits the state to benefit from prior harm without correcting it.',
    'If the doctrine extracts via obscuring state responsibility for structural inequality, suppression rises (the doctrine prevents naming the injury), theater rises (the doctrine''s neutrality language masks continued advantage), and extractiveness persists through doctrinal maintenance of the status quo. The constraint moves toward snare classification as the extraction mechanism becomes clear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_mechanism_obscured_by_neutrality, empirical, 'Whether neutrality doctrine operates as a mechanism to preserve extraction by treating hierarchy as pre-constitutional background.').

omega_variable(
    identity_lock_in_judicial_system,
    'Are judges and constitutional scholars identity-locked into the formal equality reading through professional socialization, career incentives (promotion, peer status), and the cumulative nature of precedent that makes alternative readings appear heterodox?',
    'Examine the career trajectories of judges who adopt anti-caste readings versus those who maintain colorblindness; measure the citation differential (how much more frequently is colorblindness doctrine cited and treated as settled); assess the professional costs of advancing alternative readings within law schools and judicial hierarchies.',
    'If identity-lock is operative, the persistence of formal equality doctrine is sustained not by genuine legal necessity but by institutional path-dependence. The doctrine''s extraction would then be not merely structural but also performatively maintained through gatekeeping and norm enforcement. Breaking the identity-lock would require not just better arguments but institutional restructuring (diversification of the judiciary, revaluation of alternative readings as scholarly-respectable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_judicial_system, empirical, 'Whether formal equality doctrine is maintained by identity fusion in the judiciary and legal academy.').

omega_variable(
    committer_frame_kernel_reading_ambiguity,
    'This constraint is the formal_equality_reading of the fourteenth_amendment_equal_protection kernel. Is the kernel (the text of the Fourteenth Amendment, ''nor shall any State deny to any person within its jurisdiction the equal protection of the laws'') fixed and interpretable, or is the reading''s success in becoming canonical law evidence that the reading constitutes the kernel itself?',
    'Philosophical analysis: does the kernel exist independent of readings, or does the doctrine''s institutional dominance make it the de facto kernel? Historical/doctrinal analysis: how would the doctrine''s collapse or replacement change the account of what the Amendment ''really says''?',
    'If readings constitute kernels rather than interpreting pre-existing ones, the formal equality reading is not one interpretation among alternatives but the currently-dominant framing of the constraint itself. Its extraction would then be invisible to frameworks that treat it as natural law rather than as a contested reading. Recognizing the reading-constitutive nature of doctrine opens space for counter-readings to emerge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading_ambiguity, conceptual, 'Whether the kernel is prior to readings or constituted by them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement_basis(four_tr_t1868, observed).
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1954, 0.12).
narrative_ontology:measurement_basis(four_tr_t1954, observed).
narrative_ontology:measurement(four_tr_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement_basis(four_tr_t1978, observed).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement_basis(four_tr_t1995, observed).
narrative_ontology:measurement(four_tr_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2013, 0.27).
narrative_ontology:measurement_basis(four_tr_t2013, observed).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(four_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(four_be_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement_basis(four_be_t1868, observed).
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1954, 0.35).
narrative_ontology:measurement_basis(four_be_t1954, observed).
narrative_ontology:measurement(four_be_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1978, 0.48).
narrative_ontology:measurement_basis(four_be_t1978, observed).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement_basis(four_be_t1995, observed).
narrative_ontology:measurement(four_be_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2013, 0.61).
narrative_ontology:measurement_basis(four_be_t2013, observed).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(four_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1868, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1868, 0.42).
narrative_ontology:measurement_basis(four_su_t1868, observed).
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1954, 0.55).
narrative_ontology:measurement_basis(four_su_t1954, observed).
narrative_ontology:measurement(four_su_t1978, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1978, 0.63).
narrative_ontology:measurement_basis(four_su_t1978, observed).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(four_su_t1995, observed).
narrative_ontology:measurement(four_su_t2013, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement_basis(four_su_t2013, observed).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(four_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% The fourteenth_amendment_equal_protection kernel decomposes into two structurally distinct constraints: formal_equality_reading and anti_caste_reading. Both share the same text (the Fourteenth Amendment), but differ in core axioms (neutrality vs. hierarchy-elimination), victim sets, and the extractiveness profile. Formal equality instantiates a reading where race-consciousness is prohibited; anti-caste instantiates a reading where race-consciousness for remediation is required. These readings coexist in contemporary law as competing judicial and scholarly positions. The formal equality reading influences the anti-caste reading structurally: by establishing colorblindness as the dominant doctrine, it creates the barrier anti-caste readings must overcome — institutional precedent, citation dominance, and professional gatekeeping that treat anti-caste arguments as heterodox. See commentary.kernel_context for the full reading relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
