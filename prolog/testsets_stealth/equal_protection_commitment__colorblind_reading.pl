% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Equal Protection, Color-Blind Reading: State Racial Classification as the Harm
 *   domain: constitutional law/political philosophy/social policy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the modern state practice of
 *   racial classification: race-conscious admissions at selective
 *   universities, race-conscious public contracting (set-asides), and
 *   race-conscious electoral structuring. This story authors that arrangement
 *   from the colorblind_reading of the equal_protection_commitment kernel —
 *   the reading articulated in Harlan's Plessy dissent ('Our Constitution is
 *   color-blind'), carried through the Bakke dissents, Croson, and Adarand,
 *   and adopted as governing doctrine in SFFA v. Harvard (2023). From this
 *   seat the classification itself is the harm, so the arrangement computes
 *   with moderate-high epsilon (0.47) and a snare claim: the coordination
 *   story (remediation, then diversity) is, on this reading, the cover under
 *   which the state sorts citizens by race and distributes positions
 *   accordingly. The epsilon referent is the classification arrangement, NOT
 *   the reading's endorsed race-neutral alternative (which would compute
 *   near-zero from this seat) — the kernel-reading referent rule is observed.
 *   Claim and metrics are independent authored facts: the snare claim is this
 *   reading's structural verdict; the metrics describe the arrangement's
 *   operation as the record shows it. KEY AGENTS (by structural
 *   relationship): - race_conscious_program_administrators:
 *   agenda-setter/perpetrator seat (institutional/identity_locked) —
 *   administers the classification and captures its institutional gains -
 *   race_disfavored_applicants: primary victim seat (powerless/constrained) —
 *   Asian-American and white applicants denied admission by the
 *   classification - preferred_admission_recipients: beneficiary seat
 *   (powerless/constrained) — receives allocated slots -
 *   mbe_set_aside_contractors: secondary beneficiary seat (moderate/mobile) —
 *   receives race-conscious contract set-asides -
 *   race_neutral_alternative_proponents: excluded seat (moderate/constrained)
 *   — proposals dismissed inside the institutions, heard only in courts and
 *   at the ballot - federal_courts: analytical observer seat
 *   (institutional/analytical) — adjudicates the kernel; its seat flipped to
 *   this reading in 2023
 *
 * KEY AGENTS:
 *   - race_conscious_program_administrators: agenda-setter/perpetrator (institutional/identity_locked) — sets and administers the classification, defends it in litigation, captures the arrangement's institutional gains
 *   - race_disfavored_applicants: primary victim (powerless/constrained) — Asian-American and white applicants denied admission or contract awards by the classification
 *   - preferred_admission_recipients: nominal beneficiary (powerless/constrained) — receives allocated admission slots; did not set or administer the arrangement
 *   - mbe_set_aside_contractors: secondary beneficiary (moderate/mobile) — receives set-aside contract channels; can operate outside them
 *   - race_neutral_alternative_proponents: excluded (moderate/constrained) — race-neutral proposals entertained only externally
 *   - federal_courts: analytical observer (institutional/analytical) — adjudicates the kernel the arrangement stands or falls on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.47).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.66).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, snare).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection, Color-Blind Reading: State Racial Classification as the Harm").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional law/political philosophy/social policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, 'c7b5d478-604e-453e-a654-e81f996453c2').
narrative_ontology:cs_kernel_codification('c7b5d478-604e-453e-a654-e81f996453c2', fixed_text).
narrative_ontology:cs_authority_grounding('c7b5d478-604e-453e-a654-e81f996453c2', lineage).
narrative_ontology:cs_interpretation_layer_present('c7b5d478-604e-453e-a654-e81f996453c2').
narrative_ontology:cs_reading_relation('c7b5d478-604e-453e-a654-e81f996453c2', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('c7b5d478-604e-453e-a654-e81f996453c2', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_axiom('c7b5d478-604e-453e-a654-e81f996453c2', foundational, state_racial_classification_categorically_prohibited).
narrative_ontology:cs_axiom_status(state_racial_classification_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('c7b5d478-604e-453e-a654-e81f996453c2', state_racial_classification_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('c7b5d478-604e-453e-a654-e81f996453c2', secondary, equal_protection_guarantee_is_individual_not_group).
narrative_ontology:cs_axiom_status(equal_protection_guarantee_is_individual_not_group, holdable).
narrative_ontology:cs_axiom_grounding('c7b5d478-604e-453e-a654-e81f996453c2', equal_protection_guarantee_is_individual_not_group, deontological).
narrative_ontology:cs_reference_frame('c7b5d478-604e-453e-a654-e81f996453c2', colorblind_constitutional_command).
narrative_ontology:cs_drift_state('c7b5d478-604e-453e-a654-e81f996453c2', post_sffa_2023, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c7b5d478-604e-453e-a654-e81f996453c2', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, preferred_admission_recipients).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, mbe_set_aside_contractors).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, race_conscious_program_administrators).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, race_disfavored_applicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University admissions offices, federal and state contracting agencies, and redistricting authorities set racial categories, weight applications and bids by them, and defend the practice in litigation. The arrangement's institutional gains accrue here: enrollment-management control, compliance standing with funders and accreditors, reputational capital, and a bureaucratic apparatus whose staffing depends on the practice continuing. Institutional self-conception fused with the practice across two generations of mission statements, hiring, and internal research offices; exit would mean dismantling that identity rather than relocating it.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_conscious_program_administrators, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__colorblind_reading, race_conscious_program_administrators, beneficiary).

% Asian-American and white applicants to selective universities and bidders outside set-aside eligibility bear the arrangement's direct burden; the SFFA trial record showed race was decisive for large fractions of admissions outcomes at elite institutions. Their exit is partial — institutions in ban states and less selective tiers apply race-neutral standards — but the classification followed them across most of the selective tier. Individually they had no voice in the process; their interests entered the conversation only when aggregated by an outside litigant.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_disfavored_applicants, payer,
    powerless, biographical, constrained, national).

% Applicants admitted with a preference receive slots they would not have received under race-neutral standards. The benefit is real and biographically consequential. They did not set the arrangement and do not administer it; their position is defined by receiving its allocation, and most reject the claim that the classification harms them.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, preferred_admission_recipients, beneficiary,
    powerless, biographical, constrained, national).

% Minority-owned businesses receiving set-aside contracts under federal, state, and local programs gain guaranteed bid channels closed to ineligible competitors. Unlike applicants, they can operate outside set-aside programs in open markets, so their position in the arrangement is advantageous but not captive.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, mbe_set_aside_contractors, beneficiary,
    moderate, biographical, mobile, national).

% State legislatures in ban states, policy organizations, and litigants proposing race-neutral standards — top-percent plans, class-based preferences, blind review. Inside the institutions running the arrangement their proposals were dismissed without serious consideration, and internal studies of race-neutral alternatives were suppressed or shelved at several defendant institutions; their proposals were heard only in courts and at the ballot.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, race_neutral_alternative_proponents, excluded,
    moderate, biographical, constrained, national).

% The Supreme Court and lower federal courts adjudicate the constitutional question the arrangement stands or falls on. From Bakke (1978) through Fisher II (2016) the Court's majority sustained the arrangement under the diversity rationale while this reading accumulated in dissents; in SFFA (2023) the Court adopted this reading and struck the arrangement's admissions core. The seat decides; it neither receives slots nor bears classification.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, race_conscious_program_administrators).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement's claimed coordination problem: allocating scarce positions — selective-university seats, public contracts, legislative representation — so that institutional composition tracks the polity's, and so that access routes closed by historical state-enforced exclusion are reopened. Stated without evaluation: it coordinates distribution of positions and contracts by racial category.
% TRANSFER_FUNCTION: Moves admission slots, contract awards, and representational positions from applicants and bidders disfavored by the classification (in the admissions record, Asian-American and white applicants) to those favored by it, and moves discretion, compliance capital, and reputational benefit to the administering institutions.
% ABSENT_VOICES: The rejected applicants had no seat where the classification was designed: the rationale was set by administrators and faculty, and race-neutral alternative proposals were entertained only in courts and ballot initiatives, never inside the institutions running the programs. Their interests entered the conversation only when an outside litigant aggregated them.
% DISAPPEARANCE_RATIONALE: If state racial classification vanished overnight, selective admissions outcomes, public-contract awards, and institutional-composition practices would reorganize around race-neutral criteria — as they demonstrably began to after SFFA (2023), and as they did in ban states after Proposition 209 (1996). The arrangement's administrators and beneficiaries are organized around it; its removal rearranges them.
% FOUNDING_PROBLEM: The arrangement was built to dismantle the legacy of state-enforced racial caste: de jure exclusion of Black Americans from universities, professions, and public contracting, and the accumulated positional disadvantage that exclusion produced.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real is corroborated outside the benefiting parties by historical scholarship on de jure segregation and by the Reconstruction Congress's own records. That the problem has since receded is corroborated outside the benefiting parties by judicial findings — Croson (1989) found no evidence of past discrimination by the set-aside's own jurisdiction, Adarand (1995) placed federal set-asides under strict scrutiny, and the SFFA majority found the diversity rationale rested on unsupported empirical claims. No source outside the benefiting parties attests that the original caste-enforcement problem remains live as a present legal barrier; the sibling readings dispute this recession, and that dispute is recorded in the kernel omega rather than resolved here.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.47: from this reading the harm is the classification itself, and the record shows the burden concentrating on applicants with no historical claim to the remedy — the SFFA trial record showed race was decisive for large fractions of admissions outcomes at elite institutions. Suppression 0.66: the arrangement persisted through active force — deferential doctrine, institutional litigation defense, defeat of repeal attempts, funding and accreditation leverage — not through participant preference; suppression is authored as a raw structural property and is not scaled by power or scope. Theater_ratio 0.55: the holistic-review apparatus was substantially performative — defendant institutions' own internal research found no measurable educational benefit from diversity while the process operated as racial balancing — though slots genuinely moved, so the arrangement is not mostly theater. Accessibility_collapse 0.45: alternatives did not fully collapse; ban states operated race-neutral systems from 1996 onward, and top-percent and class-based substitutes existed. Resistance 0.72: forty-five years of continuous litigation plus eight-plus state bans is among the highest-resistance profiles a domestic legal arrangement has shown. All three tracked metrics run on one shared six-point grid (1978, 1989, 1995, 2003, 2016, 2023). Extraction crept up as the remedial justification thinned and the burden shifted onto applicants with no connection to the original exclusion; theater rose as holistic review became operative cover; suppression_requirement rose as the enforcement machinery matured against proliferating alternatives — strict scrutiny arrived with Croson and Adarand, ban-state defense and compliance bureaucracies followed, and the SFFA defense was the machinery's peak. The trajectory is monotonic, not cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the administrators' seat the arrangement is their institutional mission — identity-locked, near the beneficiary end of directionality, experienced as coordination they built and staffed. From the disfavored-applicants' seat the same structure is enforced extraction with constrained exit — near the full-target end. Preferred recipients sit near symmetric: real, biographically consequential slots gained, against a categorical-harm claim they themselves reject. The sharpest divergence is temporal and sits in one seat: the federal courts sustained the arrangement from Bakke through Fisher II and then flipped to this reading in SFFA — the same text, the same arrangement, opposite computed classifications across one seat's history. The coalition check matters here: applicants were individually powerless; aggregation through an outside litigant (Students for Fair Admissions) manufactured the coalition power that flipped the judicial seat and ended the arrangement's admissions core.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: preferred_admission_recipients and mbe_set_aside_contractors derive near-beneficiary directionality; the administrators, as agenda-setters who also collect the arrangement's institutional gains, sit at the beneficiary end despite running it — for them receipt and administration coincide. Victim declarations drive high d: race_disfavored_applicants, with partial but not absent exit (ban-state and less selective tiers existed), derive near-full-target directionality short of trapped. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce accurate d for every seat. Effective extraction is the engine's computation from these declarations, power, and scope — national scope modestly amplifies verification difficulty; the authored epsilon 0.47 is the base the engine scales. Suppression is not scaled by anything: it is the raw structural fact that the arrangement needed courts, funding leverage, and institutional enforcement to hold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — dismantling state-enforced racial caste — has receded: the de jure system is gone, and the arrangement's justification chain thinned from remediation to diversity as the original problem aged. The R5 mismatch (status=dead with verdict=world_rearranges) is exactly the capture/zombie pattern this reading alleges: an arrangement whose founding problem died but which rearranged institutional life around itself and now serves the administrators' identity, staffing, and process-control interests. The snare claim prevents mislabeling in both directions: it keeps the arrangement's coordination cover (diversity, remediation) from being mistaken for genuine coordination, and it keeps this reading's categorical premise from being mistaken for an empirical finding — the intrinsic-harm claim is a normative axiom recorded in cs_structure, and the omegas hold open the empirical questions (who actually captured the gains, whether suppression was structural or internalized) that would discipline or overturn the classification if the reading's frame broke.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the colorblind_reading of the equal_protection_commitment kernel; how would classification of the same standing arrangement shift under the sibling readings (remedial_reading, diversity_reading)?',
    'Author the sibling stories as separate constraints over the same referent; compare victim sets, perpetrator sets, and epsilon across the family.',
    'Under remedial_reading, caste-perpetuating arrangements enter the victim set and race-conscious remedies compute as coordination with lower epsilon; under diversity_reading, epsilon falls where the classification operates as one factor among many. The victim set authored here (race-disfavored applicants) is reading-indexed, not topic-intrinsic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexed classification of a contested constitutional kernel; sibling readings are separate constraints.').

omega_variable(
    harm_intrinsic_vs_effect_contingent,
    'Is the harm of state racial classification intrinsic to the classification itself (this reading''s categorical premise) or contingent on its purpose and effects (the premise the sibling readings share)?',
    'No empirical data resolves this; it is the located disagreement between the readings. Resolution would be a doctrinal or philosophical settlement of the kernel, not a measurement.',
    'If harm is effect-contingent, epsilon drops where classifications produce benign effects and the snare claim weakens toward a hybrid coordination/extraction reading; if intrinsic, epsilon is floored by the classification''s existence regardless of effect, as authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_intrinsic_vs_effect_contingent, conceptual, 'The structural axis on which the readings of the kernel actually diverge.').

omega_variable(
    intended_vs_actual_beneficiaries,
    'Did the arrangement''s allocated slots and contracts accrue to its intended beneficiaries (disadvantaged members of preferred groups) or disproportionately to advantaged members of the same groups?',
    'Admissions and contracting microdata linking preference receipt to socioeconomic status; the SFFA trial record and post-Prop-209 California studies are partial existing evidence.',
    'If benefits were captured by advantaged subgroup members, the arrangement extracted without delivering its remedy, strengthening the snare classification and the capture reading of the receipt surface; if well-targeted, part of the measured extraction is the price of a functioning remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intended_vs_actual_beneficiaries, empirical, 'Whether the arrangement''s gains reached the disadvantaged or were captured within preferred groups.').

omega_variable(
    post_sffa_circumvention_persistence,
    'After SFFA removed the legal enforcement structure, does the arrangement persist through race-adjacent proxies and internalized institutional commitment, or is it genuinely decaying?',
    'Post-2023 admissions outcome data by race; tracking of essay, recommendation, and recruitment channels; comparison of composition trends before and after the decision across ban and non-ban states.',
    'If persistence is internalized, the arrangement''s suppression outlives its structural enforcement and the constraint remains live in degraded, largely performative form; if decaying, the snare classification is terminal and the interval end marks actual death rather than drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_sffa_circumvention_persistence, empirical, 'Structural versus internalized suppression: the post-enforcement trajectory discriminates between the two mechanisms.').

omega_variable(
    authority_grounding_framing,
    'Is the kernel''s authority grounded in the fixed text''s original command (this story''s framing: fixed_text under lineage) or in the living interpretive tradition''s governing doctrine (an alternative framing under which this reading''s reference frame was off-frame until 2023)?',
    'Framing choice, not data. The reference_frame and drift_state authored here follow the original-text framing; under the living-tradition framing the same history reads as a late reconstruction of a long-rejected frame rather than a restoration.',
    'Under the alternative framing, the t0-to-t1 gap is larger (a century of contrary doctrine between Harlan''s dissent and SFFA), the 2023 position is a reconstruction rather than a recovery, and computed drift and foreclosure dynamics against the sibling readings change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: original-text versus living-tradition authority for the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colorblind_reading_tr_t1978, equal_protection_commitment__colorblind_reading, theater_ratio, 1978, 0.24).
narrative_ontology:measurement(colorblind_reading_tr_t1989, equal_protection_commitment__colorblind_reading, theater_ratio, 1989, 0.3).
narrative_ontology:measurement(colorblind_reading_tr_t1995, equal_protection_commitment__colorblind_reading, theater_ratio, 1995, 0.36).
narrative_ontology:measurement(colorblind_reading_tr_t2003, equal_protection_commitment__colorblind_reading, theater_ratio, 2003, 0.43).
narrative_ontology:measurement(colorblind_reading_tr_t2016, equal_protection_commitment__colorblind_reading, theater_ratio, 2016, 0.5).
narrative_ontology:measurement(colorblind_reading_tr_t2023, equal_protection_commitment__colorblind_reading, theater_ratio, 2023, 0.55).

% Extraction over time
narrative_ontology:measurement(colorblind_reading_be_t1978, equal_protection_commitment__colorblind_reading, base_extractiveness, 1978, 0.36).
narrative_ontology:measurement(colorblind_reading_be_t1989, equal_protection_commitment__colorblind_reading, base_extractiveness, 1989, 0.4).
narrative_ontology:measurement(colorblind_reading_be_t1995, equal_protection_commitment__colorblind_reading, base_extractiveness, 1995, 0.43).
narrative_ontology:measurement(colorblind_reading_be_t2003, equal_protection_commitment__colorblind_reading, base_extractiveness, 2003, 0.45).
narrative_ontology:measurement(colorblind_reading_be_t2016, equal_protection_commitment__colorblind_reading, base_extractiveness, 2016, 0.46).
narrative_ontology:measurement(colorblind_reading_be_t2023, equal_protection_commitment__colorblind_reading, base_extractiveness, 2023, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(colorblind_reading_su_t1978, equal_protection_commitment__colorblind_reading, suppression_requirement, 1978, 0.4).
narrative_ontology:measurement(colorblind_reading_su_t1989, equal_protection_commitment__colorblind_reading, suppression_requirement, 1989, 0.47).
narrative_ontology:measurement(colorblind_reading_su_t1995, equal_protection_commitment__colorblind_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(colorblind_reading_su_t2003, equal_protection_commitment__colorblind_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(colorblind_reading_su_t2016, equal_protection_commitment__colorblind_reading, suppression_requirement, 2016, 0.62).
narrative_ontology:measurement(colorblind_reading_su_t2023, equal_protection_commitment__colorblind_reading, suppression_requirement, 2023, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'equal protection' covers one contested kernel with at least three structurally distinct readings. Per the epsilon-invariance principle the family is decomposed: colorblind_reading (this file — the classification itself is the harm; victim set is race-disfavored applicants; epsilon 0.47), remedial_reading (caste-perpetuation is the harm; race-conscious remedies are coordination; victim set is those the subordination maintains), and diversity_reading (exclusion of racial perspective is the harm; race as one factor is coordination). Each story carries its own epsilon, beneficiaries, and victims. The upstream/downstream pressure between them runs through the interpretive layer — the Supreme Court's seat — which sustained the sibling readings' rationales from Bakke through Fisher II and flipped to this reading in SFFA (2023).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
