% ============================================================================
% CONSTRAINT STORY: salic_prohibition__cognatic_reversion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__cognatic_reversion_reading, []).

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
 *   constraint_id: salic_prohibition__cognatic_reversion_reading
 *   human_readable: Salic Exclusion as Bounded Frankish Custom (Cognatic Reversion Reading)
 *   domain: constitutional/political-history
 *
 * SUMMARY:
 *   Lex Salica began as the customary code of the Salian Franks, compiled
 *   around the turn of the sixth century; its inheritance title governed
 *   private allodial land within the male kin-group. Six centuries later,
 *   French royal argument assembled for the 1328 vacancy repurposed the label
 *   into a rule barring female succession to crowns, and the doctrine spread
 *   across European dynastic law. This story instantiates ONE reading of the
 *   contested salic_prohibition kernel: the cognatic_reversion_reading, which
 *   holds that the exclusion was a territorially bounded Frankish custom,
 *   never properly binding outside its original jurisdiction, and that
 *   succession should run by cognatic primogeniture with territorial
 *   integrity weighted above agnatic purity. The epsilon referent is the
 *   standing arrangement under contest — the operation of Salic exclusion
 *   across European successions — assessed by this reading's own lights; the
 *   sibling readings share the referent and author different values over it
 *   (immutable_mandate_reading: higher suppression, universal scope,
 *   natural/divine warrant; sovereign_override_reading: lower epsilon,
 *   ordinary-legislation framing). CONSTRAINT FAMILY: the colloquial label
 *   'Salic Law' decomposes into three structurally distinct readings of one
 *   kernel; each is a separate story with its own epsilon, victim set, and
 *   classification, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - agnatic_male_line_dynasts: Primary beneficiary (institutional/constrained) — titles subsidized by the exclusion of female lines
 *   - - cadet_branch_claimants: Secondary beneficiary (powerful/constrained) — prospects depend on senior female claims failing
 *   - - female_heirs_and_descendants: Primary target (moderate/trapped) — claims nullified, exit through coerced renunciation
 *   - - cognatic_claimants: Primary target (powerful/constrained) — materially armed but juridically nullified
 *   - - royal_jurists_and_chancellors: Agenda setter (institutional/mobile) — constructs, records, and administers the doctrine; collects office and patronage
 *   - - realm_subjects_bearing_war_costs: Excluded party (organized/trapped) — bears the war bill with no seat in the councils
 *   - - treaty_guarantor_powers: Observer (institutional/mobile) — guarantees and monitors succession settlements
 *   - - constitutional_historians: Analytical observer (analytical/analytical) — sees the full transmission chain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, 0.52).
domain_priors:suppression_score(salic_prohibition__cognatic_reversion_reading, 0.38).
domain_priors:theater_ratio(salic_prohibition__cognatic_reversion_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(salic_prohibition__cognatic_reversion_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__cognatic_reversion_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__cognatic_reversion_reading, "Salic Exclusion as Bounded Frankish Custom (Cognatic Reversion Reading)").
narrative_ontology:topic_domain(salic_prohibition__cognatic_reversion_reading, "constitutional/political-history").

domain_priors:requires_active_enforcement(salic_prohibition__cognatic_reversion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__cognatic_reversion_reading, '38732798-572f-4f83-88d7-0a0f4ee44a84').
narrative_ontology:cs_kernel_codification('38732798-572f-4f83-88d7-0a0f4ee44a84', fixed_text).
narrative_ontology:cs_authority_grounding('38732798-572f-4f83-88d7-0a0f4ee44a84', lineage).
narrative_ontology:cs_interpretation_layer_present('38732798-572f-4f83-88d7-0a0f4ee44a84').
narrative_ontology:cs_reading_relation('38732798-572f-4f83-88d7-0a0f4ee44a84', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('38732798-572f-4f83-88d7-0a0f4ee44a84', salic_prohibition__sovereign_override_reading, coexists_with).
narrative_ontology:cs_axiom('38732798-572f-4f83-88d7-0a0f4ee44a84', foundational, salic_law_territorially_bounded).
narrative_ontology:cs_axiom_status(salic_law_territorially_bounded, holdable).
narrative_ontology:cs_axiom_grounding('38732798-572f-4f83-88d7-0a0f4ee44a84', salic_law_territorially_bounded, empirically_contingent).
narrative_ontology:cs_axiom('38732798-572f-4f83-88d7-0a0f4ee44a84', foundational, territorial_integrity_over_agnatic_purity).
narrative_ontology:cs_axiom_status(territorial_integrity_over_agnatic_purity, holdable).
narrative_ontology:cs_axiom_grounding('38732798-572f-4f83-88d7-0a0f4ee44a84', territorial_integrity_over_agnatic_purity, instrumental).
narrative_ontology:cs_reference_frame('38732798-572f-4f83-88d7-0a0f4ee44a84', frankish_customary_jurisdiction).
narrative_ontology:cs_drift_state('38732798-572f-4f83-88d7-0a0f4ee44a84', pan_european_dynastic_application, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('38732798-572f-4f83-88d7-0a0f4ee44a84', '').
narrative_ontology:cs_kernel_id(salic_prohibition__cognatic_reversion_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, agnatic_male_line_dynasts).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, cadet_branch_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, female_heirs_and_descendants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, cognatic_claimants).
narrative_ontology:constraint_victim(salic_prohibition__cognatic_reversion_reading, realm_subjects_bearing_war_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__cognatic_reversion_reading, royal_jurists_and_chancellors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold or claim thrones through uninterrupted male descent lines. When a reigning line lacks sons, the nearest male kinsman takes the succession under the rule they invoke. They retain jurists to elaborate the doctrine, arrange marriages to preserve male heirs, and treat the exclusion of female lines as the price of their own titles. Leaving the arrangement would mean conceding the principle that elevated them.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, agnatic_male_line_dynasts, beneficiary,
    institutional, generational, constrained, continental).

% Younger male lines whose prospects depend on senior female claims failing. They press the exclusion hardest when a senior line ends in daughters, and accept the same rule binding their own daughters in exchange for their place in the order of reversion.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cadet_branch_claimants, beneficiary,
    powerful, generational, constrained, continental).

% Daughters and granddaughters of kings whose claims are treated as void. Some sign formal renunciations under treaty pressure; others become figureheads of opposing parties, as in the Carlist conflict. Their exit runs through renunciation documents executed under duress or through war fought on their behalf by others.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, female_heirs_and_descendants, payer,
    moderate, biographical, trapped, continental).

% Claimants whose descent runs through a female link — a king's daughter or sister. Their material power may be considerable, an armed kingdom behind them, yet the rule nullifies the claim itself, so their options reduce to renunciation, negotiated compensation, or prosecuting the claim by force.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, cognatic_claimants, payer,
    powerful, biographical, constrained, continental).

% Search old texts, issue opinions, and staff the councils that decide contested successions. Office, patronage, and scholarly reputation flow to those who articulate the rule the reigning house needs. Their expertise travels between courts, so individual departure is easy even while the doctrine they built persists without them.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, royal_jurists_and_chancellors, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__cognatic_reversion_reading, royal_jurists_and_chancellors, beneficiary).

% Peasants, towns, and taxpayers who fight and fund the wars that contested successions ignite. They hold no seat in dynastic councils; their stake enters only as conscription and levy. Departure means flight or revolt.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, realm_subjects_bearing_war_costs, excluded,
    organized, generational, trapped, regional).

% Foreign crowns that sign and guarantee succession settlements — renunciations, pragmatic sanctions, partition treaties. They trade recognition for advantage elsewhere, monitor compliance, and intervene when settlements break.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, treaty_guarantor_powers, observer,
    institutional, generational, mobile, continental).

% Reconstruct what the Merovingian text actually regulated, when the crown doctrine first appears in the record, and which authorities applied it where. They see the whole transmission chain and belong to no claimant's camp.
narrative_ontology:constraint_stakeholder(salic_prohibition__cognatic_reversion_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__cognatic_reversion_reading, agnatic_male_line_dynasts).
narrative_ontology:fixing_cost_class(salic_prohibition__cognatic_reversion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single determinate answer to 'who reigns next' at each interregnum: succession passes through male lines in fixed proximity order, giving councils, courts, and treaty-makers a settled rule to apply instead of adjudicating every vacancy anew.
% TRANSFER_FUNCTION: Moves succession rights — and with them sovereignty, revenue, and alliance value — from female-line heirs and their descendants to male-line agnates; secondarily moves adjudicating authority and patronage to the jurists and councils who administer the rule.
% ABSENT_VOICES: Realm subjects who fund and fight the resulting wars had no seat in dynastic councils; women dynasts spoke only through proxies or through renunciation drafts prepared for their signature; jurists trained outside the agnatic tradition — Roman-law faculties, other customary regimes — were rarely invited into the rooms where the doctrine was elaborated.
% DISAPPEARANCE_RATIONALE: Overnight removal rewrites the succession settlements of Europe: the 1328 Valois accession fails in favor of the English claim through Isabella, the Bourbon and Habsburg arrangements take different shapes, the Carlist conflict never organizes, and modern constitutional succession orders begin from cognatic primogeniture rather than amending away male preference.
% FOUNDING_PROBLEM: Preserve allodial family land intact within the male kin-group of the Salian Franks: the inheritance title of the Merovingian compilation bars daughters from taking land while sons live, preventing parcelization through marriage and dowry. Centuries later the label was repurposed to supply a determinate male-line rule of royal succession.
% FOUNDING_PROBLEM_CORROBORATION: Legal-historical scholarship outside the beneficiary set attests the genealogy: the Merovingian text regulates private allodial inheritance and contains no royal-succession clause, and the crown doctrine first appears in fourteenth-century French royal argument assembled for the 1328 decision. No contemporary Frankish source attests a royal application. The dynastic beneficiaries never attested the bounded-custom account — they asserted timeless validity instead — so corroboration rests entirely on external legal-historical and philological work.
narrative_ontology:disappearance_verdict(salic_prohibition__cognatic_reversion_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__cognatic_reversion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__cognatic_reversion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__cognatic_reversion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__cognatic_reversion_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__cognatic_reversion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__cognatic_reversion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__cognatic_reversion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52 at interval end from this reading's lights: the arrangement nullified roughly half the eligible heir pool and their entire descendant lines, transferring thrones and alliance value to agnates — substantial extraction, now reduced by two centuries of reform. Suppression is authored at 0.38 as a RAW structural property, unscaled by power or scope: enforcement machinery (courts, renunciation instruments, armed defense of settlements) has largely dissolved, though residual male-preference statutes persist in a few realms. Theater ratio is high (0.65) because the arrangement's maintenance is now dominated by citation of antiquity — invoking a Merovingian land statute for a crown rule it never contained. The temporal series runs on one shared seven-point grid (t0 approximates the 1328 Valois decision; t60 approximates the 2013 UK Succession Act era): extractiveness accumulates to a peak around t30 (the era of renunciations, pragmatic sanctions, and the War of the Austrian Succession) then decays as realms legislate cognatic succession; suppression follows the same accumulation-decay arc, tracking enforcement capacity rather than extraction; theater rises monotonically as the original function recedes and performative antiquity-citation takes over. The claim/metric pair is authored independently: claimed_type tangled_rope states this reading's structural assessment (genuine coordination service — determinate succession — operating with asymmetric, actively enforced extraction); the metrics describe actual operation including its decay. Where the engine computes a different type for a seat, that divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience one structure. From the jurist seat the arrangement is a craft product: texts searched, opinions issued, councils staffed, reputations made. From the agnate seats it is entitlement — the principle that put their house on the throne. From the female-heir and cognate-claimant seats it is nullification of what birth gave them, enforceable against them despite considerable material power. From the excluded subject seats it is a war bill they never voted. The engine computes per-seat types from power, exit, and role; the divergence across seats is the finding, not noise to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Agnatic dynasts and cadet branches derive low d from their beneficiary declarations — the rule subsidizes their titles. Female heirs, cognatic claimants, and war-bearing subjects derive high d from their victim declarations; trapped or constrained exit keeps them near the target end, and the cognate claimant case shows why power alone does not damp d: a claimant with an army behind him is still structurally targeted, because the rule operates on the claim itself rather than on his capacity. The jurists carry a mid-range d through their secondary collection of office and patronage — administrators who mildly profit from the doctrine they run. Treaty guarantors and historians sit at the analytical end. Only extractiveness is scaled by directionality and scope; suppression is authored raw.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allodial parcelization among Salian Franks — has been extinct for a millennium, and the crown-succession application was a repurposing that outlived both its text's subject matter and, in most realms, its enforcement machinery; mandatrophy_resolved is declared on that basis. The classification prevents two opposite mislabels: reading the arrangement as pure imposition ignores the real coordination service determinate succession provided for centuries across dozens of interregna; reading it as living coordination ignores that what remains is largely ceremonial maintenance of a rule whose warrant — Frankish custom — no longer reaches its objects. The temporal series shows the classic late-life profile, enforcement decay alongside rising theatricality; that is drift data for the lifecycle detectors, not an instruction to retune the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'This story is one reading of the salic_prohibition kernel — the cognatic_reversion_reading. Which reading governs assessment: bounded Frankish custom with cognatic reversion (this), irrevocable natural/divine mandate (immutable_mandate_reading), or revocable sovereign positive law (sovereign_override_reading)?',
    'Comparative structural analysis across the three sibling stories — victim sets, enforcement bases, scope atoms, and epsilon values — together with adjudication of which characterization matches the sources'' own warrant; the engine computes per-seat classifications for each reading separately.',
    'Sibling readings relocate the disagreement structurally: the immutable-mandate reading universalizes scope and raises suppression; the sovereign-override reading reframes the arrangement as ordinary legislation and lowers epsilon toward coordination cost. Classification of THIS constraint is reading-indexed and does not transfer to siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: one kernel, three readings; this file instantiates the cognatic reversion reading only.').

omega_variable(
    contemporary_jurisdictional_recognition,
    'Did any medieval or early-modern authority actually hold the jurisdictional-bounded view contemporaneously, or is the anachronism thesis itself a retrospective reconstruction by later scholarship?',
    'Manuscript, gloss, and diplomatic history: whether any glossator, council record, or treaty argument before the nineteenth century argued that Salic custom bound only Franks or only Frankish territory.',
    'If no contemporary authority held it, this reading is a modern corrective lens and its historical assessments index a present-day evaluation projected backward; if some did, the reading revives a suppressed live position and its resistance profile rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_jurisdictional_recognition, empirical, 'Whether the reading''s core historical premise was ever a live contemporaneous position.').

omega_variable(
    coordination_extraction_separability,
    'Is the determinate-succession function separable from the agnatic-exclusion form, or did securing order in the dynastic context require excluding female lines?',
    'Counterfactual comparison with realms that operated cognatic or elective succession without collapse — Iberian female reigns, Habsburg pragmatic-sanction arrangements, the post-reform monarchies.',
    'If separable, the exclusion component is removable overhead riding on real coordination; if inseparable, part of the measured extraction is the historical price of the order the arrangement provided.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and extraction components of the arrangement are structurally separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal nullification backed by force and treaty) or internalized (dynastic actors treating agnatic legitimacy as self-evident, persisting after legal barriers fall)?',
    'Post-reform trajectory: where succession laws liberalized, did male-preference assumptions persist in court practice, public sentiment, and drafting habits after the legal barrier was removed?',
    'If internalized, effective suppression exceeds the structural measure and outlives formal repeal; if structural, repeal collapses it. Informs the omega-resolved suppression estimate and any residual-enforcement findings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the dynastic-legitimacy setting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__cognatic_reversion_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__cognatic_reversion_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sali_tr_t10, salic_prohibition__cognatic_reversion_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(sali_tr_t20, salic_prohibition__cognatic_reversion_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__cognatic_reversion_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__cognatic_reversion_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement(sali_tr_t50, salic_prohibition__cognatic_reversion_reading, theater_ratio, 50, 0.61).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__cognatic_reversion_reading, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(sali_be_t10, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(sali_be_t20, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(sali_be_t50, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 50, 0.61).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__cognatic_reversion_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sali_su_t10, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(sali_su_t20, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(sali_su_t50, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__cognatic_reversion_reading, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__cognatic_reversion_reading, resource_allocation).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__cognatic_reversion_reading, salic_prohibition__sovereign_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Salic Law' decomposes into three structurally distinct readings of one kernel (salic_prohibition), per the epsilon-invariance principle — forcing one story to span all three would make epsilon observer-dependent. This file authors the cognatic_reversion_reading; immutable_mandate_reading and sovereign_override_reading are separate stories with their own epsilon, victim sets, and classifications. Influence structure: the immutable-mandate reading historically supplied the rhetoric to which the other two respond; this reading creates downstream pressure on the sovereign-override reading by supplying the jurisdictional critique that legislatures cited when repealing male preference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
