% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Categorical Prohibition on State Killing (Abolition Reading)
 *   domain: legal/political/normative
 *
 * SUMMARY:
 *   This story instantiates ONE reading - the abolition reading - of the
 *   contested kernel state_killing_legitimacy; per the closed-context
 *   discipline it does not describe or average over the sibling readings,
 *   which are separate constraints linked through the network. The standing
 *   arrangement under contest is deliberate state killing of convicted
 *   persons; assessed by this reading's own lights, that arrangement
 *   appropriates the condemned person's entire remaining life with no
 *   compensable offset, which is exactly what the prohibition's categorical
 *   form encodes. Authoring follows the manifest delta: epsilon is high and
 *   anchored to the standing arrangement (the fixed referent for
 *   kernel-reading stories), condemned persons enter as rights-bearer
 *   beneficiaries of the prohibition, and the state killing power enters as
 *   the entity the prohibition strips - carried as a non-agent payer so a
 *   non-agent cannot feed the chi arithmetic directly. Claim and metrics are
 *   independent by design: the constraint is CLAIMED as a tangled rope (real
 *   coordination function, real asymmetric incidence, actively enforced
 *   against resistance), and the metrics are authored as this reading finds
 *   them, not tuned to any predicted engine verdict. KEY AGENTS (by
 *   structural relationship): see key_agents; the primary protected seat is
 *   condemned_persons (powerless/trapped), the principal cost-bearing seat is
 *   retentionist_governments (institutional/mobile, exit = abolish), the
 *   administrative enforcer is international_human_rights_bodies
 *   (institutional/analytical), the denied-claim seat is
 *   murder_victim_families_seeking_execution (organized/constrained), and the
 *   excluded lineages (retributivist_legal_tradition,
 *   utilitarian_policy_analysts) sit outside the framework by construction.
 *
 * KEY AGENTS:
 *   - condemned_persons: primary protected seat (powerless/trapped) - persons under capital sentence whose lives the prohibition insures; reconceived under this reading as rights-bearers rather than deserved recipients
 *   - future_accused_populations: diffuse insured class (moderate/constrained) - everyone exposed to capital jurisdiction, protected against wrongful execution
 *   - state_killing_power: the contested prerogative itself (institutional/non-agent, excluded from derivation) - what the prohibition strips; the manifest's 'power as victim'
 *   - retentionist_governments: principal cost-bearing states (institutional/mobile) - exit from the cost is available and equals adopting the prohibition
 *   - international_human_rights_bodies: administrative enforcer (institutional/analytical) - treaty monitoring, judgments, accession conditionality
 *   - abolitionist_constitutional_states: entrenched champions (institutional/constrained) - populations insured, diplomatic posture advantaged, rollback severely costly
 *   - murder_victim_families_seeking_execution: denied-claim seat (organized/constrained, directionality override 0.68) - desert-satisfaction demand permanently refused with no weighing slot
 *   - retributivist_legal_tradition: excluded doctrine (analytical) - lex talionis lineage ruled inadmissible by the categorical premise
 *   - utilitarian_policy_analysts: excluded doctrine (analytical) - deterrence calculus barred from the table by the categorical form
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) - documents which enforcement designs hold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.95).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.85).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.09).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.09).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Categorical Prohibition on State Killing (Abolition Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "legal/political/normative").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '67c0631c-7133-49f3-8230-8f3ece797850').
narrative_ontology:cs_kernel_codification('67c0631c-7133-49f3-8230-8f3ece797850', formalized).
narrative_ontology:cs_authority_grounding('67c0631c-7133-49f3-8230-8f3ece797850', expertise).
narrative_ontology:cs_interpretation_layer_present('67c0631c-7133-49f3-8230-8f3ece797850').
narrative_ontology:cs_reading_relation('67c0631c-7133-49f3-8230-8f3ece797850', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('67c0631c-7133-49f3-8230-8f3ece797850', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_axiom('67c0631c-7133-49f3-8230-8f3ece797850', foundational, human_dignity_admits_no_execution_exception).
narrative_ontology:cs_axiom_status(human_dignity_admits_no_execution_exception, holdable).
narrative_ontology:cs_axiom_grounding('67c0631c-7133-49f3-8230-8f3ece797850', human_dignity_admits_no_execution_exception, deontological).
narrative_ontology:cs_axiom('67c0631c-7133-49f3-8230-8f3ece797850', secondary, irreversible_judicial_error_forbids_lethal_sanction).
narrative_ontology:cs_axiom_status(irreversible_judicial_error_forbids_lethal_sanction, holdable).
narrative_ontology:cs_axiom_grounding('67c0631c-7133-49f3-8230-8f3ece797850', irreversible_judicial_error_forbids_lethal_sanction, instrumental).
narrative_ontology:cs_reference_frame('67c0631c-7133-49f3-8230-8f3ece797850', categorical_dignity_inviolability).
narrative_ontology:cs_drift_state('67c0631c-7133-49f3-8230-8f3ece797850', contemporary_global_retentionist_practice, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('67c0631c-7133-49f3-8230-8f3ece797850', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, future_accused_populations).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, abolitionist_constitutional_states).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_killing_power).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, retentionist_governments).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, murder_victim_families_seeking_execution).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, inviolable_dignity_principle).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, right_to_life_nondiscrimination).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, irreversibility_precaution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons under sentence of death in jurisdictions still carrying out executions. They await a date the state has set for killing them; appeals, clemency petitions, and diplomatic intervention are the only levers, and each is discretionary. Under this reading they stand as rights-bearers whose fundamental interest the prohibition insures; every year it holds in a jurisdiction is a year their sentence cannot end on a gallows or gurney.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% Everyone living under a jurisdiction's criminal law is a potential capital defendant, correctly convicted or not. The prohibition functions for them as standing insurance: if they are ever wrongly convicted of a capital crime, the worst outcome available is reversible imprisonment rather than irreversible death. They carry no day-to-day burden from the arrangement and usually never invoke it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, future_accused_populations, beneficiary,
    moderate, generational, constrained, national).

% Not a person or group but the inherited prerogative itself: the capacity claimed by sovereigns since the lex talionis to put a convicted person to death as the terminal expression of legal authority. The prohibition's entire operation consists of stripping this capacity - declaring it inadmissible regardless of what the condemned deserves or what executing them might achieve. What the prerogative loses, it loses entirely; it cannot be surrendered in reduced amounts.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_killing_power, payer,
    institutional, civilizational, constrained, universal).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__abolition_reading, state_killing_power).

% Governments that continue to authorize and carry out executions - the United States federal system and a shrinking set of its states, Japan, China, Iran, Belarus, and others. They bear continuous pressure: periodic treaty-body review, diplomatic criticism, exclusion from accession-linked agreements, and domestically funded abolition campaigns. The pressure ends the day they dismantle their execution machinery; until then they pay in standing and negotiating friction. Holding the prerogative is the cost; relinquishing it is the exit.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retentionist_governments, payer,
    institutional, generational, mobile, national).

% UN treaty bodies and regional courts - the Human Rights Committee, the European Court of Human Rights and its Commission, EU accession monitors. They administer the prohibition operationally: periodic country reviews, judgments treating execution as inhuman treatment, protocol signatures tracked, membership conditioned on abolition. They collect no revenue from the arrangement and bear none of its costs; they run it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Survivors of homicide who want the killer put to death as the proportionate answer. The prohibition refuses that outcome permanently, in every case, regardless of the crime's brutality. Some of these families organize politically against abolition, fund retentionist campaigns, and testify in hearings; the arrangement gives their claim no weighing slot at all.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, murder_victim_families_seeking_execution, payer,
    organized, biographical, constrained, local).

% States that abolished long ago and entrenched the ban constitutionally or by unwithdrawable treaty - Germany, Portugal, the EU bloc collectively. Their populations live under the insurance the ban provides; their diplomacy trades on it. Their cost was paid at adoption; today they spend mainly on defending the standard against rollback elsewhere. Reversal would carry severe reputational and treaty consequences.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_constitutional_states, beneficiary,
    institutional, generational, constrained, global).

% A doctrine, not an organization: the lex-talionis lineage asserting that proportional desert can forfeit a murderer's right to life. The categorical premise of the arrangement excludes this lineage from the conversation entirely - its core claim cannot be stated within the framework without contradiction. It continues in philosophy departments, some legislatures, and public opinion outside the arrangement's institutions.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributivist_legal_tradition, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__abolition_reading, retributivist_legal_tradition).

% A methodology, not a body: analysts who would admit execution if the arithmetic of deterrence and social cost came out favorable. The arrangement's categorical form bars their calculus from the table - no finding of net benefit can reopen the question. They publish cost studies and testify in retentionist jurisdictions; inside the arrangement's institutions their inputs are structurally inadmissible.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, utilitarian_policy_analysts, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(state_killing_legitimacy__abolition_reading, utilitarian_policy_analysts).

% Academic observers who map how jurisdictions encode, defend, and occasionally erode the ban - comparing entrenched constitutions, protocol regimes, and reinstatement attempts. They take no operational part; they document which enforcement designs hold and which decay.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__abolition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform floor across signatory polities: no state kills a person as punishment. Solves a genuine collective problem - judicial error is irreversible, so every jurisdiction retaining execution imposes an uninsurable wrongful-death risk on all its members - by eliminating the error class outright rather than minimizing it, and gives abolitionist coalitions a common standard to monitor and defend.
% TRANSFER_FUNCTION: Moves life-security assurance to every person under a jurisdiction's law, financed by the state's permanent forfeiture of its execution prerogative. Nothing flows back to the state except reputational and treaty standing; the condemned receive the protection first-order, the general population second-order.
% ABSENT_VOICES: Two lineages are structurally absent: retributive-desert claimants (survivors demanding proportional death) and deterrence-focused policy analysts. The categorical premise excludes both by design - admitting either would dissolve the category. They speak in retentionist legislatures, referenda, and opinion polling outside the arrangement's institutions.
% DISAPPEARANCE_RATIONALE: Overnight dissolution would reopen the execution question everywhere: several retentionist governments hold queued reinstatement legislation, condemned persons would lose the only protection currently sparing them, and abolitionist coalitions would lose the standard around which dozens of jurisdictions coordinate review and accession conditionality.
% FOUNDING_PROBLEM: The mid-century reckoning with state killing at scale: legal authorization had dignified industrial killing, and the founders sought a norm making the state's deliberate killing of a defenseless prisoner illegitimate as such - independent of what the person did or what executing him might purchase.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Amnesty International's execution censuses record thousands of confirmed executions yearly; UN Secretary-General quinquennial reports on capital punishment document continuing official practice; European Court of Human Rights and Human Rights Committee jurisprudence treats non-abolition as a live compliance issue. No seat inside the arrangement disputes that state killing continues at scale; the open dispute is whether that fact indicts the practice.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.95, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.95, anchored per the fixed referent rule to the standing arrangement (state killing) as this reading assesses it: the practice takes the condemned person's entire remaining life, and the categorical premise asserts no desert or utility offset exists - the near-ceiling value is the reading-indexed measure of that totality, drifting slightly upward as DNA-era wrongful-execution documentation accumulates (each exonerated death-row inmate deepens the irreversibility charge). Suppression is 0.85, a raw unscaled structural property covering both faces: the condemned face absolute exit-trap under the practice, and the prohibition's own enforcement machinery categorically forecloses one state option against persistent reinstitution attempts. The suppression_requirement series is authored because enforcement-capacity change is a tracked dynamic here: the arrangement began declaratory (UDHR-era norms with no machinery, 0.14) and hardened into treaty protocols, court nullification, and accession vetoes (0.85) - a rising enforcement ratchet, not a stable picture. Theater_ratio declines from 0.34 to 0.09 as declaratory symbolism gave way to operative review; the current 0.09 matches the scalar. Accessibility_collapse is 0.42: within the reading, acceptance collapses alternatives nearly completely (a categorical admits no balancing), but the sibling readings remain fully live positions across the discourse, so collapse is far from discourse-wide. Resistance is 0.70: a large retentionist bloc sustains open rejection, reinstatement attempts recur, and public opinion oscillates post-atrocity. All three series share one nine-point grid (1948-2026); no metric is sampled on a private schedule. Gain_flow is authored as condemned_persons because the protection demonstrably lands there first-order (every commutation is the gain materializing) - note this marks receipt, not rent-engineering: the condemned hold no agenda power. Fixing_cost is prohibitive: removal means breaching constitutional entrenchment or unwithdrawable protocols, against entrenched majorities and treaty partners.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergently and should. From the condemned seat the arrangement is experienced as a shield - the only thing standing between the sentence and its execution. From the retentionist-government seat the same instrument is experienced as externally imposed forfeiture, enforced through review bodies and accession conditions those governments did not consent to domestically. From the denied-family seat it is a permanent refusal that never weighs their claim. From the treaty-body seat it is administrative routine. Same structure, four incompatible phenomenologies; the engine computes per-seat classifications from power, exit, and declared position, and the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (condemned_persons, future_accused_populations, abolitionist_constitutional_states) derive low directionality - the prohibition subsidizes them, most intensively where exit is weakest (the trapped condemned seat saturates the subsidy). retentionist_governments are declared victims and derive high directionality: they hold the stripped prerogative and pay until they abolish, with exit genuinely available (mobile) - the cost is the power itself. state_killing_power carries agent=false: it is the manifest's 'power as victim,' retained for narrative completeness and excluded from derivation so that a non-agent never feeds the chi arithmetic; its forfeiture is carried structurally by the seat that holds it. One override: murder_victim_families_seeking_execution bear a real cost but appear in no declaration list, and the derivation cannot see unlisted cost-bearing - the organized-atom override (0.68) places them near-target. Atom granularity is coarse: treaty bodies and retentionist governments share the institutional atom with opposed relationships, so neither receives an override and both ride derivation or fallback; that residual imprecision is noted here rather than papered over with atom-wide corrections that would misassign the other seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - state killing at scale under color of law - remains live (corroborated externally by execution censuses and treaty-body findings), so no mandatrophy is declared and none should be: the arrangement's function has not outlived its mandate, and there is no sunset clause to atrophy past. Classification discipline cuts both ways. Reading the arrangement as pure extraction (the retentionist critique: an elite imposition against domestic preference) would erase the genuine coordination goods - elimination of the irreversible-error class, a common monitoring standard, insurance every future accused carries. Reading it as pure coordination (the movement's own self-description) would erase the real imposed costs - permanent refusal of desert-satisfaction claims and forfeiture demanded of unwilling sovereigns. The tangled_rope claim holds both faces in one structure without letting either dissolve the other. A scaffold reading is equally wrong: categorical premises do not transition; they bind indefinitely or break.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the abolition reading of kernel state_killing_legitimacy. How would the retributive and deterrence sibling readings restructure the beneficiary set, the victim set, and assessed epsilon over the same standing arrangement?',
    'Generate the two sibling stories against the same referent and compare computed per-seat classifications across the family; the divergence localizes what each reading''s premise does structurally.',
    'Under the retributive reading the condemned become sanctioned payers and epsilon drops toward desert-weighted levels; under the deterrence reading the general population becomes the intended beneficiary of the killing itself and epsilon hinges on empirical deterrence findings. Neither flip is representable inside this file by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading indexicality: this constraint is the abolition member of a three-reading kernel; sibling readings are separate constraints with different victim sets and epsilon.').

omega_variable(
    epsilon_referent_prohibition_vs_practice,
    'Does the authored epsilon measure the standing practice of state killing as this reading assesses it (the fixed referent for kernel-reading stories), or the prohibition''s own incidence on the seats it governs?',
    'Compile-time referent audit confirming the value anchors to the arrangement the story is about; per-seat chi already separates the shield face (condemned, subsidized) from the forfeiture face (power-holders, amplified).',
    'Anchored to the prohibition alone, epsilon would collapse toward the enforcement-mechanism coordination floor and the story would recompute toward pure coordination; the current authoring keeps the referent on the standing arrangement, which is what the categorical premise is a response to.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_prohibition_vs_practice, conceptual, 'Residual ambiguity between practice-assessed epsilon and prohibition-incidence epsilon within a single authored story.').

omega_variable(
    categorical_dignity_non_negotiability,
    'Is dignity-violability genuinely categorical in the entrenched instruments - no threshold of aggregate utility or extremity rescues an execution - or do formulations admit implicit weighting?',
    'Doctrinal survey of protocol texts, constitutional clauses, and treaty-body general comments for exception carve-outs (wartime treason, military codes), plus drafting-history interviews.',
    'Any admitted exception converts the constraint from a categorical side-constraint into a weighted trade-off, weakening the categorical anchor on epsilon and moving the computed classification toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_dignity_non_negotiability, conceptual, 'Whether the prohibition''s categorical form survives contact with its own codifications.').

omega_variable(
    entrenchment_durability_against_reinstatement,
    'Can the enforcement entrenchment hold through populist reinstatement surges - post-atrocity crime waves, referendum campaigns, capture of reviewing bodies?',
    'Track reinstatement attempts in abolitionist jurisdictions over coming decades and score success rates against entrenchment depth (constitutional unamendability versus statutory repeal).',
    'A successful reinstatement would reverse the suppression_requirement trajectory, date a lifecycle turn toward decay, and shift family balance toward the licensing readings; repeated failure confirms the hardened-enforcement picture the series records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_durability_against_reinstatement, empirical, 'Durability of the prohibition''s enforcement machinery against reversal pressure.').

omega_variable(
    denied_desert_satisfaction_cost_status,
    'Do survivors denied executions bear a cost this arrangement must weigh, or is their claim an overridden preference under the categorical premise?',
    'Preference-resolved: settled only by accepting or rejecting the deontological axiom; longitudinal studies of survivor outcomes under abolition inform but cannot determine the answer.',
    'Counting the cost raises the denied-claim seat''s effective burden and pushes the hybrid structure further from pure coordination; overriding it keeps the arrangement''s costs concentrated on the power-holders alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(denied_desert_satisfaction_cost_status, preference, 'Status of the desert-satisfaction claim the prohibition permanently refuses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_abolition_tr_t1948, state_killing_legitimacy__abolition_reading, theater_ratio, 1948, 0.34).
narrative_ontology:measurement(skl_abolition_tr_t1955, state_killing_legitimacy__abolition_reading, theater_ratio, 1955, 0.3).
narrative_ontology:measurement(skl_abolition_tr_t1965, state_killing_legitimacy__abolition_reading, theater_ratio, 1965, 0.26).
narrative_ontology:measurement(skl_abolition_tr_t1980, state_killing_legitimacy__abolition_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(skl_abolition_tr_t1990, state_killing_legitimacy__abolition_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement(skl_abolition_tr_t2000, state_killing_legitimacy__abolition_reading, theater_ratio, 2000, 0.13).
narrative_ontology:measurement(skl_abolition_tr_t2010, state_killing_legitimacy__abolition_reading, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(skl_abolition_tr_t2020, state_killing_legitimacy__abolition_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(skl_abolition_tr_t2026, state_killing_legitimacy__abolition_reading, theater_ratio, 2026, 0.09).

% Extraction over time
narrative_ontology:measurement(skl_abolition_be_t1948, state_killing_legitimacy__abolition_reading, base_extractiveness, 1948, 0.88).
narrative_ontology:measurement(skl_abolition_be_t1955, state_killing_legitimacy__abolition_reading, base_extractiveness, 1955, 0.89).
narrative_ontology:measurement(skl_abolition_be_t1965, state_killing_legitimacy__abolition_reading, base_extractiveness, 1965, 0.89).
narrative_ontology:measurement(skl_abolition_be_t1980, state_killing_legitimacy__abolition_reading, base_extractiveness, 1980, 0.91).
narrative_ontology:measurement(skl_abolition_be_t1990, state_killing_legitimacy__abolition_reading, base_extractiveness, 1990, 0.92).
narrative_ontology:measurement(skl_abolition_be_t2000, state_killing_legitimacy__abolition_reading, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(skl_abolition_be_t2010, state_killing_legitimacy__abolition_reading, base_extractiveness, 2010, 0.93).
narrative_ontology:measurement(skl_abolition_be_t2020, state_killing_legitimacy__abolition_reading, base_extractiveness, 2020, 0.94).
narrative_ontology:measurement(skl_abolition_be_t2026, state_killing_legitimacy__abolition_reading, base_extractiveness, 2026, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(skl_abolition_su_t1948, state_killing_legitimacy__abolition_reading, suppression_requirement, 1948, 0.14).
narrative_ontology:measurement(skl_abolition_su_t1955, state_killing_legitimacy__abolition_reading, suppression_requirement, 1955, 0.18).
narrative_ontology:measurement(skl_abolition_su_t1965, state_killing_legitimacy__abolition_reading, suppression_requirement, 1965, 0.24).
narrative_ontology:measurement(skl_abolition_su_t1980, state_killing_legitimacy__abolition_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(skl_abolition_su_t1990, state_killing_legitimacy__abolition_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(skl_abolition_su_t2000, state_killing_legitimacy__abolition_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(skl_abolition_su_t2010, state_killing_legitimacy__abolition_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(skl_abolition_su_t2020, state_killing_legitimacy__abolition_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(skl_abolition_su_t2026, state_killing_legitimacy__abolition_reading, suppression_requirement, 2026, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% Kernel state_killing_legitimacy decomposes into three readings; this file instantiates only the abolition reading. The colloquial label 'the death penalty debate' conflates structurally distinct constraints: a categorical dignity prohibition (this story - high assessed epsilon, condemned persons shielded, the killing power stripped), a proportional-desert licensing rule (retributive reading - condemned persons as sanctioned payers), and a deterrence-signal instrument (deterrence reading - populations positioned as beneficiaries of the killing itself). Each carries its own epsilon, victim set, and classification; the family links exist so erosion of the categorical premise propagates visibly into the licensing readings' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__abolition_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
