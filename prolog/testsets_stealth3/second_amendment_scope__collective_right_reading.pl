% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [SUPERSEDED (repudiated as governing doctrine by Heller v. District of Columbia, 2008)]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective-Rights Reading: State Militia Authority Protection
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   second_amendment_scope: the collective-rights reading, under which the
 *   Second Amendment's prefatory militia clause qualifies its operative
 *   clause, so that the text protects state authority to maintain militias —
 *   and nothing else. The arrangement this reading produces shields state
 *   military institutions (today the National Guard) from federal disarmament
 *   while leaving regulation of privately owned firearms entirely to ordinary
 *   lawmaking; individual ownership claims receive no constitutional
 *   standing. The interval 0-30 maps to 1978-2008, the reading's mature
 *   administrative era, ending at its repudiation in Heller v. District of
 *   Columbia. Epsilon's referent is the standing arrangement THIS reading
 *   instantiates, assessed by the reading's own lights: within that frame the
 *   arrangement extracts little (it protects an institutional relationship
 *   and imposes no burden on private conduct beyond ordinary law), hence low
 *   extractiveness. The grievance of excluded individual claimants is real
 *   but belongs to the sibling readings' ledgers — it is routed to omega
 *   variables here, not averaged into this constraint's epsilon. The claimed
 *   type and the authored metrics are independent facts: the metrics trace a
 *   functioning coordination arrangement whose protective object atrophied
 *   and whose enforcement costs escalated until an external interpretive
 *   shock removed it. Sibling readings (individual_right_reading,
 *   civic_right_reading) are separate constraint files linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   state_governments: Primary beneficiary (institutional/constrained) -
 *   organized_militia_institutions: Secondary beneficiary
 *   (institutional/constrained) - federal_firearms_regulators: Beneficiary
 *   (institutional/mobile) - federal_judiciary: Agenda setter
 *   (institutional/analytical) - individual_arms_claimants: Excluded voice
 *   (organized/constrained) - constitutional_scholars: Analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - - state_governments: Primary beneficiary (institutional/constrained) — hold constitutionally protected authority to maintain their own military forces; cannot exit the federal union that hosts the protection.
 *   - - organized_militia_institutions: Secondary beneficiary (institutional/constrained) — the state military forces whose protected lane the arrangement reserves; their dual state-federal identity is the arrangement's operational core.
 *   - - federal_firearms_regulators: Beneficiary (institutional/mobile) — federal lawmakers and agencies whose rule-writing over private arms faces no objection from the arms clause under this reading; lost their widest policy latitude when the reading fell.
 *   - - federal_judiciary: Agenda setter (institutional/analytical) — administers the reading, decides which claims it answers, and ultimately replaced it; its enforcement effort escalated across the interval.
 *   - - individual_arms_claimants: Excluded voice (organized/constrained) — citizens asserting a personal entitlement to arms; hold no constitutional card under this reading and pursued the claim through politics and scholarship instead.
 *   - - constitutional_scholars: Analytical observer (analytical/analytical) — map the dispute, excavate founding-era practice, and supply the counter-consensus that armed the winning side.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.22).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective-Rights Reading: State Militia Authority Protection").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, 'ca296764-5815-48b9-9233-cc862227c2ed').
narrative_ontology:cs_kernel_codification('ca296764-5815-48b9-9233-cc862227c2ed', fixed_text).
narrative_ontology:cs_authority_grounding('ca296764-5815-48b9-9233-cc862227c2ed', lineage).
narrative_ontology:cs_interpretation_layer_present('ca296764-5815-48b9-9233-cc862227c2ed').
narrative_ontology:cs_reading_relation('ca296764-5815-48b9-9233-cc862227c2ed', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ca296764-5815-48b9-9233-cc862227c2ed', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('ca296764-5815-48b9-9233-cc862227c2ed', foundational, prefatory_clause_limits_protected_class).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_protected_class, holdable).
narrative_ontology:cs_axiom_grounding('ca296764-5815-48b9-9233-cc862227c2ed', prefatory_clause_limits_protected_class, conventional).
narrative_ontology:cs_axiom('ca296764-5815-48b9-9233-cc862227c2ed', foundational, states_are_the_amendment_rights_bearers).
narrative_ontology:cs_axiom_status(states_are_the_amendment_rights_bearers, holdable).
narrative_ontology:cs_axiom_grounding('ca296764-5815-48b9-9233-cc862227c2ed', states_are_the_amendment_rights_bearers, conventional).
narrative_ontology:cs_reference_frame('ca296764-5815-48b9-9233-cc862227c2ed', prefatory_clause_qualified_operative_text).
narrative_ontology:cs_drift_state('ca296764-5815-48b9-9233-cc862227c2ed', heller_repudiation_moment, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('ca296764-5815-48b9-9233-cc862227c2ed', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militia_institutions).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federal_firearms_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fifty constitutional polities that maintain military forces under their own authority (today chiefly the National Guard) while ceding general defense to the federal government. The arrangement guarantees them institutional standing to arm and organize their own forces without federal veto; in exchange they accept federal funding and integration standards. Leaving the arrangement would mean abandoning federal military funding or the union itself, neither of which any state treats as available.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% The state military forces themselves — officer corps, armories, enlistment structures — that occupy the protected lane the constitutional settlement reserves for them. They receive state authority and federal money through the same door, and their institutional self-conception is bound up with the dual state-federal identity the settlement preserves.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militia_institutions, beneficiary,
    institutional, generational, constrained, national).

% Federal lawmakers and enforcement agencies writing rules for privately owned firearms. Because the arrangement reads the constitutional text as speaking only to militia institutions, their rule-writing faces no objection from the arms clause itself; their policy space is bounded by statute and politics alone. When the reading fell, this seat lost the widest policy latitude in the system and adapted to a new doctrinal environment rather than collapsing.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_firearms_regulators, beneficiary,
    institutional, biographical, mobile, national).

% The courts, above all the Supreme Court, that decide what the constitutional text protects. For decades this seat administered the militia-centered reading, rejecting personal-right claims in case after case; administering it grew costlier as counter-scholarship and political mobilization mounted, until the seat itself overturned the reading in 2008. Its only exit is doctrinal: it changes the reading by rewriting it.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Private citizens who assert a personal entitlement to keep and bear arms regardless of militia service. Under this arrangement they hold no constitutional card: their claims fail in court, and their recourse runs entirely through legislation, elections, and cultural persistence. Organized membership organizations and litigating networks kept the claim alive for decades until the reading was overturned.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_arms_claimants, excluded,
    organized, biographical, constrained, national).

% Academic lawyers and historians who mapped the dispute: excavating founding-era militia practice, testing the prefatory-clause grammar, and building the counter-consensus that eventually reached the courts. They collect nothing from the arrangement and bear none of its costs; their product became the evidentiary arsenal of the side that won.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides authority over organized armed force between two levels of government: states keep institutional capacity to arm, organize, and deploy their own military forces; the federal government conducts general defense and writes the rules for privately held arms without constitutional interference from the arms clause. The problem solved is the founding-era one of securing state military institutions inside a union with a national army.
% TRANSFER_FUNCTION: Moves constitutional protection to state military institutions and regulatory discretion to federal and state lawmakers. Concretely: personal-right claims filed in court are transferred out of the judicial channel into legislative politics, where organized majorities prevail; no goods or money move through the arrangement itself.
% ABSENT_VOICES: Individual arms claimants were the structurally absent seat: for most of the interval, courts answered 'whose right does the text protect?' without seating the people who claimed it personally. Founding-era historians questioning the subordination reading of the prefatory clause were likewise marginal until the counter-scholarship of the 1980s and 1990s. Both voices entered the conversation only as the reading's enforcement costs grew unsustainable.
% DISAPPEARANCE_RATIONALE: Had the militia-centered reading vanished at its height, state military institutions lose their constitutional shield against federal absorption (a live worry in the Guard-federalization fights of the mid-century), federal firearms regulation immediately faces constitutional challenge it currently deflects, and the personal-right movement gains its judicial channel decades early — the entire subsequent arc of American arms jurisprudence rearranges around a different reading.
% FOUNDING_PROBLEM: Reconcile the founding generation's dread of standing armies with the need for common defense: the text was written so that states would retain 'well regulated Militia' capacity as the security of a free state, checking federal monopolization of armed force.
% FOUNDING_PROBLEM_CORROBORATION: Anti-Federalist ratification-era writings and state ratification records attest the founding problem from outside the eventual settlement's beneficiaries; twentieth-century military historiography (the Dick Act and National Defense Act literature) attests that the problem's object — an independent state militia — was progressively absorbed into a federally integrated Guard, hollowing the mandate; individual-right scholars corroborate from an opposing seat that the problem no longer sustains a militia-only reading. No attestation comes from the excluded claimant seat during the reading's early decades — their scholarship arrives late in the interval.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22 at interval end) because the arrangement's burden falls almost entirely on government action-space, not on governed populations: it protects a narrow institutional relationship (state military capacity) and leaves private conduct to ordinary democratic lawmaking. Suppression (0.58) is a raw, unscaled structural property and is the story's dynamic center: sustaining the militia-only reading required active judicial work — dismissing personal-right claims circuit by circuit — and that enforcement burden climbed steadily as counter-scholarship and organized political mobilization grew, peaking just before the 2008 repudiation. Theater_ratio rises from 0.15 to 0.42 because the arrangement's protected object decayed underneath it: the independent state militia of the founding design was progressively absorbed into a federally funded, federally disciplined Guard, so an increasing share of the reading's operation consisted of defending a constitutional category whose living referent had thinned. Accessibility_collapse is moderate-low (0.40): rival readings of the same text remained fully articulable throughout — two of them are live sibling constraints — and ultimately prevailed. Resistance is high (0.78): few interpretive arrangements in American law have been contested so long or so effectively. All three tracked series share one time grid (points 0, 5, 10, 15, 20, 25, 30), so no metric's end-state is backfilled onto earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different arrangements from the same text. State governments and militia institutions experience the arrangement as federalism insurance: a guarantee that their military capacity cannot be extinguished from Washington. Federal regulators experience it as unobstructed policy space — the same reading that protects states removes the arms clause from their path. Individual arms claimants experience a closed door: the question 'whose right?' was answered without seating them, and their only exits ran through legislatures and elections. The judiciary experienced a stable doctrine for decades and then an accelerating enforcement burden it ultimately refused to keep carrying. There is no payer seat inside this reading's own frame — the nearest analogue, the federal government's surrendered power to disarm state forces, was priced by the reading as negligible because the militia it would have disarmed was already federally integrated. That absence of an in-frame payer is exactly why the arrangement computes as low-extraction from its own seat while remaining bitterly contested from outside it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (state_governments, organized_militia_institutions, federal_firearms_regulators) sit near the beneficiary end of directionality: the arrangement subsidizes their authority and policy space. No victims are declared because this reading's frame contains none — individual claimants are denied standing, which is denial of a benefit they claim, not extraction the arrangement levies; importing their grievance as victimhood would fold the sibling readings' constraint into this one. The excluded seat therefore derives its position from absence of declaration rather than from a victim entry, and the story's modest residual extractiveness reflects scope amplification (national scope makes the interpretive monopoly harder to verify or evade) rather than any targeted transfer. The judiciary, as agenda setter, is positioned by administration rather than collection: it spends enforcement effort rather than receiving rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guaranteeing state militia capacity against federal standing-army dominance — substantially died underneath the arrangement: the militia became the federally funded, federally disciplined National Guard, and the dread that built the text lost its object. The arrangement nonetheless persisted on doctrinal inertia for decades, which is why mandatrophy_resolved is declared true and why the theater_ratio series climbs. Classifying this as a rope rather than a snare prevents the inverse error: because the reading's frame contains no victims, reading it as extraction-from-gun-owners would import the individual-rights reading's ledger into a constraint that never levied one. Conversely, the classification refuses piton at interval end: the arrangement was not quietly atrophying into performance — it was killed by an external interpretive contest it ultimately lost, with enforcement costs rising to the very end. The post-2008 residue (minority scholarship, occasional losing briefs) trends piton-ward and is flagged for a successor story rather than folded into this interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story is one reading of the kernel second_amendment_scope. What structurally changes if a sibling reading governs instead?',
    'Compile and compare the sibling stories (individual_right_reading, civic_right_reading): beneficiary sets, victim sets, and epsilon values over the same referent text.',
    'Under individual_right_reading, individual claimants enter the beneficiary set and regulators become targets; under civic_right_reading, militia-participating individuals gain conditional standing. This reading''s low epsilon and state-centered beneficiary set hold only within its own frame.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the arms-scope kernel this constraint instantiates and what siblings would change.').

omega_variable(
    prefatory_clause_interpretive_force,
    'Does the prefatory militia clause actually limit the operative clause''s protection, or is it a non-restrictive rationale?',
    'Systematic analysis of founding-era drafting, ratification records, contemporaneous usage of ''well regulated militia'' and ''bear arms'', and early militia acts.',
    'Strong restrictive evidence stabilizes this reading''s structure and low epsilon; weak evidence transfers legitimacy to the sibling readings and reclassifies the arrangement''s beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_interpretive_force, empirical, 'The textual crux on which the whole kernel contest turns.').

omega_variable(
    militia_institution_atrophy_trajectory,
    'Did the institution this reading protects remain a live, independent state military capacity, or was it absorbed into a federally integrated Guard?',
    'Military-historical record: Dick Act 1903, National Defense Act 1916, post-1945 integration, Guard dual-command practice.',
    'If absorbed, the reading''s protective function decays toward performance and the post-repudiation residue trends inertial; if live, the arrangement''s coordination function persists and the low-extraction reading strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_institution_atrophy_trajectory, empirical, 'Whether the protected object of the arrangement thinned beneath the doctrine.').

omega_variable(
    excluded_claimant_recourse_adequacy,
    'Was political recourse (legislation, elections, cultural mobilization) an adequate substitute for the judicial channel this reading closed to individual claimants?',
    'Compare outcomes for claimant goals under judicial-channel regimes (post-2008) versus the political-channel decades: policy durability, protection of disadvantaged claimants, responsiveness.',
    'If recourse was inadequate, this reading''s low epsilon is seat-relative only — outsider seats compute heavier effective burdens, explaining the resistance series; if adequate, the low reading-indexed epsilon is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_claimant_recourse_adequacy, preference, 'Whether closing the judicial channel to individuals imposed uncompensated burdens the reading''s frame cannot register.').

omega_variable(
    disarmament_history_shadow,
    'Did the militia-only reading''s long dominance facilitate state disarmament campaigns aimed at discrete groups (post-Reconstruction south, twentieth-century urban regulations) whose costs the reading''s own frame never registers?',
    'Historical record of state firearms regulation and its differential enforcement, 1870-1965, and its interaction with the absence of any individual constitutional check.',
    'Registering those episodes as costs raises observer-seat epsilon estimates and reframes the arrangement''s coordination story; the reading''s own frame counts none of it, which is precisely the perspectival gap the corpus measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_history_shadow, empirical, 'Historical shadow costs of the reading''s dominance, invisible from its own seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__collective_right_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t5, second_amendment_scope__collective_right_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__collective_right_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(seco_tr_t15, second_amendment_scope__collective_right_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__collective_right_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(seco_tr_t25, second_amendment_scope__collective_right_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__collective_right_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__collective_right_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement(seco_be_t5, second_amendment_scope__collective_right_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__collective_right_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(seco_be_t15, second_amendment_scope__collective_right_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__collective_right_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(seco_be_t25, second_amendment_scope__collective_right_reading, base_extractiveness, 25, 0.21).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__collective_right_reading, base_extractiveness, 30, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__collective_right_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seco_su_t5, second_amendment_scope__collective_right_reading, suppression_requirement, 5, 0.34).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__collective_right_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(seco_su_t15, second_amendment_scope__collective_right_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__collective_right_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(seco_su_t25, second_amendment_scope__collective_right_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__collective_right_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Second Amendment' decomposes into three structurally distinct constraints — one per reading of the kernel second_amendment_scope. Each reading fixes a different right-bearer (states only / individuals unconditionally / individuals conditionally on militia service), hence a different beneficiary set, a different epsilon over the same referent text, and a different regulatory-authority footprint. This file is the collective_right_reading member. Edges run from this reading to both siblings because its dominance and collapse structurally shaped their operating environments: its enforcement suppressed personal-right claims for decades, and its 2008 repudiation created the doctrinal vacancy the individual reading filled and the civic reading contests. Within any single authoritative framework the readings genuinely foreclose one another (each denies the others' right-bearer premise); across parties they coexist as live positions. Sibling files carry reciprocal links and their own epsilon referents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
