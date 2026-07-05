% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   state-killing-authority kernel: capital punishment justified strictly by
 *   its deterrent efficacy against future murders. Under this reading, the
 *   condemned person's death is instrumental — valuable only insofar as it
 *   changes the behavior of third parties who never appear in the courtroom.
 *   The reading stands or falls entirely on an empirical claim (does
 *   execution deter more than incarceration?) that decades of criminological
 *   research have failed to establish affirmatively, while the institutional
 *   apparatus built on the claim (capital statutes, prosecutorial charging
 *   practices, political rhetoric) persists and hardens independent of that
 *   empirical record. This is why theater_ratio rises over the measured
 *   interval: as the empirical case for deterrence weakens under scrutiny,
 *   the public and legal invocation of deterrence as justification becomes
 *   increasingly performative relative to its evidentiary support, while the
 *   machinery of capital prosecution continues to operate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.58).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.62).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '1f0fb22d-aadb-4151-95a1-75dcbbc84e88').
narrative_ontology:cs_kernel_codification('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', formalized).
narrative_ontology:cs_authority_grounding('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', lineage).
narrative_ontology:cs_interpretation_layer_present('1f0fb22d-aadb-4151-95a1-75dcbbc84e88').
narrative_ontology:cs_reading_relation('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', state_killing_authority__categorical_abolition, coexists_with).
narrative_ontology:cs_axiom('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', foundational, state_authority_conditioned_on_prevention_efficacy).
narrative_ontology:cs_axiom_status(state_authority_conditioned_on_prevention_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', state_authority_conditioned_on_prevention_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', secondary, condemned_life_instrumentally_weighable_against_future_lives).
narrative_ontology:cs_axiom_status(condemned_life_instrumentally_weighable_against_future_lives, holdable).
narrative_ontology:cs_axiom_grounding('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', condemned_life_instrumentally_weighable_against_future_lives, instrumental).
narrative_ontology:cs_reference_frame('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', consequentialist_public_safety_framework).
narrative_ontology:cs_drift_state('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', post_meta_analysis_criminology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f0fb22d-aadb-4151-95a1-75dcbbc84e88', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, deterrence_advocacy_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, tough_on_crime_politicians).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, wrongfully_convicted_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, capital_defendants_indigent).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, state_authority_grounded_in_prevention_efficacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death and used, under this reading, as the instrumental mechanism by which future murders are supposedly prevented. Their own culpability or rehabilitation potential is analytically secondary to the deterrent signal their execution sends. They have no exit: appeals exhaust, and the justification for their death rests entirely on a contested empirical claim about other people's future behavior, not on anything about them specifically beyond the underlying conviction.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, national).

% A subset of condemned persons who did not commit the crime. Under a deterrence framework, the entire justification structure gives no more scrutiny to actual guilt than any other input to the deterrence calculus — if executing an innocent person still produces a deterrent signal (or the error is undetected), the framework's internal logic does not require actual guilt to achieve its stated aim. Exonerations, when they occur, arrive after irreversible harm and rely entirely on outside forces (innocence projects, DNA evidence) never guaranteed by the arrangement itself.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, wrongfully_convicted_persons, payer,
    powerless, immediate, trapped, national).

% An unidentifiable class of people who, under this reading, are saved from future murder because would-be killers are deterred by the threat of execution. They never know they benefited; the causal claim that they exist as a class at all depends on unresolved econometric disputes about whether capital punishment deters more than long incarceration. They bear no cost and have no voice — they are a hypothesized beneficiary, not an organized one.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    moderate, generational, analytical, national).

% Face capital charges without resources for the extensive expert testimony, investigation, and appellate representation that better-resourced defendants can mount. Under the deterrence framework the disparate treatment is irrelevant to the justificatory question (does execution deter?), which means resource asymmetry in who actually gets executed is treated as noise rather than as evidence bearing on the arrangement's legitimacy.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, capital_defendants_indigent, payer,
    powerless, immediate, trapped, regional).

% Elected or appointed prosecutors who pursue and defend capital sentences, publicly justifying them on deterrence grounds. They administer charging decisions, control which cases become capital cases, and benefit professionally and electorally from framing executions as protecting the public. Their career and reelection incentives are tied to invoking deterrence rhetoric regardless of the state of the underlying econometric evidence.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, deterrence_advocacy_prosecutors, agenda_setter,
    institutional, biographical, mobile, regional).

% Legislators and executives who campaign on capital punishment as crime prevention, collecting electoral benefit from the deterrence claim's public appeal independent of whether it holds up empirically. They author and defend the statutes that keep the deterrence framework in place.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, tough_on_crime_politicians, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__deterrence_instrument, tough_on_crime_politicians, agenda_setter).

% Study whether executions measurably reduce murder rates relative to alternative sanctions. The empirical literature is genuinely contested — some studies find deterrent effects, most find none or find effects that vanish under better identification strategies. Their findings are cited selectively by both advocates and abolitionists.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminologists_deterrence_researchers, observer,
    analytical, generational, analytical, national).

% Some families of murder victims want capital punishment for reasons of desert or closure that have nothing to do with deterring future crimes — they are folded into the deterrence framework's public narrative even though their actual motivating claim belongs to the retributive reading, not this one. Their desert-based voice is analytically absent from this constraint's own justificatory logic even as it is politically invoked alongside it.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, murder_victims_families_retributive, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__deterrence_instrument, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__deterrence_instrument, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal mechanism by which the state can, in principle, reduce future murders by deterring would-be killers through the credible threat of execution — solving a genuine collective problem (reducing violent crime) if the empirical premise holds.
% TRANSFER_FUNCTION: Moves the life of the condemned person into the ledger as an instrumental cost, transferred against a hypothesized reduction in future murders experienced by an unidentifiable class of potential victims. Politically, it also transfers electoral capital to officials who invoke the deterrence claim.
% ABSENT_VOICES: Wrongfully convicted persons have no voice until post-hoc exoneration, if it happens at all. Indigent capital defendants are structurally underrepresented in the process that decides their fate. Retributive-minded victims' families are present in public discourse but their actual justificatory claim (desert, not deterrence) is absent from this reading's own logic even when their support is politically used to sustain it.
% DISAPPEARANCE_RATIONALE: If the deterrence justification collapsed (e.g., decisive evidence that executions do not reduce murder rates), the retributive and abolitionist readings would still remain live — capital punishment might persist under a different justification, or might end, depending on which reading political and legal authority defaults to. The deterrence-specific arrangement (statutes and prosecutorial practice explicitly grounded in prevention efficacy) would lose its stated foundation, but the underlying institution of capital punishment does not disappear with this one reading — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The stated problem is the persistence of murder despite existing sanctions — capital punishment as deterrence was framed as a stronger disincentive than incarceration, intended to reduce the future murder rate below what lesser sanctions would achieve.
% FOUNDING_PROBLEM_CORROBORATION: Independent criminological research (National Research Council 2012 panel, Donohue & Wolfers-style natural-experiment studies) attests that decades of empirical work have failed to establish a reliable deterrent effect distinguishable from that of long-term incarceration — i.e., outside academic bodies corroborate that the founding empirical problem this reading claims to solve remains unresolved or negative. Deterrence-invoking prosecutors and tough-on-crime politicians (the reading's own beneficiaries) continue to assert the problem is live; that assertion is not corroborated from outside the benefiting parties.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, contested).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the condemned person's life is treated as a fungible instrumental cost within the deterrence calculus, and that the class of victims who supposedly benefit is diffuse, unidentifiable, and cannot verify or contest the claim made on their behalf. Suppression (0.62) reflects the active legal and political machinery required to sustain capital sentencing (appeals exhaustion, execution protocols, resistance to abolition campaigns) — this is a raw structural property and is not scaled here; only extractiveness responds to directionality and scope in the engine's computation. Theater ratio (0.55) is elevated because the deterrence justification is increasingly disconnected from the empirical record even as it remains the publicly stated rationale — a hallmark of proxy-goal substitution. Accessibility collapse (0.40) is moderate rather than high: alternative sanctions (life without parole) are widely available and adopted in many jurisdictions, so the deterrence framework has not collapsed all alternatives, unlike a true mountain. Resistance (0.68) is high: abolitionist movements, innocence projects, and international human rights bodies actively contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (prosecutors, politicians), the arrangement reads as legitimate crime prevention grounded in state authority to protect the public. From the condemned-persons seat, the same arrangement reads as their life being spent as an instrument toward an unverified collective good they have no voice in demonstrating or contesting. The engine should compute these as structurally different experiences of the same constraint, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons and wrongfully convicted persons sit at the extreme target end: their lives are the direct cost paid by the arrangement, with no meaningful exit once sentenced. Indigent capital defendants are similarly targeted, compounded by resource asymmetry that the deterrence framework's internal logic treats as irrelevant. Potential future victims are the nominal beneficiary class, but this benefit is hypothesized rather than demonstrated and accrues to an unidentifiable population that never organizes to claim or defend it — hence gain_flow is authored as diffuse rather than naming a capturing seat. Deterrence-advocacy prosecutors and tough-on-crime politicians are the actual concentrated beneficiaries of the arrangement's persistence (career and electoral capital), which is why they are named as beneficiaries/agenda-setters even though the story's official beneficiary (future victims) never actually collects anything traceable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reducing future murders below what incarceration alone achieves — remains formally the stated justification, but independent criminological corroboration (outside the benefiting parties) finds no reliable deterrent effect distinguishable from incarceration. This is a mandatrophy signature: the founding_problem_status is authored as contested precisely because status is asserted as live by beneficiaries (prosecutors, politicians) and as unresolved-or-dead by outside researchers. Classifying this as tangled_rope rather than snare preserves the genuine (if empirically thin) coordination claim — reducing violent crime is a real collective problem — while still registering the asymmetric extraction (condemned persons pay an irreversible cost) and the requirement of active enforcement (capital sentencing and execution infrastructure) that a pure rope would not need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_reading_vs_retributive_reading,
    'Is this reading (deterrence_instrument) or the sibling retributive_desert reading the one that actually operates within real capital sentencing decisions and jury reasoning, even when deterrence rhetoric is used publicly?',
    'Analysis of jury instructions, sentencing memoranda, and prosecutorial closing arguments across capital cases to determine which justificatory logic (forward-looking prevention vs backward-looking desert) actually drives charging and sentencing decisions, versus which is merely invoked in public communication.',
    'If retributive logic actually drives sentencing while deterrence rhetoric is deployed only for public legitimation, this constraint''s claimed_type and beneficiary structure would need re-examination — the ''future victims'' beneficiary class may be rhetorical cover for a retributive practice, which would push this reading''s real operation toward the sibling constraint''s structure rather than its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_reading_vs_retributive_reading, conceptual, 'Whether deterrence or retribution is the operative (not merely stated) justification in practice.').

omega_variable(
    empirical_deterrence_effect_magnitude,
    'Does capital punishment produce a measurable marginal deterrent effect beyond life imprisonment, and if so, of what magnitude?',
    'Continued natural-experiment and panel-data criminological research using credible identification strategies (e.g., moratoria, Furman-era discontinuities) to isolate the causal effect of execution risk on murder rates.',
    'If a robust positive deterrent effect were established, the coordination-function claim underlying this reading would be substantially strengthened, potentially shifting classification toward a less extractive reading (closer to rope). If no effect is confirmed (consistent with most existing literature), the beneficiary class (potential_future_victims) is effectively empty and the reading''s justificatory foundation collapses, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_deterrence_effect_magnitude, empirical, 'The unresolved empirical status of the deterrent effect, on which this entire reading''s legitimacy depends.').

omega_variable(
    wrongful_execution_rate_uncertainty,
    'What proportion of executions under this framework have been or would be of factually innocent persons?',
    'Post-hoc exoneration studies, DNA-based re-examination of closed capital cases, and error-rate estimation methodologies applied to historical capital sentencing data.',
    'A demonstrated non-trivial wrongful-execution rate directly undermines the deterrence framework''s own internal cost-benefit logic (if the framework is meant to be justified ''at acceptable cost'', the cost side must include irreversible errors) and strengthens the case that suppression and extraction are structurally under-measured here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_rate_uncertainty, empirical, 'The unresolved rate of wrongful execution, bearing directly on the ''acceptable cost'' clause of this reading''s own justificatory formula.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__deterrence_instrument, theater_ratio, 0, 0.3).
narrative_ontology:measurement(stat_tr_t8, state_killing_authority__deterrence_instrument, theater_ratio, 8, 0.36).
narrative_ontology:measurement(stat_tr_t16, state_killing_authority__deterrence_instrument, theater_ratio, 16, 0.42).
narrative_ontology:measurement(stat_tr_t24, state_killing_authority__deterrence_instrument, theater_ratio, 24, 0.47).
narrative_ontology:measurement(stat_tr_t32, state_killing_authority__deterrence_instrument, theater_ratio, 32, 0.51).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__deterrence_instrument, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__deterrence_instrument, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t8, state_killing_authority__deterrence_instrument, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(stat_be_t16, state_killing_authority__deterrence_instrument, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(stat_be_t24, state_killing_authority__deterrence_instrument, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(stat_be_t32, state_killing_authority__deterrence_instrument, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__deterrence_instrument, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__deterrence_instrument, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t8, state_killing_authority__deterrence_instrument, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(stat_su_t16, state_killing_authority__deterrence_instrument, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(stat_su_t24, state_killing_authority__deterrence_instrument, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(stat_su_t32, state_killing_authority__deterrence_instrument, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__deterrence_instrument, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_killing_authority kernel, decomposed per the ε-invariance principle because the three readings have structurally distinct beneficiary sets, distinct empirical dependencies, and distinct failure modes. deterrence_instrument (this story) grounds legitimacy in a contingent empirical claim about crime-prevention efficacy, with potential_future_victims as a diffuse hypothesized beneficiary class. retributive_desert grounds legitimacy in backward-looking desert with no deterrence-dependent beneficiary class at all. categorical_abolition denies the legitimacy of state killing under any framework and treats the condemned as a rights-holder rather than an instrumental cost. All three are linked via affects_constraints because a shift in the dominant public/legal reading (e.g., empirical collapse of the deterrence claim) creates downstream pressure on which of the remaining readings authority defaults to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
