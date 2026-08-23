% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Capital Punishment Regime — Categorical Abolition Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   The standing arrangement under contest is the capital-punishment regime
 *   of retentionist jurisdictions: statutes designating capital crimes,
 *   prosecutorial charging for death, appellate review, clemency machinery,
 *   and scheduled executions. This story instantiates ONE reading of the
 *   state_killing_authority kernel — the categorical_abolition reading — and
 *   per the fixed epsilon-referent rule, epsilon is authored for THAT
 *   standing arrangement as this reading sees it, never for the abolitionist
 *   alternative the reading endorses. On this reading the condemned remain
 *   full rights-holders (life is inalienable; no crime forfeits it), so every
 *   execution removes a rights-holder from the community; the state itself
 *   enters the potential-violator set whenever it kills; the arrangement's
 *   gains land as career and electoral capital with prosecutors and
 *   officeholders and as promised closure for a subset of bereaved families,
 *   while anti-execution family members are pushed out of the official
 *   conversation. The claim/metric pair is authored independently:
 *   claimed_type states what this reading holds structurally true; the
 *   metrics describe how the arrangement actually operates over 1976–2026.
 *   Sibling readings are separate files linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   condemned_prisoners: primary target (powerless/trapped) — bear the
 *   arrangement's terminal burden; remain in the rights-holder set under this
 *   reading - wrongfully_condemned_defendants: secondary target
 *   (powerless/trapped) — bear the full sentence without the conduct the
 *   other readings' predicates require - condemned_prisoners_families:
 *   secondary target (powerless/trapped) — bear loss, witnessing, and stigma
 *   - prosecutorial_offices: primary beneficiary and case-level administrator
 *   (institutional/arbitrage) — convert death sentences into career and
 *   electoral capital; curate which victims' voices are heard -
 *   retentionist_political_officeholders: agenda setter
 *   (institutional/arbitrage) — maintain statutes, sign or stay warrants,
 *   harvest punitive sentiment - pro_execution_victim_family_members:
 *   beneficiary (moderate/constrained) — receive the arrangement's promised
 *   finality - abolitionist_victim_family_members: excluded voice
 *   (moderate/constrained) — oppose the killing of their kin's killers and
 *   are sidelined by the enforcing offices - human_rights_monitors:
 *   analytical observer (organized/analytical) — document error rates and
 *   procedures without enforcement power
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.9).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.7).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Capital Punishment Regime — Categorical Abolition Reading").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, 'a1a2f056-a715-4972-b1d9-615b393b6bf3').
narrative_ontology:cs_kernel_codification('a1a2f056-a715-4972-b1d9-615b393b6bf3', formalized).
narrative_ontology:cs_authority_grounding('a1a2f056-a715-4972-b1d9-615b393b6bf3', lineage).
narrative_ontology:cs_interpretation_layer_present('a1a2f056-a715-4972-b1d9-615b393b6bf3').
narrative_ontology:cs_reading_relation('a1a2f056-a715-4972-b1d9-615b393b6bf3', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('a1a2f056-a715-4972-b1d9-615b393b6bf3', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('a1a2f056-a715-4972-b1d9-615b393b6bf3', foundational, life_inalienable_against_all_crime_and_consequence).
narrative_ontology:cs_axiom_status(life_inalienable_against_all_crime_and_consequence, holdable).
narrative_ontology:cs_axiom_grounding('a1a2f056-a715-4972-b1d9-615b393b6bf3', life_inalienable_against_all_crime_and_consequence, deontological).
narrative_ontology:cs_axiom('a1a2f056-a715-4972-b1d9-615b393b6bf3', secondary, executing_state_enters_violator_set).
narrative_ontology:cs_axiom_status(executing_state_enters_violator_set, holdable).
narrative_ontology:cs_axiom_grounding('a1a2f056-a715-4972-b1d9-615b393b6bf3', executing_state_enters_violator_set, deontological).
narrative_ontology:cs_reference_frame('a1a2f056-a715-4972-b1d9-615b393b6bf3', universal_inalienable_life_prohibition).
narrative_ontology:cs_drift_state('a1a2f056-a715-4972-b1d9-615b393b6bf3', contemporary_partial_retention_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1a2f056-a715-4972-b1d9-615b393b6bf3', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, prosecutorial_offices).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retentionist_political_officeholders).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, pro_execution_victim_family_members).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_prisoners).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, wrongfully_condemned_defendants).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_prisoners_families).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, sovereign_monopoly_of_lethal_sanction).
narrative_ontology:constraint_vindicates(state_killing_authority__categorical_abolition, retributive_finality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sentenced to death under retained capital statutes; confined on death rows under special-security conditions. Appeal routes are narrowed by procedural-default rules and habeas restrictions; release depends on clemency decisions made by the same offices that sought the sentence, or on exonerations that typically arrive through outside journalism and forensic luck rather than the review machinery itself. Execution ends their situation permanently and irreversibly.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_prisoners, payer,
    powerless, biographical, trapped, national).

% Convicted and death-sentenced for acts they did not commit; identified mainly by chance — investigative journalism, DNA access won after long litigation, dying witnesses' confessions — and some cleared only after execution. They carry the full weight of the sentence without the conduct the arrangement's other justifications describe.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, wrongfully_condemned_defendants, payer,
    powerless, biographical, trapped, national).

% Parents, children, and spouses of the condemned; they endure decades of scheduled execution dates and stays, attend executions as spectators when dates hold, and carry associated stigma in their communities. Anti-execution members of these families report being steered away from victim-impact proceedings, whose speaking slots go to kin who support the sentence.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_prisoners_families, payer,
    powerless, biographical, trapped, national).

% District and state attorneys who charge capital crimes, argue for death at sentencing, and defend sentences through appeals. Capital prosecutions are career landmarks and campaign assets, and offices publicize execution counts. Charging discretion lets the same office bypass the penalty when a defendant is prominent or cooperative. Kin who oppose execution report finding their statements left out of the victim-impact presentations these offices assemble.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, prosecutorial_offices, beneficiary,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__categorical_abolition, prosecutorial_offices, agenda_setter).

% Legislators and governors who enact, preserve, or expand capital statutes, sign death warrants, grant or deny clemency, and appoint pardon boards. The issue reliably mobilizes punitive-sentiment voting blocs; supporting abolition carries perceived electoral risk while supporting the arrangement carries almost none for them personally. They can amend or suspend the statutes at any session — the decision sits entirely with them.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retentionist_political_officeholders, agenda_setter,
    institutional, biographical, arbitrage, national).

% Bereaved kin who support the death sentence for their relative's killer; they testify at clemency hearings, appear at executions, and receive the arrangement's central human promise — that the killing will bring completion. Their testimony anchors the official claim that the arrangement serves victims.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, pro_execution_victim_family_members, beneficiary,
    moderate, biographical, constrained, national).

% Bereaved kin who oppose execution of their relative's killer and organize through victims'-family networks against the arrangement. Prosecutors and press treat their position as exceptional; they are left off victim-impact rosters and clemency panels, and their opposition is framed as a betrayal of the dead.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victim_family_members, excluded,
    moderate, biographical, constrained, national).

% Treaty bodies, NGOs, and academic centers that track executions, error rates, execution methods, and racial patterns; they publish reports, litigate before international bodies, and lobby legislatures. They hold no enforcement power inside retentionist jurisdictions; their leverage is documentation and diplomatic pressure.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, human_rights_monitors, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, prosecutorial_offices).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically replaced private vengeance and lynch law with a state-monopolized, procedure-bound channel for the community's demand for ultimate sanction; in the standing arrangement it supplies a terminal sanction tier, finality rituals for a subset of bereaved families, and a demonstrative display of sovereign resolve. Stated without evaluation; this reading records that the channel exists while denying that anything legitimate flows out of it.
% TRANSFER_FUNCTION: Moves the lives of condemned prisoners irreversibly out of the community into state disposal; converts punitive public sentiment into career and electoral capital flowing to prosecutors and officeholders; moves public funds into a capital-litigation apparatus costing multiples of life-imprisonment alternatives; delivers promised closure to a subset of bereaved families.
% ABSENT_VOICES: Abolitionist members of victims' families — present in the world but excluded from official proceedings and press framing by the enforcing offices; the executed themselves, whose testimony the arrangement permanently destroys; wrongfully condemned persons discovered too late; and the home communities of the condemned. Part of this absence is produced by the arrangement itself: execution deletes the strongest witness.
% DISAPPEARANCE_RATIONALE: Prosecutorial career ladders, tough-on-crime electoral coalitions, death-row housing and execution protocols, victim-services closure pipelines, and the capital-defense legal niche all depend on the arrangement. Overnight removal would force reorganization around life-without-parole sentences, redirect criminal-justice careers and campaign rhetoric, and strand families waiting on promised finality — while homicide trials and imprisonment themselves proceed unchanged.
% FOUNDING_PROBLEM: Consolidating the community's retaliatory impulse under state monopoly: ending blood feud, private vendetta, and lynch law by reserving the ultimate sanction to a procedure-bound authority — later supplemented by the aims of deterring the gravest crimes and expressing maximal condemnation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by penal historiography documenting the vengeance-monopolization transition (private vengeance and public spectacle giving way to administrative procedure) and by abolition scholarship. Status is contested: retentionist governments and pro-execution advocates attest that the gravest-crime-response problem remains live, while criminological literature from outside the benefiting parties finds that secure lifelong incapacitation dissolved the original problem and that the deterrent supplement is empirically unsupported.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.9, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.90) because on this reading the arrangement's output is the irreversible removal of persons who remain rights-holders — the strongest form of taking the framework recognizes — and the measurement series shows accumulation (0.70 to 0.90) as eligible-offense expansions, federalization layers, and political intensification stacked onto the base practice without removing earlier layers. Suppression is 0.70: persistence rests on narrowing review (procedural default, habeas restriction), clemency scarcity, and the political marginalization of anti-execution voices, including victims' own kin. Theater_ratio (0.65, rising from 0.30) tracks the growing share of activity that performs justice rather than delivering it: closure rhetoric that outcome studies do not confirm, ceremonial gravity around a shrinking number of executions, and defensive maintenance of the institution as its functional justifications erode. Accessibility_collapse is moderate (0.5) because the alternative — permanent incapacitation — exists and is in active use; what collapses is the political viability of choosing it, not its existence. Resistance is high (0.7): abolition movements, innocence infrastructure, treaty pressure, and a multi-decade contraction of executions constitute sustained, partly successful opposition. The suppression_requirement series is authored deliberately (the story traces enforcement-capacity change): it climbs to a 2000 peak reflecting the hardening of federal review limits in the late 1990s, then gently declines as executions contract and DNA-access concessions were forced — enforcement machinery applied less intensely over a shrinking practice, with residual pockets hardening. All three series share one seven-point grid (1976–2026, endpoints matching interval.start/end) so no metric borrows another's timeline. Claim/metric independence is preserved: the snare claim comes from the categorical-abolition seat; the engine computes per-seat types and may well land some seats in tangled_rope territory given the declared coordination function — the seam between snare and tangled_rope is exactly where this kernel's dispute lives, and that divergence is data, not error. Receipt surface: gain_flow names prosecutorial_offices because their situation demonstrably accrues the arrangement's ongoing returns (career-defining wins, campaign assets, control of victim-voice curation); officeholders take episodic electoral rents and pro-execution kin take symbolic returns, but the continuous flow lands with prosecutors — so 'diffuse' would be a false universal negative. fixing_cost is 'cheap': repeal or moratorium is a single legislative act or executive order, already demonstrated in multiple jurisdictions; what blocks fixing is the captured beneficiary structure, not engineering difficulty — and under the receipt-surface cell semantics, a named-seat capture stays snare-flavored under either cost class.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter and beneficiary seats the arrangement presents as lawful ultimate sanction, career reward, and responsive government; from the payer seats the identical structure presents as irreversible removal of rights-holders under review processes the enforcers themselves gatekeep; from the excluded seat it presents as a machine that curates which grieving voices count. Pro-execution kin experience promise; abolitionist kin experience erasure; the same hearing room hosts both. The engine derives these divergent per-seat classifications from the authored structural data — power, exit, and role — not from the story-level claim, and the divergence between the payer-side computation and the agenda-setter-side computation is the measurable content of this kernel's dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The three payer seats sit near the full-target end: condemned and wrongfully condemned prisoners are physically and legally trapped (exit only through clemency or exoneration granted by the enforcing side), and their families cannot exit the relationship at all — trapped targets amplify effective extraction toward its ceiling. The two institutional seats sit near the beneficiary end with arbitrage-grade exit: prosecutors and officeholders administer the arrangement, can reshape or decline it at will, and collect its returns — derivation damps their effective extraction accordingly. Pro-execution family members derive a low-to-moderate d: they receive a genuine symbolic subsidy but with constrained exit (publicly committed grief identities are costly to reverse). Abolitionist family members are not coordinated at all — they are excluded, and their exclusion is maintained by the same offices that distribute the arrangement's benefits, which is why they are seated as excluded rather than as a second victim class: their objection is real, but the structural fact is removal from the conversation. Human-rights monitors occupy the analytical seat: no chi stake, documentation only. No directionality overrides are needed — the beneficiary/victim declarations plus exit atoms produce the correct spread without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — monopolizing retaliatory violence that had been privately administered — is historically genuine and largely dissolved: secure lifelong incapacitation now performs the protective function the ultimate sanction once monopolized, and the deterrent supplement is empirically unsupported. What persists is expressive demand and captured enforcement, which places the story in mismatch-flag territory: founding_problem_status 'contested' crossed with disappearance_verdict 'world_rearranges'. Unlike a piton, however, this arrangement has concentrated, self-interested beneficiaries who actively maintain it — prosecutorial career structures and punitive electoral coalitions — so inertia-plus-performance is not the right diagnosis; the extraction has living collectors, which is the snare signature. The classification discipline cuts both ways: the vengeance-channeling pedigree must not launder present-day extraction as necessary coordination (the coordination function recorded in Q3 is descriptive, not a warrant), and abolitionist revulsion must not erase the real historical coordination achievement the founding problem documents. Reading the arrangement through its founding problem alone would produce a rope verdict; reading it through present operation alone would miss why it was built; the battery forces both into view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_killing_kernel_reading_indexicality,
    'This story instantiates the categorical_abolition reading of the state_killing_authority kernel; would the sibling readings (retributive_desert, deterrence_instrument) classify the same underlying arrangement differently?',
    'Generate the sibling stories and compare per-seat classifications: retributive_desert removes the condemned from the rights-holder set (shrinking the victim class to the wrongfully condemned); deterrence_instrument conditions the arrangement''s warrant on the measured prevention record.',
    'Under retributive_desert the arrangement could compute as coordination serving desert; under deterrence_instrument its classification rides on the empirical deterrence literature; only the categorical reading yields the victim structure and high extraction authored here — the classification is reading-indexed over a fixed referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_killing_kernel_reading_indexicality, conceptual, 'Committer structure: this constraint is the categorical_abolition reading of a three-reading kernel; siblings are separate files.').

omega_variable(
    wrongful_conviction_rate,
    'What fraction of death sentences are imposed on persons who did not commit the crime?',
    'Matched follow-up cohorts of death-row populations (the leading estimate is roughly four percent) plus systematic tracking of posthumous exonerations and near-miss discoveries.',
    'Each percentage point of error converts part of the arrangement''s output into taking of unambiguous non-guilty rights-holders, raising effective extraction and strengthening this reading''s irreversibility argument; a near-zero validated rate would weaken the wrongfully-condemned victim class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_conviction_rate, empirical, 'Error rate inside the standing arrangement''s output.').

omega_variable(
    deterrence_effect_nullity,
    'Does the standing arrangement prevent homicides relative to permanent-imprisonment alternatives?',
    'Panel and natural-experiment studies comparing homicide trajectories across adjacent jurisdictions that differ in execution practice (the existing literature finds null or bracing effects).',
    'A confirmed null strips the arrangement''s consequentialist cover, leaving purely expressive and career-political function — pushing theater_ratio upward and hardening the snare profile; a robust positive effect would give the sibling deterrence_instrument reading live structural footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effect_nullity, empirical, 'Whether the arrangement''s preventive claim has empirical content.').

omega_variable(
    victim_closure_benefit_authenticity,
    'Does execution deliver durable psychological benefit to pro-execution family members — the arrangement''s principal claimed human return?',
    'Longitudinal studies of bereaved families stratified by outcome (execution obtained vs. life sentence), which currently find no lasting advantage for the execution cohort and reported harm for some.',
    'If the closure return fails to materialize, the pro-execution family seat loses beneficiary legitimacy, concentrating the arrangement''s gains further in prosecutorial and political seats and sharpening the capture picture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_closure_benefit_authenticity, empirical, 'Whether the promised return to supporting kin is real or rhetorical.').

omega_variable(
    suppression_composition_structural_internalized,
    'Is the measured suppression of anti-arrangement positions chiefly structural (statutes narrowing review, clemency scarcity, exclusion of dissenting kin from proceedings) or internalized (officeholders'' anticipatory electoral fear, chilling of dissent, self-censorship by officials)?',
    'Cross-jurisdiction comparison holding crime profiles constant while political cultures differ, plus post-abolition trajectories: rapid normalization after repeal indicates a large internalized share; continued litigation battles indicate a structural share requiring statutory dismantling.',
    'A predominantly internalized composition predicts fast suppression decay once the political frame breaks; a predominantly structural composition means the arrangement persists until statutes and review rules are physically changed, extending the enforcement timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_composition_structural_internalized, empirical, 'Composition of the arrangement''s suppressive force between built barriers and absorbed fear.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 1976, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ska_cat_abol_tr_t1976, state_killing_authority__categorical_abolition, theater_ratio, 1976, 0.3).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t1976, observed).
narrative_ontology:measurement(ska_cat_abol_tr_t1984, state_killing_authority__categorical_abolition, theater_ratio, 1984, 0.36).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t1984, observed).
narrative_ontology:measurement(ska_cat_abol_tr_t1992, state_killing_authority__categorical_abolition, theater_ratio, 1992, 0.44).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t1992, observed).
narrative_ontology:measurement(ska_cat_abol_tr_t2000, state_killing_authority__categorical_abolition, theater_ratio, 2000, 0.52).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t2000, observed).
narrative_ontology:measurement(ska_cat_abol_tr_t2008, state_killing_authority__categorical_abolition, theater_ratio, 2008, 0.58).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t2008, observed).
narrative_ontology:measurement(ska_cat_abol_tr_t2016, state_killing_authority__categorical_abolition, theater_ratio, 2016, 0.62).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t2016, observed).
narrative_ontology:measurement(ska_cat_abol_tr_t2026, state_killing_authority__categorical_abolition, theater_ratio, 2026, 0.65).
narrative_ontology:measurement_basis(ska_cat_abol_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(ska_cat_abol_be_t1976, state_killing_authority__categorical_abolition, base_extractiveness, 1976, 0.7).
narrative_ontology:measurement_basis(ska_cat_abol_be_t1976, observed).
narrative_ontology:measurement(ska_cat_abol_be_t1984, state_killing_authority__categorical_abolition, base_extractiveness, 1984, 0.76).
narrative_ontology:measurement_basis(ska_cat_abol_be_t1984, observed).
narrative_ontology:measurement(ska_cat_abol_be_t1992, state_killing_authority__categorical_abolition, base_extractiveness, 1992, 0.8).
narrative_ontology:measurement_basis(ska_cat_abol_be_t1992, observed).
narrative_ontology:measurement(ska_cat_abol_be_t2000, state_killing_authority__categorical_abolition, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement_basis(ska_cat_abol_be_t2000, observed).
narrative_ontology:measurement(ska_cat_abol_be_t2008, state_killing_authority__categorical_abolition, base_extractiveness, 2008, 0.87).
narrative_ontology:measurement_basis(ska_cat_abol_be_t2008, observed).
narrative_ontology:measurement(ska_cat_abol_be_t2016, state_killing_authority__categorical_abolition, base_extractiveness, 2016, 0.89).
narrative_ontology:measurement_basis(ska_cat_abol_be_t2016, observed).
narrative_ontology:measurement(ska_cat_abol_be_t2026, state_killing_authority__categorical_abolition, base_extractiveness, 2026, 0.9).
narrative_ontology:measurement_basis(ska_cat_abol_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(ska_cat_abol_su_t1976, state_killing_authority__categorical_abolition, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement_basis(ska_cat_abol_su_t1976, observed).
narrative_ontology:measurement(ska_cat_abol_su_t1984, state_killing_authority__categorical_abolition, suppression_requirement, 1984, 0.58).
narrative_ontology:measurement_basis(ska_cat_abol_su_t1984, observed).
narrative_ontology:measurement(ska_cat_abol_su_t1992, state_killing_authority__categorical_abolition, suppression_requirement, 1992, 0.66).
narrative_ontology:measurement_basis(ska_cat_abol_su_t1992, observed).
narrative_ontology:measurement(ska_cat_abol_su_t2000, state_killing_authority__categorical_abolition, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement_basis(ska_cat_abol_su_t2000, observed).
narrative_ontology:measurement(ska_cat_abol_su_t2008, state_killing_authority__categorical_abolition, suppression_requirement, 2008, 0.73).
narrative_ontology:measurement_basis(ska_cat_abol_su_t2008, observed).
narrative_ontology:measurement(ska_cat_abol_su_t2016, state_killing_authority__categorical_abolition, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement_basis(ska_cat_abol_su_t2016, observed).
narrative_ontology:measurement(ska_cat_abol_su_t2026, state_killing_authority__categorical_abolition, suppression_requirement, 2026, 0.7).
narrative_ontology:measurement_basis(ska_cat_abol_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_killing_authority__deterrence_instrument).

% DUAL FORMULATION NOTE:
% Kernel decomposition: the colloquial label 'the death penalty debate' covers one kernel — state_killing_authority — instantiated as three structurally distinct constraints. retributive_desert is the traditional upstream reading (forfeiture of the right to life; cited to ground the arrangement), deterrence_instrument is the downstream empirical reading (conditional on measured prevention), and categorical_abolition (this file) rejects both warrants outright. The readings differ in victim-set composition (does the condemned remain a rights-holder?), in epsilon referent assessment, and in failure modes (desert miscalibration vs. deterrence null results vs. irreversibility under error). Family members are linked via affects_constraints; each is authored as a separate file per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
