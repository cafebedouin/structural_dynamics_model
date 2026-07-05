% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__punitive_liability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__punitive_liability_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__punitive_liability_reading
 *   human_readable: Versailles Reparations Clauses — Punitive Liability Reading (Article 231 War Guilt Grounding)
 *   domain: international relations/legal history/political economy
 *
 * SUMMARY:
 *   This story instantiates the PUNITIVE LIABILITY reading of the Versailles
 *   reparations kernel: Article 231's war-guilt clause is read as grounding a
 *   claim on Germany's total war costs, not bounded by documented damage or
 *   by Germany's fiscal capacity to pay. Under this reading, the Reparations
 *   Commission's schedules and enforcement mechanisms (including the 1923
 *   Ruhr occupation following default) are the natural extension of a settled
 *   moral-legal premise: Germany caused the war and therefore owes its total
 *   cost. This is a distinct constraint from the
 *   limited_responsibility_reading (which reads Article 231 as legal
 *   boilerplate bounding claims by capacity) and the repudiation_reading
 *   (which denies binding obligation under duress). Each reading has a
 *   different ε, different victim/beneficiary structure, and different
 *   persistence logic; they are linked here only through
 *   network.affects_constraints and cs_structure.reading_relations, per the
 *   ε-invariance principle — this file does not average across them.
 *
 * KEY AGENTS:
 *   - french_state_treasury: institutional beneficiary, largest claimant, arbitrage exit via occupation threat
 *   - allied_reparations_commission_administrators: institutional agenda_setter, schedules and enforces under the total-liability premise
 *   - german_industrial_workers and german_taxpayers: powerless payers, trapped exit, bear the transfer through requisition, taxation, and inflation
 *   - keynes_and_capacity_economists: analytical excluded voice, argued the premise was self-defeating from outside the treaty apparatus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, 0.81).
domain_priors:suppression_score(versailles_reparations_clauses__punitive_liability_reading, 0.72).
domain_priors:theater_ratio(versailles_reparations_clauses__punitive_liability_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(versailles_reparations_clauses__punitive_liability_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__punitive_liability_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__punitive_liability_reading, "Versailles Reparations Clauses — Punitive Liability Reading (Article 231 War Guilt Grounding)").
narrative_ontology:topic_domain(versailles_reparations_clauses__punitive_liability_reading, "international relations/legal history/political economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__punitive_liability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__punitive_liability_reading, 'e54d319f-a01f-4f9c-b660-6a7259a34c63').
narrative_ontology:cs_kernel_codification('e54d319f-a01f-4f9c-b660-6a7259a34c63', fixed_text).
narrative_ontology:cs_authority_grounding('e54d319f-a01f-4f9c-b660-6a7259a34c63', extraction).
narrative_ontology:cs_interpretation_layer_present('e54d319f-a01f-4f9c-b660-6a7259a34c63').
narrative_ontology:cs_reading_relation('e54d319f-a01f-4f9c-b660-6a7259a34c63', versailles_reparations_clauses__limited_responsibility_reading, influences).
narrative_ontology:cs_reading_relation('e54d319f-a01f-4f9c-b660-6a7259a34c63', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('e54d319f-a01f-4f9c-b660-6a7259a34c63', foundational, war_guilt_grounds_total_liability).
narrative_ontology:cs_axiom_status(war_guilt_grounds_total_liability, holdable).
narrative_ontology:cs_axiom_grounding('e54d319f-a01f-4f9c-b660-6a7259a34c63', war_guilt_grounds_total_liability, conventional).
narrative_ontology:cs_axiom('e54d319f-a01f-4f9c-b660-6a7259a34c63', foundational, treaty_obligation_binding_regardless_of_signing_conditions).
narrative_ontology:cs_axiom_status(treaty_obligation_binding_regardless_of_signing_conditions, holdable).
narrative_ontology:cs_axiom_grounding('e54d319f-a01f-4f9c-b660-6a7259a34c63', treaty_obligation_binding_regardless_of_signing_conditions, conventional).
narrative_ontology:cs_reference_frame('e54d319f-a01f-4f9c-b660-6a7259a34c63', total_war_guilt_liability_framework).
narrative_ontology:cs_drift_state('e54d319f-a01f-4f9c-b660-6a7259a34c63', post_dawes_young_committee_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e54d319f-a01f-4f9c-b660-6a7259a34c63', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_state_treasury).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, belgian_state_treasury).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, allied_reparations_commission_administrators).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__punitive_liability_reading, french_war_reconstruction_contractors).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, weimar_state_fiscal_capacity).
narrative_ontology:constraint_victim(versailles_reparations_clauses__punitive_liability_reading, german_currency_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the largest share of reparations receipts under the Reparations Commission's allocation schedule, justified by the devastation of northern France. Under the punitive liability reading, Article 231's designation of German responsibility for the war grounds claims well beyond documented reconstruction cost, extending to pension and disability payments and open-ended future assessments. Can escalate collection through occupation (as in the Ruhr) if payments lapse.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, french_state_treasury, beneficiary,
    institutional, generational, arbitrage, continental).

% Collects a substantial secondary share, citing invasion damages. Benefits from the same war-guilt grounding used to justify claims that outstrip Belgium's own occupation-era losses.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, belgian_state_treasury, beneficiary,
    institutional, generational, arbitrage, continental).

% Sets annual payment schedules, audits German fiscal capacity, and authorizes sanctions (occupation, customs seizure) for default. Operates from the treaty's Article 231 premise that Germany's liability is not bounded by capacity to pay but by the totality of Allied war costs, giving the Commission wide discretion to revise demands upward.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, allied_reparations_commission_administrators, agenda_setter,
    institutional, generational, analytical, continental).

% Receive reparations-in-kind (coal, industrial equipment, direct labor obligations) channeled through the reconstruction program; their commercial position depends on the punitive liability framing continuing to authorize large transfers rather than a capacity-limited settlement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, french_war_reconstruction_contractors, beneficiary,
    organized, biographical, mobile, national).

% Bear the real burden through wage suppression, coal and industrial output requisitioned in kind, and the inflationary financing the Weimar state resorts to in order to meet schedules. Have no seat in the Commission and no legal channel to contest the liability premise; exit means emigration, which is costly and limited.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_industrial_workers, payer,
    powerless, biographical, trapped, national).

% Fund payments through taxation levied by a state whose fiscal sovereignty is subordinated to externally set schedules; under the punitive reading the tax burden has no principled ceiling tied to actual damage caused, only to Allied assessment of total war cost.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_taxpayers, payer,
    powerless, biographical, trapped, national).

% The German state itself operates under externally imposed budget priorities; reparations service claims a fixed first call on revenue before domestic spending, and the Commission can impose administrative and territorial sanctions if targets are missed. Diplomatic renegotiation is the only lever, and it operates entirely within a framework that treats the underlying liability as settled by Article 231.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, weimar_state_fiscal_capacity, payer,
    moderate, generational, constrained, national).

% Absorb the consequences of the state's recourse to money creation to meet reparations obligations without corresponding tax capacity, most severely during the 1923 hyperinflation triggered in part by the Ruhr occupation response to a payment default. Have no direct relationship to the treaty terms but bear a major transmission-channel cost.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, german_currency_holders, payer,
    powerless, biographical, trapped, national).

% Hold claims on Allied war debts to the United States that are structurally linked to reparations receipts but have no formal voice in setting the German liability schedule; they would argue for a capacity-based settlement that stabilizes the whole debt chain, but that argument belongs to a different reading of the kernel.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, international_bondholders_and_creditors, excluded,
    organized, biographical, constrained, continental).

% Argued from outside the Commission (most prominently in 'The Economic Consequences of the Peace') that the total-liability premise was economically self-defeating and would require crushing German capacity permanently to service; their analysis was publicly influential but structurally outside the treaty's enforcement apparatus.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__punitive_liability_reading, keynes_and_capacity_economists, excluded,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__punitive_liability_reading, french_state_treasury).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__punitive_liability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Allied creditor states a single treaty-grounded mechanism to collect compensation for war destruction and to coordinate collection among multiple claimant states without each negotiating bilaterally with Germany.
% TRANSFER_FUNCTION: Moves wealth — cash, coal, industrial capital, labor obligations — from German taxpayers, workers, and currency holders to French and Belgian state treasuries and reconstruction contractors, under a liability schedule set and revised by the Reparations Commission.
% ABSENT_VOICES: German workers and taxpayers who bear the transfer have no seat on the Commission. International bondholders whose war-debt claims are entangled with reparations flows are not party to the schedule-setting. Economists arguing for a capacity-bounded settlement (Keynes chief among them) published outside the treaty process with no binding effect on it.
% DISAPPEARANCE_RATIONALE: If the punitive liability grounding were removed, the entire schedule of open-ended assessments, the occupation sanctions used to enforce them, and the war-guilt clause's role in domestic German politics would collapse; French and Belgian reconstruction financing would have to be renegotiated on a capacity basis, and the political fuel Article 231 supplied to German revanchism would be substantially altered.
% FOUNDING_PROBLEM: Allied states needed a legal and moral basis to compel compensation for extensive war damage from Germany, and to coordinate collection among multiple claimant nations rather than negotiate separately.
% FOUNDING_PROBLEM_CORROBORATION: Allied Commission administrators and French/Belgian treasuries attest the liability remains live and proportionate to damage sustained. Independent economists writing outside the Commission (Keynes, and later historians drawing on Reichsbank and Dawes/Young Committee records) attest the punitive-liability framing outran actual reconstruction need and functioned increasingly as extraction rather than compensation; no corroboration for the punitive reading's totality claim comes from outside the beneficiary states themselves.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__punitive_liability_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__punitive_liability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__punitive_liability_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__punitive_liability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__punitive_liability_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__punitive_liability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__punitive_liability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81 by 1932, peaking at 0.85 during the 1923 Ruhr crisis) because under this reading the liability ceiling is set by total Allied war cost rather than by German capacity or documented damage — there is structurally no principled cap. Suppression is high (0.72, spiking to 0.88 in 1923) because alternatives (renegotiation on capacity grounds, unilateral German reduction) were actively foreclosed by occupation and sanction threats whenever Germany missed a schedule. Theater ratio is comparatively low-moderate (0.28) because the enforcement machinery (customs control, Ruhr occupation, Commission audits) was substantively functional extraction, not mostly performative — though its performative share grew somewhat over the Dawes-Plan renegotiation years as face-saving schedules replaced hard collection.
 *
 * PERSPECTIVAL GAP:
 *   From the Commission and creditor-treasury seats, this reading experiences as legitimate compensation collection grounded in settled war-guilt law — a rope-like coordination among Allied claimants. From the German payer seats, the same structure is enforced extraction with no capacity ceiling and no voice in setting terms — the beneficiary/victim asymmetry plus active enforcement (occupation, sanctions) is why the engine should compute this as tangled_rope or more extractive from the payer seats even though the agenda-setter seat perceives coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   French and Belgian treasuries and reconstruction contractors are beneficiaries with arbitrage-grade exit (they can escalate collection mechanisms and are not exposed to reciprocal claims) — d near the beneficiary end. German workers, taxpayers, and currency holders are trapped payers under a liability with no capacity ceiling — d near the full-target end. Weimar state fiscal capacity sits as a constrained institutional payer: it can negotiate (Dawes, Young plans) but operates entirely inside a framework that treats the underlying total-liability premise as settled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (compensating documented Allied war damage) was largely addressed by the mid-1920s reconstruction of northern France and Belgium, yet the punitive liability reading kept the payment schedule pegged to total war cost rather than to remaining reconstruction need — a classic founding-problem/persistence mismatch. The R5 status is marked contested rather than dead because the Commission and French/Belgian treasuries continued to assert the liability was still proportionate, while corroboration from outside the beneficiary states (Keynes at the time, later Dawes/Young Committee economic assessments) increasingly treated the schedule as decoupled from actual damage — exactly the founding_problem_status/disappearance_verdict mismatch the framework is designed to surface as a capture flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    war_guilt_clause_moral_vs_legal_status,
    'Does Article 231''s language establish a moral judgment of German culpability for the totality of the war, or is it a legal-liability clause whose scope is properly bounded by demonstrable damage and capacity to pay?',
    'Comparative textual and diplomatic-history analysis of the treaty drafting record (the Lansing/Sackett memoranda, the Commission on Responsibility''s findings) against how the clause was actually invoked in Commission schedule-setting versus how German negotiators and later historians characterized its scope.',
    'If the clause is properly a bounded legal-liability instrument, this punitive_liability reading is itself a constructed extraction dressed in the kernel''s legal language, and the limited_responsibility_reading would be the structurally correct instantiation. If the clause genuinely grounds unlimited moral liability, this reading''s classification (tangled_rope, high ε) stands as the accurate structural read of the actual kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(war_guilt_clause_moral_vs_legal_status, conceptual, 'Whether Article 231 grounds unlimited moral liability or bounded legal liability — the central interpretive fork between this reading and the limited_responsibility sibling.').

omega_variable(
    duress_and_treaty_legitimacy,
    'Does the fact that German negotiators had no genuine alternative to signing (occupation and continued blockade being the alternative) delegitimize the liability premise this reading relies on, independent of the clause''s textual scope?',
    'Analysis of the negotiating conditions (blockade continuation threat, exclusion from substantive negotiation, ultimatum structure of the final terms) against international-law standards for treaty validity under duress, as later invoked by the repudiation_reading.',
    'If duress is found to void the underlying obligation, this reading''s entire liability structure — and not merely its scope — loses grounding, which is the repudiation_reading''s core claim; this reading''s classification as tangled_rope presumes the obligation itself is valid and only asks how much is owed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(duress_and_treaty_legitimacy, conceptual, 'Whether treaty duress undermines the liability obligation this reading presumes valid, which is the foreclosure question against the repudiation_reading sibling.').

omega_variable(
    capacity_ceiling_versus_total_cost_administration,
    'Did the Reparations Commission''s actual schedule-setting practice (Dawes 1924, Young 1929) function as a de facto capacity ceiling despite the treaty''s nominal total-cost premise, meaning the punitive reading''s formal scope diverged from its administered scope over time?',
    'Compare the treaty''s total-liability language against the actual payment schedules negotiated in the Dawes and Young plans; if administered amounts tracked German fiscal capacity rather than total Allied war cost, the punitive reading''s formal premise was informally abandoned in practice well before Lausanne 1932.',
    'If administration converged toward capacity-bounded amounts, the punitive reading''s high-ε classification would be most accurate for the 1919-1924 period and progressively less accurate afterward — supporting the declining suppression_requirement trend after 1923 in the measurements above, and suggesting the constraint drifted toward the limited_responsibility reading''s structure without formal amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_ceiling_versus_total_cost_administration, empirical, 'Whether Commission practice diverged from the treaty''s punitive-liability text over time, blurring the boundary with the limited_responsibility sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__punitive_liability_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1921, 0.2).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1923, 0.22).
narrative_ontology:measurement(vers_tr_t1926, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1926, 0.3).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1929, 0.32).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__punitive_liability_reading, theater_ratio, 1932, 0.28).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1919, 0.6).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1921, 0.68).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1923, 0.85).
narrative_ontology:measurement(vers_be_t1926, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1926, 0.74).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1929, 0.7).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__punitive_liability_reading, base_extractiveness, 1932, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1919, 0.55).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1921, 0.62).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1923, 0.88).
narrative_ontology:measurement(vers_su_t1926, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1926, 0.7).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1929, 0.6).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__punitive_liability_reading, suppression_requirement, 1932, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__punitive_liability_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, dawes_plan_restructuring).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, young_plan_restructuring).
narrative_ontology:affects_constraint(versailles_reparations_clauses__punitive_liability_reading, weimar_hyperinflation_1923).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the versailles_reparations_clauses kernel. The limited_responsibility_reading treats Article 231 as bounded legal liability tracking German capacity (lower ε, closer to tangled_rope-with-sunset or scaffold structure). The repudiation_reading treats the entire treaty obligation as void under duress (a snare reading from the German side with no legitimate coordination function at all — pure extraction). This punitive_liability_reading treats Article 231 as grounding quasi-unlimited moral-financial liability, producing the highest ε and the strongest tangled_rope classification of the three (genuine coordination function among Allied claimants, but active enforcement with clear victims). All three are linked here rather than merged, per DP-001 ε-invariance: each has a stable, distinct ε and must not be averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__punitive_liability_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
