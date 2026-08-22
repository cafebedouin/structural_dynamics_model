% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Clauses — Capacity-Bounded (Limited Responsibility) Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the limited-responsibility reading of the
 *   Versailles reparations kernel: the view, advanced principally by German
 *   negotiators and increasingly accepted by Anglo-American financial
 *   interests through the 1920s, that Article 231 functions as a legal
 *   predicate establishing jurisdiction for claims rather than a moral
 *   verdict of German war guilt, and that any resulting payment obligation
 *   must be bounded by demonstrated German economic capacity rather than by
 *   Allied assessments of total damage incurred. Under this reading, the 1921
 *   London Schedule's nominal 132 billion gold marks was never the operative
 *   constraint — actual transfers were repeatedly rescheduled downward (Dawes
 *   1924, Young 1929) as capacity arguments prevailed in successive revision
 *   rounds, culminating in the effective termination of payments at Lausanne
 *   in 1932. The sibling readings — punitive_liability_reading (Germany owes
 *   total war costs, Article 231 as moral-financial foundation) and
 *   repudiation_reading (the treaty is void as extracted under duress) — are
 *   NOT part of this story; they are separate constraints with their own ε
 *   and stakeholder structures, linked here only via
 *   cs_structure.reading_relations and network edges.
 *
 * KEY AGENTS:
 *   - german_negotiating_delegation: agenda-setting/beneficiary seat that authors and repeatedly wins the capacity-bounding argument
 *   - german_industrial_elites: beneficiary — retains industrial capital that would be targeted under a punitive reading
 *   - anglo_american_creditor_banks: beneficiary — loan flows depend on German solvency being preserved
 *   - french_reconstruction_authorities: primary payer/victim — reconstruction costs exceed what capacity-bounded schedules deliver
 *   - belgian_occupied_territories: payer/victim — similarly under-compensated relative to damage incurred
 *   - allied_war_pensioners: powerless payer/victim — real value of pensions erodes across successive downward revisions
 *   - economic_historians: analytical observer of the contest over whether capacity limits were genuine or constructed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.42).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.38).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Clauses — Capacity-Bounded (Limited Responsibility) Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, '1b2a275c-10f5-43a8-a007-1fb212133d38').
narrative_ontology:cs_kernel_codification('1b2a275c-10f5-43a8-a007-1fb212133d38', fixed_text).
narrative_ontology:cs_authority_grounding('1b2a275c-10f5-43a8-a007-1fb212133d38', lineage).
narrative_ontology:cs_interpretation_layer_present('1b2a275c-10f5-43a8-a007-1fb212133d38').
narrative_ontology:cs_reading_relation('1b2a275c-10f5-43a8-a007-1fb212133d38', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b2a275c-10f5-43a8-a007-1fb212133d38', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('1b2a275c-10f5-43a8-a007-1fb212133d38', foundational, article_231_is_jurisdictional_not_moral).
narrative_ontology:cs_axiom_status(article_231_is_jurisdictional_not_moral, holdable).
narrative_ontology:cs_axiom_grounding('1b2a275c-10f5-43a8-a007-1fb212133d38', article_231_is_jurisdictional_not_moral, conventional).
narrative_ontology:cs_axiom('1b2a275c-10f5-43a8-a007-1fb212133d38', foundational, obligation_bounded_by_demonstrated_capacity).
narrative_ontology:cs_axiom_status(obligation_bounded_by_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('1b2a275c-10f5-43a8-a007-1fb212133d38', obligation_bounded_by_demonstrated_capacity, instrumental).
narrative_ontology:cs_reference_frame('1b2a275c-10f5-43a8-a007-1fb212133d38', treaty_text_as_negotiated_ceiling).
narrative_ontology:cs_drift_state('1b2a275c-10f5-43a8-a007-1fb212133d38', post_dawes_young_revision_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b2a275c-10f5-43a8-a007-1fb212133d38', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_negotiating_delegation).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, anglo_american_creditor_banks).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_reconstruction_authorities).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, belgian_occupied_territories).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_war_pensioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argues within treaty negotiations and successive revision conferences (London Schedule, Dawes, Young) that Article 231 is a formal jurisdictional predicate for claims, not an admission of war guilt, and that any payment schedule must be capped by demonstrated German fiscal and export capacity. Uses capacity arguments to repeatedly renegotiate schedules downward and to build domestic political capital around resisting 'unlimited' liability.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_negotiating_delegation, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_negotiating_delegation, beneficiary).

% Benefit directly when payment schedules are pegged to capacity rather than to Allied claimed war costs — capacity assessments are contestable and can be shaped through lobbying, currency management, and strategic underinvestment in exportable surplus. Retain capital and industrial base that a punitive-liability reading would have targeted for transfer or dismantlement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites, beneficiary,
    powerful, generational, arbitrage, national).

% Extend loans (Dawes, Young Plan bond issues) premised on German capacity to service both reparations and private debt; benefit from the capacity-bounded framing because it stabilizes German solvency enough to keep debt service flowing to them, while French claims are subordinated to that solvency logic.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, anglo_american_creditor_banks, beneficiary,
    institutional, generational, mobile, global).

% Bear the direct cost of reduced and repeatedly rescheduled payments needed to rebuild the devastated northern departments. Cannot unilaterally enforce larger transfers without Anglo-American diplomatic and financial cooperation, and are structurally out-argued once 'capacity' becomes the accepted metric rather than 'damage incurred.' Their 1923 Ruhr occupation was itself an attempt to break out of this constraint and was walked back.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_reconstruction_authorities, payer,
    powerful, biographical, constrained, national).

% Suffered direct wartime destruction and occupation costs; compensation claims are processed through the same capacity-bounded schedule as French claims and are similarly reduced and deferred across revision rounds, with no independent leverage to force a different standard.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, belgian_occupied_territories, payer,
    moderate, biographical, trapped, regional).

% War widows, disabled veterans, and dependents whose pension claims were folded into reparations totals under Article 232's pensions clause; receive a diminishing real share as headline reparations figures are revised downward across the 1920s, with no seat in the revision negotiations themselves.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_war_pensioners, payer,
    powerless, biographical, trapped, national).

% Assess (Keynes onward) whether the capacity constraint reflected genuine fiscal limits or was itself a negotiating construct shaped by whichever side controlled the underlying capacity estimates; their retrospective analyses feed later legitimacy contests over both this reading and its rivals.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formally bounded, economically grounded standard for setting reparations payments so that claims can be revised through expert schedules (Dawes 1924, Young 1929) rather than through repeated unilateral coercive enforcement (e.g., renewed occupation), stabilizing the German currency and the broader European credit system on which Allied recovery finance also depended.
% TRANSFER_FUNCTION: Moves the effective magnitude of compensation downward from the levels Allied damage assessments would have justified, redistributing the shortfall's burden onto French and Belgian reconstruction budgets and onto Allied pensioners, while preserving German industrial capital and channeling new Anglo-American loan capital into Germany in place of direct reparations transfer.
% ABSENT_VOICES: Devastated-zone civilian claimants in France and Belgium, and rank-and-file pensioners, had no direct seat at the Dawes or Young negotiating tables — those were conducted among finance ministries, central bankers, and German industrial representatives; the capacity assessments that determined their compensation were produced by experts appointed largely through Anglo-American and German channels.
% DISAPPEARANCE_RATIONALE: If the capacity-bounded standard were abandoned in favor of unrestricted liability enforcement, Germany's currency and industrial base would face renewed occupation-style enforcement pressure (as briefly happened in the Ruhr), Anglo-American loan flows underwriting German solvency would likely halt, and French/Belgian claims would either be pursued through coercion or written off entirely — the entire 1920s international credit architecture built around German capacity to pay would need to reorganize.
% FOUNDING_PROBLEM: Allied powers needed a mechanism to extract compensation for wartime damage from a defeated Germany without either bankrupting German industry outright (destabilizing European trade and credit) or triggering renewed continental conflict through unenforceable maximalist demands.
% FOUNDING_PROBLEM_CORROBORATION: German negotiators and Anglo-American bankers attest the capacity problem was genuinely binding — Germany could not pay what was demanded without currency collapse (borne out by the 1923 hyperinflation). French reconstruction officials and independent economic historians (e.g., later revisionist work questioning Keynes's own capacity estimates) attest that capacity figures were themselves contested and politically shaped, meaning the 'founding problem' as a fixed fact is itself part of what this reading constructs rather than discovers.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42, declining over the interval (0.55 to 0.30) as successive revisions (Dawes, Young, Lausanne) reduce the effective transfer — capturing this reading's own internal claim that the arrangement was progressively 'rightsizing' toward true capacity, while still registering as extractive relative to French/Belgian damage claims that were never fully honored. Theater ratio rises through the mid-1920s (0.20 to 0.34) as expert commissions (Dawes committee, Young committee) increasingly perform technical neutrality around figures that remained politically negotiated, then falls as the arrangement is wound down at Lausanne. Suppression is moderate-to-declining, spiking around the 1923 Ruhr occupation (France's attempt to coercively break the capacity-bounded framing) and falling thereafter as the standard becomes normalized and less contested.
 *
 * DIRECTIONALITY LOGIC:
 *   German negotiators and industrial elites sit near the beneficiary end: the capacity standard directly caps what can be extracted from them, and they have institutional/arbitrage-grade capacity to shape how 'capacity' itself is measured. Anglo-American banks are structural beneficiaries at one remove — their loan exposure benefits from German solvency being prioritized over French compensation. French and Belgian authorities and Allied pensioners sit near the target end: the same standard that protects German capacity is the mechanism reducing what reaches them, and their exit options are constrained or trapped (France could occupy the Ruhr but had to withdraw; pensioners have no exit at all).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (extracting compensation without destroying the paying economy) had genuine force circa 1919-1923 given real German fiscal distress and hyperinflation. By the late 1920s the arrangement's persistence increasingly served a different function: legitimizing progressively smaller transfers to French/Belgian claimants and pensioners while stabilizing an Anglo-American-German credit relationship. Classifying this as tangled_rope rather than a clean rope captures that the capacity-bounding function was real (coordination: avoiding renewed continental economic collapse) while the same mechanism was also extractive toward the uncompensated victims — both were true simultaneously, which is exactly the ambiguity the tangled_rope category exists to hold rather than force into a pure-coordination or pure-extraction verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_measurement_construction,
    'Were German capacity assessments (Dawes 1924, Young 1929) objective technical measurements of fiscal reality, or were they themselves negotiated political constructs shaped disproportionately by German and Anglo-American financial interests?',
    'Comparative analysis of independent contemporary economic estimates (e.g., French counter-assessments, later historiographic reconstructions of German fiscal capacity) against the figures adopted by the Dawes and Young committees, checking whether committee composition and methodology systematically favored lower capacity estimates.',
    'If capacity figures were substantially constructed rather than discovered, the limited-responsibility reading''s core legitimacy claim weakens considerably and the arrangement looks more like negotiated extraction-avoidance by the beneficiary coalition than a neutral economic standard — pushing the classification further toward snare-adjacent tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_measurement_construction, empirical, 'Whether the capacity standard was objectively measured or politically constructed by beneficiary interests.').

omega_variable(
    article_231_legal_vs_moral_character,
    'Is Article 231''s function as a jurisdictional predicate for claims genuinely separable from a moral attribution of war guilt, or does the legal formality inescapably carry moral content given its drafting history and Allied public reception?',
    'Analysis of the treaty drafting records, contemporaneous German and Allied public reception, and subsequent invocation of Article 231 in domestic political discourse on both sides.',
    'If the legal/moral separation cannot be sustained, this reading''s foundational axiom is substantially weakened, strengthening the punitive_liability_reading''s claim that this is definitional evasion rather than legitimate limitation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_231_legal_vs_moral_character, conceptual, 'Whether Article 231''s legal-formality framing is a coherent separation from moral judgment or a rhetorical evasion.').

omega_variable(
    revision_process_representativeness,
    'Did the Dawes/Young revision processes adequately represent French, Belgian, and pensioner interests, or were those parties structurally out-negotiated by German-Anglo-American financial coordination?',
    'Examination of delegation composition, voting/influence structures within the Reparations Commission and successor committees, and outcome tracking of French/Belgian objections raised and rejected.',
    'Confirms or weakens the victim-group designation for French/Belgian authorities and pensioners, and bears on whether requires_active_enforcement should be read as coercive suppression of legitimate claims versus orderly renegotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revision_process_representativeness, empirical, 'Whether the revision process structurally disadvantaged claimant parties relative to the German-Anglo-American beneficiary coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 1919, 1932).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(vers_tr_t1921, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1921, 0.24).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1923, 0.28).
narrative_ontology:measurement(vers_tr_t1924, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1924, 0.32).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1929, 0.34).
narrative_ontology:measurement(vers_tr_t1932, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 1932, 0.3).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1919, 0.55).
narrative_ontology:measurement(vers_be_t1921, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1921, 0.5).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1923, 0.48).
narrative_ontology:measurement(vers_be_t1924, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1924, 0.44).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1929, 0.38).
narrative_ontology:measurement(vers_be_t1932, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 1932, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1919, 0.5).
narrative_ontology:measurement(vers_su_t1921, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1921, 0.45).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1923, 0.6).
narrative_ontology:measurement(vers_su_t1924, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1924, 0.4).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1929, 0.32).
narrative_ontology:measurement(vers_su_t1932, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 1932, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the Versailles reparations question,' per the ε-invariance principle: punitive_liability_reading treats Article 231 as grounding near-unlimited moral-financial liability (high ε, victims = German taxpayers/industrial base); limited_responsibility_reading (this story) treats Article 231 as formal jurisdictional predicate bounded by capacity (moderate, declining ε, victims = Allied claimants); repudiation_reading treats the treaty as void for duress (near-zero legitimate ε, victims = none under its own logic, contested by all other readings). Each reading has a distinct beneficiary/victim structure and a distinct ε trajectory; they are linked as a kernel family rather than merged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
