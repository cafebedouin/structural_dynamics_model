% ============================================================================
% CONSTRAINT STORY: exit_suppression_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exit_suppression_mechanism, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: exit_suppression_mechanism
 *   human_readable: H-2A Adverse-Effect Wage Floor Under Unmeasurable Instrument Substitution
 *   domain: political_economy/administrative_law/agricultural_labor
 *
 * SUMMARY:
 *   The H-2A guest worker program admits agricultural labor conditioned on
 *   DOL certifying that admission will not adversely affect wages of
 *   similarly-employed US workers. That test is only as real as the wage
 *   baseline it measures against. For decades, USDA's Farm Labor Survey
 *   provided a farm-sector-specific baseline, making the adverse-effect test
 *   falsifiable. USDA discontinued the FLS; DOL substituted the Occupational
 *   Employment and Wage Statistics (OEWS) survey, which structurally excludes
 *   most farm establishments from its sampling frame. The operative causal
 *   question is whether farmworker wage stagnation traces to ordinary
 *   supply/demand adjustment in a labor market with genuine surplus, or to a
 *   specific engineered mechanism: workers cannot observe an alternative wage
 *   (because the instrument measuring the 'prevailing wage' cannot see the
 *   sector it governs) and cannot act on any wage they do observe because
 *   H-2A visa status ties them to a single certified employer, foreclosing
 *   exit. This story treats exit_suppression_mechanism as the compound
 *   structural claim that unmeasurability plus employer-tied status jointly
 *   suppress wages below what a portable-status, correctly-measured baseline
 *   would yield -- distinct from and structurally prior to the underlying
 *   labor-supply story addressed elsewhere. State-line wage discontinuities
 *   at binding-vs-non-binding minimum wage borders (F9/F10) and the NAICS 111
 *   wage-growth gap relative to the private-sector average (F3/F4) are the
 *   observables that would empirically distinguish the two hypotheses.
 *
 * KEY AGENTS:
 *   - h2a_certified_employers: Primary beneficiary (institutional/arbitrage) -- collects the wage suppression via a baseline it does not set but that structurally favors it, and via workers who cannot exit mid-contract
 *   - h2a_workers: Primary target (powerless/trapped) -- bears the wage suppression, cannot observe an enforceable alternative wage, cannot change employer without risking status
 *   - us_domestic_farmworkers: Secondary target (powerless/constrained) -- the class the adverse-effect clause exists to protect, whose wages are the ostensible measurement object
 *   - dol_ofcc: Agenda-setter (institutional/analytical) -- administers certification, adopted the substitute instrument, defends the substitution as methodologically sound
 *   - usda_nass: Secondary institutional actor (institutional/constrained) -- discontinued FLS citing budget and methodological exposure, initiating the instrument gap
 *   - gao_crs: Analytical observer (institutional/analytical) -- audits the instrument substitution and its downstream wage effects from outside both benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exit_suppression_mechanism, 0.68).
domain_priors:suppression_score(exit_suppression_mechanism, 0.79).
domain_priors:theater_ratio(exit_suppression_mechanism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exit_suppression_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(exit_suppression_mechanism, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(exit_suppression_mechanism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exit_suppression_mechanism, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(exit_suppression_mechanism, resistance, 0.47).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exit_suppression_mechanism, tangled_rope).
narrative_ontology:human_readable(exit_suppression_mechanism, "H-2A Adverse-Effect Wage Floor Under Unmeasurable Instrument Substitution").
narrative_ontology:topic_domain(exit_suppression_mechanism, "political_economy/administrative_law/agricultural_labor").

domain_priors:requires_active_enforcement(exit_suppression_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exit_suppression_mechanism, 'acd929a3-6878-4235-bde9-f77e05413c08').
narrative_ontology:cs_kernel_codification('acd929a3-6878-4235-bde9-f77e05413c08', formalized).
narrative_ontology:cs_authority_grounding('acd929a3-6878-4235-bde9-f77e05413c08', extraction).
narrative_ontology:cs_interpretation_layer_present('acd929a3-6878-4235-bde9-f77e05413c08').
narrative_ontology:cs_reading_relation('acd929a3-6878-4235-bde9-f77e05413c08', exit_suppression_mechanism__bureaucratic_drift_reading, coexists_with).
narrative_ontology:cs_reading_relation('acd929a3-6878-4235-bde9-f77e05413c08', exit_suppression_mechanism__hold_up_efficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('acd929a3-6878-4235-bde9-f77e05413c08', exit_suppression_mechanism__coverage_neutral_reading, influences).
narrative_ontology:cs_axiom('acd929a3-6878-4235-bde9-f77e05413c08', foundational, measurement_instrument_constitutive_of_legal_standard).
narrative_ontology:cs_axiom_status(measurement_instrument_constitutive_of_legal_standard, holdable).
narrative_ontology:cs_axiom_grounding('acd929a3-6878-4235-bde9-f77e05413c08', measurement_instrument_constitutive_of_legal_standard, empirically_contingent).
narrative_ontology:cs_axiom('acd929a3-6878-4235-bde9-f77e05413c08', secondary, instrument_substitution_as_de_facto_repeal).
narrative_ontology:cs_axiom_status(instrument_substitution_as_de_facto_repeal, holdable).
narrative_ontology:cs_axiom_grounding('acd929a3-6878-4235-bde9-f77e05413c08', instrument_substitution_as_de_facto_repeal, conventional).
narrative_ontology:cs_reference_frame('acd929a3-6878-4235-bde9-f77e05413c08', farm_labor_survey_baseline_falsifiability).
narrative_ontology:cs_drift_state('acd929a3-6878-4235-bde9-f77e05413c08', post_oews_substitution, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('acd929a3-6878-4235-bde9-f77e05413c08', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exit_suppression_mechanism, h2a_certified_employers).
narrative_ontology:constraint_victim(exit_suppression_mechanism, h2a_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(exit_suppression_mechanism, usda_nass).
narrative_ontology:constraint_vindicates(exit_suppression_mechanism, adverse_effect_wage_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petition DOL for H-2A certification, pay the AEWR computed from OEWS data, and receive workers bound to their operation by visa status. They benefit from a wage floor that cannot be empirically shown to be adverse-effect-compliant, and from a labor supply that cannot exit mid-contract for a better-paying certified employer even if one exists nearby. They can adjust recruitment volume, timing, and which regions they certify in year to year -- a form of arbitrage unavailable to the workers they employ.
narrative_ontology:constraint_stakeholder(exit_suppression_mechanism, h2a_certified_employers, beneficiary,
    institutional, biographical, arbitrage, national).

% Travel from home countries under contracts tied to a single named employer. Cannot observe a reliable market wage for their specific occupation and locality because the instrument computing the prevailing wage structurally excludes most farm establishments. Cannot act on a better wage even if aware of one, because changing employers requires the new employer to file a new petition and often requires the worker to depart the country and re-enter -- a process most cannot afford to attempt without secured alternative employment first, which the isolation and information gap make difficult to arrange.
narrative_ontology:constraint_stakeholder(exit_suppression_mechanism, h2a_workers, payer,
    powerless, immediate, trapped, national).

% The class the adverse-effect clause exists to protect. Not directly party to H-2A certification, but their wages are the ostensible reference point the test protects. Have no formal voice in the certification process and no standing to challenge individual AEWR determinations; their interests are represented, if at all, by farmworker advocacy organizations that are not parties to certification either.
narrative_ontology:constraint_stakeholder(exit_suppression_mechanism, us_domestic_farmworkers, excluded,
    powerless, biographical, constrained, national).

% Administers H-2A certification and computes the AEWR from OEWS data following USDA's discontinuation of the Farm Labor Survey. Defends the substitution as methodologically sound and has committed, via interim final rule, to phasing farm employers into the OEWS sample. Can revise the instrument, the methodology, or the certification standard at will -- the broadest degrees of freedom of any seat in this structure.
narrative_ontology:constraint_stakeholder(exit_suppression_mechanism, dol_ofcc, agenda_setter,
    institutional, generational, analytical, national).

% Discontinued the Farm Labor Survey citing budget exposure and methodological concerns about survey design. Bears no direct cost from the AEWR's degraded measurement basis and benefits modestly from reduced survey overhead; its discretionary choice is the upstream trigger for DOL's instrument substitution, though the two agencies' actions were not necessarily coordinated.
narrative_ontology:constraint_stakeholder(exit_suppression_mechanism, usda_nass, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(exit_suppression_mechanism, usda_nass, beneficiary).

% Audits the instrument substitution's effects on wage floor accuracy from outside the benefiting employer class and the administering agencies, producing reports that can trigger congressional or judicial scrutiny of the certification process without themselves being party to it.
narrative_ontology:constraint_stakeholder(exit_suppression_mechanism, gao_crs, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exit_suppression_mechanism, h2a_certified_employers).
narrative_ontology:fixing_cost_class(exit_suppression_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A correctly-instrumented adverse-effect wage test solves a genuine hold-up/undercutting problem: without it, admitted guest workers could be used to depress wages below what a tight domestic labor market would otherwise sustain, harming both domestic farmworkers and, longer-run, program legitimacy. The certification process, when its measurement basis functions, coordinates guest-worker admission with wage protection.
% TRANSFER_FUNCTION: Moves wage income from h2a_workers (and secondarily from us_domestic_farmworkers via crowd-out) to h2a_certified_employers, mediated by a wage floor that cannot rise to reflect true sector-specific labor market tightness because the instrument computing it structurally excludes the sector, compounded by employer-tied visa status that forecloses workers from exiting toward whatever true market wage might exist.
% ABSENT_VOICES: H-2A workers themselves have no formal participatory role in AEWR-setting or in USDA's or DOL's instrument-selection decisions; they are represented, if at all, by farmworker legal aid organizations and worker centers who are not parties to certification. Rival portable-status program designs (the H-1B counterexample) are never before the agency as an alternative to the current tied-status structure.
% DISAPPEARANCE_RATIONALE: If the exit-suppression mechanism were removed overnight -- restoring a farm-specific wage measurement instrument and converting H-2A status to be portable across certified employers -- offered wages would likely converge toward the level observable at binding-state-minimum borders, employer certification volumes and recruitment practices would shift substantially, and the state-line wage discontinuity (F9/F10) this story identifies as diagnostic would narrow or disappear. Employers who currently benefit from the suppressed floor would face materially higher labor costs.
% FOUNDING_PROBLEM: Guest-worker admission programs create a structural risk that an elastic supply of admitted labor will be used to undercut wages that a tighter, protectionist domestic labor market would otherwise sustain; the INA's adverse-effect clause was built to make that undercutting legally and empirically detectable and prohibited.
% FOUNDING_PROBLEM_CORROBORATION: DOL attests the founding problem remains addressed through the OEWS-based AEWR and the IFR's remediation commitment. GAO and academic labor economists studying the FLS-to-OEWS transition (corroboration from outside both DOL and the employer class) have documented the sector-coverage gap as a real measurement discontinuity with unresolved bias-direction implications; no independent source outside DOL itself has affirmed that the current instrument fully satisfies the statutory test's original falsifiability requirement.
narrative_ontology:disappearance_verdict(exit_suppression_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(exit_suppression_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exit_suppression_mechanism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(exit_suppression_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(exit_suppression_mechanism, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exit_suppression_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exit_suppression_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exit_suppression_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily across the interval (0.42 to 0.68) tracking the years since FLS discontinuation and OEWS substitution -- the gap between the instrument's blind spot and the AEWR's continued operation as if fully informed widens as farm-sector wage data staleness compounds. Suppression is authored high and rising (0.58 to 0.79) because the mechanism's persistence depends on two compounding structural facts, not participant consent: the worker cannot observe a countervailing wage (unmeasurability) and cannot act on one even if observed (employer-tied status forecloses lateral movement without risking status termination). Theater ratio is moderate and rising (0.20 to 0.42): the certification process continues to perform statutory compliance -- DOL issues determinations, publishes AEWRs, processes applications -- while the substantive falsifiability of the underlying test has eroded, consistent with instrument_capture_reading's framing of the substitution as a procedural continuity masking substantive discontinuity. Resistance is moderate (0.47): worker resistance is structurally dampened by isolation, geographic dispersion, and status precarity, but is not negligible -- worker centers, legal aid networks, and periodic litigation (APA challenges to OEWS adoption) constitute real, if underpowered, resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (DOL), the arrangement is continuous statutory compliance using the best available federal wage instrument, with a stated remediation path (the IFR's phased inclusion of farm employers in OEWS sampling). From the payer seat (h2a_workers), the same arrangement operates as an unenforceable protection: the wage floor cannot be contested because there is no farm-specific baseline against which a worker or advocate could demonstrate adverse effect, and even a demonstrated gap does not translate into exit because status is employer-tied. The engine should compute markedly different seat classifications precisely because DOL's exit options are analytical/arbitrage (it can revise the instrument at will) while workers' exit options are trapped (visa status forecloses employer change without re-starting the entire certification and travel process, often from another country).
 *
 * DIRECTIONALITY LOGIC:
 *   h2a_certified_employers sit near the beneficiary end of directionality: they set no formal policy but structurally benefit from a wage floor that cannot rise to reflect the sector's true labor-market tightness, and from workers who cannot leave for a better-paying certified employer without risking removal. h2a_workers sit near the full-target end: they bear the suppressed wage, cannot observe an alternative (unmeasurability), and cannot act on one if somehow observed (visa lock-in) -- both suppression channels operate on the same seat simultaneously, which is the compound mechanism this story isolates from ordinary supply/demand adjustment. us_domestic_farmworkers are declared as a secondary victim class in the narrative (the clause's intended beneficiary) but are not carried in beneficiaries/victims arrays because their harm is indirect (crowd-out effects on a wage floor that no longer functions) rather than the direct extraction this constraint measures; a downstream sibling story on domestic-worker crowd-out would be the correct place to formalize that claim per the epsilon-invariance principle.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- protecting domestic agricultural wages from undercutting by an admitted guest-worker supply -- remains textually live (the statute is unrepealed) but functionally contested: DOL's authority to certify continues to be exercised as though the measurement problem the clause depends on is solved, when the instrument doing the measuring cannot see the regulated sector. This is precisely the tangled_rope signature rather than a pure snare: there IS a genuine coordination function (a real adverse-effect test, when properly instrumented, solves a real hold-up/undercutting problem that would otherwise erode domestic farm wages) AND there is asymmetric extraction riding the same structure (employers benefit from a floor that cannot rise to meet true market tightness, workers bear a wage suppressed below what accurate measurement plus portable status would yield). Classifying this as snare would erase the genuine, if degraded, coordination function the clause still nominally performs; classifying it as rope would erase the compounding of unmeasurability with visa lock-in that the instrument_capture_reading identifies as functional repeal without a vote.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coverage_gap_bias_direction,
    'Does OEWS''s structural exclusion of farm establishments from its sampling frame bias the computed AEWR downward, or is the direction of bias genuinely indeterminate given OEWS''s superior granularity and frequency relative to the discontinued Farm Labor Survey?',
    'GAO or independent econometric comparison of AEWR trajectories under FLS (historical) versus OEWS (current) for the same commodity/region cells, controlling for the IFR''s phased inclusion of farm employers in the OEWS sample.',
    'If bias is confirmed downward and structural, the instrument substitution functions as a de facto repeal mechanism supporting tangled_rope/snare classification. If bias is neutral or the gap is genuinely closing under the IFR remediation, the coverage_neutral_reading is vindicated and the constraint shifts toward ordinary regulatory transition (scaffold or rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coverage_gap_bias_direction, empirical, 'Whether OEWS''s farm-exclusion structurally biases the wage floor downward or is direction-neutral.').

omega_variable(
    intent_vs_structure_mechanism_threshold,
    'Does a mechanism require coordinated intent across DOL/USDA/employer-lobby actors to count as extraction, or does structural alignment of independently-motivated discretionary choices producing a consistent downward-biased outcome itself constitute the operative mechanism?',
    'Documentary/administrative-record review (rulemaking dockets, inter-agency correspondence, comment-period submissions) to establish whether the instrument substitution and employer-tied status restructuring were coordinated or independently arrived at.',
    'If coordinated, instrument_capture_reading is strongly supported and extraction should be scored higher; if genuinely independent (bureaucratic_drift_reading), the same observable pattern carries substantially less normative weight and the constraint may be closer to a piton (atrophied enforcement via inertia) than a tangled_rope (active engineered extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intent_vs_structure_mechanism_threshold, conceptual, 'Whether mechanism-hood requires coordinated intent or is satisfied by structural outcome alone.').

omega_variable(
    hold_up_efficiency_counterfactual,
    'Is employer-tied visa status a legitimate mechanism-design solution to a genuine contracting hold-up problem (sunk recruitment costs, judgment-proof workers), such that the wage-floor degradation is an incidental second-order cost rather than the point — or does the H-1B portability counterexample show that the hold-up problem is solvable without tying, making the tying itself the extractive lever?',
    'Comparative program analysis: does H-1B''s portable-status structure (where workers may transfer employers without re-starting the visa process) produce comparably functioning labor markets without the sunk-cost/hold-up failure hold_up_efficiency_reading predicts for a portable H-2A?',
    'If H-1B''s portability disproves the hold-up necessity claim, hold_up_efficiency_reading''s legitimation of tied status collapses and the tying reads as extraction-enabling rather than efficiency-enabling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hold_up_efficiency_counterfactual, conceptual, 'Whether employer-tied status is a necessary efficient response to a hold-up problem or an extractive lever disguised as one.').

omega_variable(
    cs_framing_kernel_choice,
    'Is the operative kernel the statutory adverse-effect clause itself (INA text, fixed), or the measurement instrument that operationalizes it (FLS/OEWS, substitutable)? The instrument_capture_reading treats the instrument as constitutive of the standard (''a legal standard requires operational measurement to be a standard at all''); a narrower textualist reading could hold the statutory guarantee stands independent of any particular measurement instrument, with instrument adequacy a separate administrative-law question (arbitrary-and-capricious review) rather than a kernel-drift question.',
    'Judicial review outcome (APA challenge to the OEWS substitution) would settle whether courts treat instrument adequacy as go/no-go for statutory compliance or as a severable procedural matter.',
    'If the kernel is the instrument itself, drift_state (codification_collapse) is the correct characterization and this story''s classification holds. If the kernel is the statutory text alone, this constraint may be better modeled as a narrower rulemaking-quality dispute with lower stakes, and the CS classification would shift toward implicit/practice rather than formalized/extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_choice, conceptual, 'Whether the contested kernel is the statutory text or the measurement instrument that operationalizes it — the two candidate framings this story''s cs_structure block must choose between.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exit_suppression_mechanism, 2019, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exit_tr_t2019, exit_suppression_mechanism, theater_ratio, 2019, 0.2).
narrative_ontology:measurement_basis(exit_tr_t2019, observed).
narrative_ontology:measurement(exit_tr_t2020, exit_suppression_mechanism, theater_ratio, 2020, 0.24).
narrative_ontology:measurement_basis(exit_tr_t2020, observed).
narrative_ontology:measurement(exit_tr_t2021, exit_suppression_mechanism, theater_ratio, 2021, 0.29).
narrative_ontology:measurement_basis(exit_tr_t2021, observed).
narrative_ontology:measurement(exit_tr_t2022, exit_suppression_mechanism, theater_ratio, 2022, 0.34).
narrative_ontology:measurement_basis(exit_tr_t2022, observed).
narrative_ontology:measurement(exit_tr_t2023, exit_suppression_mechanism, theater_ratio, 2023, 0.38).
narrative_ontology:measurement_basis(exit_tr_t2023, observed).
narrative_ontology:measurement(exit_tr_t2024, exit_suppression_mechanism, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(exit_tr_t2024, observed).
narrative_ontology:measurement(exit_tr_t2026, exit_suppression_mechanism, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(exit_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(exit_be_t2019, exit_suppression_mechanism, base_extractiveness, 2019, 0.42).
narrative_ontology:measurement_basis(exit_be_t2019, observed).
narrative_ontology:measurement(exit_be_t2020, exit_suppression_mechanism, base_extractiveness, 2020, 0.46).
narrative_ontology:measurement_basis(exit_be_t2020, observed).
narrative_ontology:measurement(exit_be_t2021, exit_suppression_mechanism, base_extractiveness, 2021, 0.51).
narrative_ontology:measurement_basis(exit_be_t2021, observed).
narrative_ontology:measurement(exit_be_t2022, exit_suppression_mechanism, base_extractiveness, 2022, 0.58).
narrative_ontology:measurement_basis(exit_be_t2022, observed).
narrative_ontology:measurement(exit_be_t2023, exit_suppression_mechanism, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement_basis(exit_be_t2023, observed).
narrative_ontology:measurement(exit_be_t2024, exit_suppression_mechanism, base_extractiveness, 2024, 0.66).
narrative_ontology:measurement_basis(exit_be_t2024, observed).
narrative_ontology:measurement(exit_be_t2026, exit_suppression_mechanism, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(exit_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(exit_su_t2019, exit_suppression_mechanism, suppression_requirement, 2019, 0.58).
narrative_ontology:measurement_basis(exit_su_t2019, observed).
narrative_ontology:measurement(exit_su_t2020, exit_suppression_mechanism, suppression_requirement, 2020, 0.61).
narrative_ontology:measurement_basis(exit_su_t2020, observed).
narrative_ontology:measurement(exit_su_t2021, exit_suppression_mechanism, suppression_requirement, 2021, 0.66).
narrative_ontology:measurement_basis(exit_su_t2021, observed).
narrative_ontology:measurement(exit_su_t2022, exit_suppression_mechanism, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement_basis(exit_su_t2022, observed).
narrative_ontology:measurement(exit_su_t2023, exit_suppression_mechanism, suppression_requirement, 2023, 0.75).
narrative_ontology:measurement_basis(exit_su_t2023, observed).
narrative_ontology:measurement(exit_su_t2024, exit_suppression_mechanism, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(exit_su_t2024, observed).
narrative_ontology:measurement(exit_su_t2026, exit_suppression_mechanism, suppression_requirement, 2026, 0.79).
narrative_ontology:measurement_basis(exit_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exit_suppression_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(exit_suppression_mechanism, h2a_employer_tied_visa_status).
narrative_ontology:affects_constraint(exit_suppression_mechanism, farm_labor_survey_discontinuation).
narrative_ontology:affects_constraint(exit_suppression_mechanism, naics_111_wage_supply_demand_baseline).

% DUAL FORMULATION NOTE:
% This story isolates the exit-suppression hypothesis (unmeasurability + visa lock-in as the operative causal mechanism) from the sibling hypothesis that observed NAICS 111 wage stagnation reflects ordinary labor-supply/demand adjustment (naics_111_wage_supply_demand_baseline, a distinct constraint with its own epsilon -- likely lower, since ordinary market adjustment is not itself an extraction mechanism). Per the epsilon-invariance principle these are NOT two measurements of one constraint: exit_suppression_mechanism claims a specific engineered suppression pathway with an identifiable beneficiary (h2a_certified_employers) and victim (h2a_workers); the supply/demand sibling claims a market-clearing process with no comparable beneficiary/victim structure. h2a_employer_tied_visa_status is authored separately to isolate the status-lock component (which this story treats as a compounding input, not the sole mechanism) from the measurement-instrument component (farm_labor_survey_discontinuation, which isolates USDA's discontinuation decision on its own administrative-process merits per the bureaucratic_drift_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exit_suppression_mechanism, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
