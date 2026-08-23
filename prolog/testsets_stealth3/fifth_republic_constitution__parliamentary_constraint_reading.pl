% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Presidential Policy Implementation Requires Legislative Authorization (Parliamentary Reading)
 *   domain: constitutional_law/comparative_politics
 *
 * SUMMARY:
 *   This file instantiates the parliamentary_constraint_reading of the Fifth
 *   Republic constitutional kernel: the president is read as a coordinated
 *   executive whose policy implementations reach effect only through
 *   legislative authorization — passage through both chambers, budget assent,
 *   and exposure to the confidence/censure machinery (Articles 20, 34, 47-50
 *   of the 1958 text). Under this reading the standing arrangement's
 *   beneficiary is the legislative majority, which holds the authorization
 *   gate and collects agenda leverage; the executive enters the victim set
 *   whenever the Assembly withholds confidence or blocks legislation. Per the
 *   epsilon-invariance principle this is one clean constraint: the sibling
 *   readings (hyper-presidential, cohabitation-equilibrium) instantiate
 *   different constraints over the same kernel text and are authored in their
 *   own files, linked here through network edges. Claim and metrics are
 *   independent authored facts: claimed_type is tangled_rope (genuine
 *   coordination — democratic authorization — plus asymmetric payment through
 *   the same gate, actively enforced), while the metrics are authored from
 *   this reading's own lights (OQ-26): extraction from the governed is
 *   reading-indexed low because the tolls the executive pays are priced by
 *   this reading as the legitimate cost of authorization rather than captured
 *   rent, even in the high-toll fragmentation era of 2022-2025.
 *
 * KEY AGENTS:
 *   - national_assembly_majority: Primary beneficiary (organized/constrained) — holds the authorization gate; collects agenda leverage, amendments, budget priorities, and the credible censure threat
 *   - president_and_government: Primary target (institutional/constrained) — originates policy but cannot implement without passing the gate; enters the victim set whenever confidence is withheld or legislation blocked
 *   - upper_house_senate: Secondary beneficiary (organized/constrained) — shares the passage gate; amendment and delay leverage without confidence rights
 *   - parliamentary_minorities: Secondary beneficiary (organized/constrained) — collects procedural footholds and episodic coalition wins (December 2024)
 *   - french_citizenry: Secondary beneficiary (moderate/mobile) — collects the accountability dividend; pays in delayed reforms
 *   - conseil_constitutionnel: Analytical observer (institutional/analytical) — certifies that authorization traffic respects the constitutional rules
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.31).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.55).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Presidential Policy Implementation Requires Legislative Authorization (Parliamentary Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/comparative_politics").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '9efa44f5-ef4c-4cfe-8697-68e3bee5cc98').
narrative_ontology:cs_kernel_codification('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', fixed_text).
narrative_ontology:cs_authority_grounding('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', lineage).
narrative_ontology:cs_interpretation_layer_present('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98').
narrative_ontology:cs_reading_relation('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', fifth_republic_constitution__hyper_presidential_reading, forecloses).
narrative_ontology:cs_reading_relation('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', foundational, implementation_requires_legislative_assent).
narrative_ontology:cs_axiom_status(implementation_requires_legislative_assent, holdable).
narrative_ontology:cs_axiom_grounding('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', implementation_requires_legislative_assent, deontological).
narrative_ontology:cs_axiom('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', secondary, government_responsible_before_assembly).
narrative_ontology:cs_axiom_status(government_responsible_before_assembly, holdable).
narrative_ontology:cs_axiom_grounding('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', government_responsible_before_assembly, conventional).
narrative_ontology:cs_reference_frame('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', parliamentary_authorization_regime).
narrative_ontology:cs_drift_state('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', contemporary_fragmentation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9efa44f5-ef4c-4cfe-8697-68e3bee5cc98', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, upper_house_senate).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_minorities).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, french_citizenry).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_and_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authorization gate: no presidential policy program reaches implementation until it secures this bloc's votes in the Palais Bourbon. Collects agenda leverage in exchange for assent — amendments accepted, budget priorities reshaped, committee control, and the credible threat of censure. Its grip is electoral and perishable: a failed dissolving gamble (1962, 1988, 1997, 2024) can hand the gate to opponents, and every deputy faces re-election within five years, so receipts are recycled rather than banked.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, beneficiary,
    organized, biographical, constrained, national).

% Sets the national policy agenda and originates most legislation, but cannot carry proposals into effect without passing the authorization gate. When the Assembly aligns, the toll is light: smooth passage, shared credit, orderly programming. When the Assembly withholds confidence or blocks legislation, the executive absorbs the costs — abandoned programs, amended budgets, repeated engagement-of-responsibility gambles, and ultimately the risk of a censure motion toppling the government, as in December 2024. Exit is poor: the decree path is closed, dissolving the Assembly risks replacing a hostile majority with a worse one, and resignation forfeits the mandate outright.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president_and_government, payer,
    institutional, biographical, constrained, national).

% Elects both the president and the Assembly and collects the accountability dividend: policy takes effect only after elected representatives assent, so responsibility for outcomes stays attributable and contestable at every election. Pays indirectly when authorization delays slow reforms it wants. Holds a real but seldom-used exit: freedom of movement within the EU lowers the cost of leaving the jurisdiction entirely.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, french_citizenry, beneficiary,
    moderate, generational, mobile, national).

% Shares the passage gate: legislation must clear both chambers, giving senators amendment leverage and delay rights, softened only by the Government's ability to give the final word to the National Assembly under defined conditions. Indirectly elected, longer-lived, and impossible to dissolve, it converts participation in authorization into slower but steadier receipts. It cannot grant or withhold confidence — that lever sits with the National Assembly alone.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, upper_house_senate, beneficiary,
    organized, biographical, constrained, national).

% Loses most votes but collects procedural rents: amendment tables, commission inquiries, oversight hearings, and high-visibility leadership of censure motions. In fragmented assemblies these footholds appreciate sharply — the December 2024 censure succeeded only because normally rival minorities coalitioned once around a shared target. Receipts are episodic and reputation-based rather than programmatic.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_minorities, beneficiary,
    organized, biographical, constrained, national).

% Certifies that legislative traffic respects the constitutional rules of the gate — reviewing organic laws, referral procedures, and contested uses of the engagement of responsibility. Adjudicates boundary cases such as ordinance-enabling authorizations without administering the gate itself. Its pronouncements shape what counts as authorized, but it collects no policy rents.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, conseil_constitutionnel, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of concentrating policy initiative in a singly-elected executive: by requiring implementation to pass the authorization gate, it converts unilateral presidential preference into policy that a working majority of elected representatives has assented to, and gives that majority a continuing reason to cohere.
% TRANSFER_FUNCTION: Moves agenda control and policy-shaping concessions from the president and government to the legislative majority — amendments accepted, budgets reshaped, programs deferred — and, when confidence is formally withheld, moves office-survival risk from the majority onto the government: a successful censure motion ends it, as in December 2024.
% ABSENT_VOICES: Voters who backed the president's platform but sit outside the Assembly majority: their preferred policies die at the gate with no recourse until the next election, and their objection is voiced only from outside the chamber — in media, streets, and polls. Also absent: future cohorts who inherit commitments contracted when thin majorities traded long-term obligations for short-term authorization.
% DISAPPEARANCE_RATIONALE: If the authorization requirement vanished overnight, the executive would implement policy along the decree path, the Assembly's gatekeeping role and the majority's leverage would evaporate, ministerial responsibility would hollow into ceremony, and the 2022-2025 pattern of blocked budgets and censured governments — the machinery's most visible recent work — would have been impossible.
% FOUNDING_PROBLEM: The chronic instability of the Third and Fourth Republics, where fragmented coalition assemblies toppled governments within months: the 1958 designers built the confidence and responsibility machinery so that a coherent government could form, govern, and be answerable — stability first, answerability second.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: constitutional historiography of the 1958 drafting (the comite consultatif constitutionnel record and the Conseil constitutionnel's own anniversary commentaries) and comparative-politics scholarship attribute the design to cabinet instability, not executive restraint; no serious current actor claims monthly cabinet collapses recur. The corroboration deliberately cuts against this reading's emphasis — the founding texts aimed to strengthen government against the Assembly, which is precisely why the founding problem is dead while the arrangement lives on adopted functions.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.31: the referent is the standing authorization arrangement assessed by this reading's lights — the tolls (amendments, budget discipline, program abandonment, censure risk) are authorization costs that recycle into policy assent rather than rents captured by any seat, hence reading-indexed low even though the executive's 2022-2025 burden is behaviorally heavy; the hyper-presidential sibling file is expected to author markedly higher epsilon over the same referent, and that cross-file divergence is the corpus signal. Suppression 0.52: the unilateral-implementation path is structurally foreclosed for the executive seat, and the confidence/censure/budget machinery is in its most active phase since 1962; suppression is authored raw and unscaled — the engine scales only extractiveness. Theater 0.14: authorization traffic is presently maximally substantive — every vote contested, amendments decisive, the December 2024 censure real. Accessibility_collapse 0.30: alternatives do not collapse — negotiation, revision, coalition-building, and timing strategies remain open; the gate structures the option space rather than closing it. Resistance 0.45: presidents recurrently test the boundary (dissolution gambits 1962/1988/1997/2024, referendum appeals, engagement-of-responsibility pressure), within bounded cost. CYCLICAL PATTERN: all three series oscillate with electoral-alignment arithmetic — theater high and enforcement dormant under aligned majorities (1958-1968, 2002-2017), theater low and enforcement hot under cohabitation and fragmentation (1986-1997, 2022-2025); at least two full cycles appear on the shared twelve-point grid. The oscillation is exogenous — driven by electoral shocks, not intermittent reinforcement manufactured by the constraint itself. Metrics were measured at the interval end, the fragmentation trough: enforcement high, theater low, extraction at its reading-indexed ceiling. Coalition note: enforcement depends on episodic minority coalition; the December 2024 censure succeeded only because rival blocs coalitioned once — coalition power is real but unsustained.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently by construction. From the assembly-majority seat the gate is the guarantee of its agenda relevance — coordination it welcomes. From the executive seat the same gate is an enforced toll that turns acute exactly when confidence is withheld. The citizenry seat collects the accountability dividend and prices delays as minor. Same-level lateral dynamic: the assembly majority and the parliamentary minorities hold identical power atoms and identical exit options, yet collect different receipts — agenda control versus procedural footholds — differentiated purely by coalition position, which is why minority receipts are episodic and reputation-based. The engine computes this per-seat divergence from the authored structural data; the story-level claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: national_assembly_majority, upper_house_senate, parliamentary_minorities, and french_citizenry are declared beneficiaries (low d, damped effective extraction); president_and_government is declared victim with constrained exit (high d, amplified effective extraction — near the full-target end because the decree path is closed and dissolution is self-defeating). The citizenry's EU mobility pushes it furthest toward the beneficiary end. No directionality_overrides were authored: derivation from the declarations plus exit atoms reproduces the true relationships, and the executive's legitimacy-side offset (shared credit under aligned majorities) is qualitative and diffuse — too weak to warrant a numeric correction keyed to a power atom shared with the observer seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic cabinet instability — is dead: no actor claims monthly collapses recur, and corroboration comes from outside the beneficiary set. The arrangement nonetheless persists and rearranges the world (world_rearranges), so the R5 mismatch flag (dead status crossed with a rearranging verdict) is expected and honest here. Cross-checking against the theater path resolves it away from zombie decay: theater_ratio sits at its series minimum (0.14), the enforcement machinery demonstrably fired (December 2024 censure), and the successor function — binding presidential unilateralism to elected assent — is live (see omega successor_function_liveness). mandatrophy_resolved is authored true for the founding mandate specifically: the anti-instability mandate is spent and the structure persists on an adopted function. The classification guards both error directions: calling this a pure rope would erase the executive's enforced payments through the gate; calling it a snare would erase the genuine authorization coordination that even the paying seat values; calling it a piton would contradict the live enforcement record. The residual honesty obligation is temporal: if enforcement decays and theater rises in future intervals, this story should be re-authored toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates one reading — the parliamentary_constraint_reading — of the kernel fifth_republic_constitution. What structural deltas would the sibling readings introduce over the same referent?',
    'Cross-read the sibling files (fifth_republic_constitution__hyper_presidential_reading, fifth_republic_constitution__cohabitation_equilibrium_reading), each authored on the shared referent: the hyper-presidential sibling flips the executive into the beneficiary seat and the Assembly into the target seat, authoring high extraction and reframing the same enforcement machinery as usurpation; the cohabitation sibling splits seats by phase.',
    'Classification is reading-relative by design (OQ-26): resolving the contest does not merge the files — each sibling keeps its own epsilon over the shared referent, and cross-file comparison is the intended consumption of this constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is one reading of a contested constitutional kernel; sibling readings instantiate different constraints over the same text.').

omega_variable(
    fortynine_three_membership,
    'Does Article 49.3 — passage of a bill without an Assembly vote unless a censure motion succeeds within the rescission window — belong INSIDE this reading''s authorization constraint (as a defective enforcement mode) or OUTSIDE it (as property of the hyper-presidential sibling''s constraint)?',
    'Conceptual test: whether rescindability-by-censure preserves ''authorization'' in this reading''s sense; empirical assist: if successful censure against engaged-responsibility passages becomes routine, 49.3 converges on ordinary authorization and belongs inside.',
    'Inside: this file''s suppression and theater absorb 49.3''s mechanical-passage character. Outside: this file''s epsilon excludes it and the sibling file must carry it, moving the family split accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fortynine_three_membership, conceptual, 'Boundary ambiguity over which constitutional instrument belongs to which sibling constraint.').

omega_variable(
    censure_capacity_restoration,
    'Is the December 2024 successful censure (toppling the Barnier government, the first since 1962) a durable restoration of the confidence machinery or a one-off artifact of extreme fragmentation?',
    'Track censure success frequency, budget-passage mode (ordinary versus engaged-responsibility), and government survival spans across 2025-2030.',
    'Durable restoration strengthens certification of the enforcement layer as functional, supporting the coordination half of the hybrid; a one-off leaves enforcement contingent and episodic, keeping the gate''s bite dependent on fragmentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(censure_capacity_restoration, empirical, 'Whether the confidence machinery''s demonstrated capacity is structural or conjunctural.').

omega_variable(
    minority_coalition_durability,
    'Can the Assembly''s fragmented minorities convert episodic gate-coalitions (December 2024) into durable capture of the authorization machinery, or does electoral recycling keep every seat''s receipts transient?',
    'Observe whether any bloc sustains decisive agenda control across two or more complete budget cycles without electoral renewal.',
    'Durable capture would move gain_flow from ''diffuse'' toward a named seat and raise reading-indexed extractiveness; transient receipts keep the affirmative diffuse claim standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_coalition_durability, empirical, 'Whether the diffuse-gain assertion survives observation of minority coalition behavior.').

omega_variable(
    successor_function_liveness,
    'Is the arrangement''s post-1962 persistence explained by a dead founding mandate (inertial retention) or by a live successor function — binding presidential unilateralism to elected assent?',
    'Counterfactual probe: if presidential unilateral implementation were legally enabled tomorrow, would restoration demand arise from outside the executive? The December 2024 censure — carried by otherwise rival parties — is a live positive observation.',
    'A live successor function confirms the coordination half is real and blocks inertial reclassification despite the dead founding problem; a dead successor function would flip the story toward the inertial type at the next re-authoring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(successor_function_liveness, conceptual, 'Whether persistence runs on adopted function or spent mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.3).
narrative_ontology:measurement(fift_tr_t1962, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(fift_tr_t1968, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1968, 0.35).
narrative_ontology:measurement(fift_tr_t1978, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1978, 0.32).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1993, 0.16).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1997, 0.13).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement(fift_tr_t2017, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.12).
narrative_ontology:measurement(fift_tr_t2025, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2025, 0.14).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.15).
narrative_ontology:measurement(fift_be_t1962, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1962, 0.22).
narrative_ontology:measurement(fift_be_t1968, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1968, 0.14).
narrative_ontology:measurement(fift_be_t1978, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1978, 0.15).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.26).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1993, 0.25).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1997, 0.28).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2002, 0.13).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2012, 0.15).
narrative_ontology:measurement(fift_be_t2017, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2017, 0.16).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.33).
narrative_ontology:measurement(fift_be_t2025, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2025, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(fift_su_t1962, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1962, 0.45).
narrative_ontology:measurement(fift_su_t1968, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1968, 0.15).
narrative_ontology:measurement(fift_su_t1978, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1993, 0.38).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1997, 0.42).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2002, 0.16).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2012, 0.18).
narrative_ontology:measurement(fift_su_t2017, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2017, 0.22).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement(fift_su_t2025, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Fifth Republic constitution': one kernel text, three structurally distinct constraints (one per declared reading), each with its own epsilon, beneficiary/victim structure, and classification, linked by network edges rather than averaged into one story. Downstream/upstream structure: the parliamentary reading's authorization gate is cited by the cohabitation reading as the mechanism that makes negotiated equilibria necessary, and is rejected by the hyper-presidential reading as an illegitimate restraint on the direct mandate; cross-reading epsilon divergence over the shared referent is the measurement this family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
