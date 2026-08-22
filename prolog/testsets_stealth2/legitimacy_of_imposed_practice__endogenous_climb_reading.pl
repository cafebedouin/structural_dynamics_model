% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Internalization Requirement for Practice Displacement (Endogenous-Climb Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A modernizing state decrees a new calendar and a mandated dress; the
 *   countryside keeps the lunar feast days and the old headwear for decades;
 *   the cities adopt partially, through merchants, teachers, and returning
 *   soldiers, while households retain the old forms in private. This story
 *   instantiates the endogenous_climb_reading of the
 *   legitimacy_of_imposed_practice kernel: the claim that practice
 *   displacement requires internalization and fails without bottom-up
 *   adoption pathways. Referent discipline: epsilon is authored over the
 *   standing arrangement under contest - the decree-enforcement regimes of
 *   the two anchor episodes (French Republican Calendar, 1793-1805, with
 *   rural Sunday/lunar observance persisting for decades after abolition; Hat
 *   Law, 1925, with urban adoption via diffusion and rural private retention)
 *   - as this reading assesses that arrangement, never over the
 *   internalized-practice arrangement the reading would endorse (which would
 *   score near zero by construction). Assumptions stated: the measurement
 *   interval pools both episodes on a common years-since-decree axis (see
 *   omega pooled_axis_comparability); the two episodes are treated as
 *   instances of one arrangement type. Claim/metric independence:
 *   claimed_type=mountain records this reading's presentation of the
 *   internalization requirement as a structural feature of commitment
 *   formation (emerges_naturally=true); the metrics record the decree
 *   regime's actual operation - decaying enforcement, rising ceremony - and
 *   the divergence between the claim and the metric profile is the
 *   measurement the corpus exists to take, not an error to reconcile.
 *   Beneficiary declaration on a mountain is intentional false-summit
 *   authoring: the omega naturality_vs_constructed_shelter documents the
 *   natural-law-versus-constructed ambiguity the schema requires. Family:
 *   linked to the exogenous_override and hybrid_scaffolding sibling stories
 *   via network.affects_constraints. KEY AGENTS (by structural relationship):
 *   - local_practice_communities: Primary beneficiary
 *   (moderate/identity_locked) - retain lunar observance and customary dress;
 *   outlast every enforcement campaign - urban_diffusion_intermediaries:
 *   Secondary beneficiary (moderate/mobile) - carry and supply the adopted
 *   forms along the urban pathway - state_modernization_programs: Primary
 *   target and agenda-setter (institutional/constrained) - ministries whose
 *   decrees fail; absorb the wasted budgets and slipped timelines -
 *   decree_enforcement_officials: Payer-administrator
 *   (institutional/constrained) - enforce unenforceable rules; comply
 *   publicly, retain privately - religious_authorities: Excluded seat
 *   (organized/trapped) - custodians of the liturgical lunar calendar, barred
 *   from the drafting rooms - first_generation_adopters: Symmetric switchers
 *   (powerless/identity_locked) - the young and state-employed caught between
 *   filial and state practice - comparative_historians: Analytical observer
 *   (analytical/analytical) - see the full family of readings and the pooled
 *   record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.46).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.38).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, mountain).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Internalization Requirement for Practice Displacement (Endogenous-Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '409f0ce5-e267-4150-ab05-1f4cb6b6bb9a').
narrative_ontology:cs_kernel_codification('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', distributed).
narrative_ontology:cs_authority_grounding('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', diffuse_epistemic).
narrative_ontology:cs_reading_relation('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', foundational, internalization_precedes_durable_displacement).
narrative_ontology:cs_axiom_status(internalization_precedes_durable_displacement, holdable).
narrative_ontology:cs_axiom_grounding('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', internalization_precedes_durable_displacement, empirically_contingent).
narrative_ontology:cs_axiom('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', secondary, endogenous_uptake_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(endogenous_uptake_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', endogenous_uptake_constitutes_legitimacy, conventional).
narrative_ontology:cs_reference_frame('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', internalized_practice_baseline).
narrative_ontology:cs_drift_state('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', contemporary_counterexample_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('409f0ce5-e267-4150-ab05-1f4cb6b6bb9a', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, local_practice_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_diffusion_intermediaries).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_programs).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, decree_enforcement_officials).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, first_generation_adopters).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, local_practice_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, first_generation_adopters).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, internalization_necessity_thesis).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, gradualism_over_decree_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Village and small-town households that kept the old ritual calendar and customary dress through the enforcement years: fined and monitored when nonconformity was policed, left alone as enforcement relaxed, and still observing lunar feast days and older dress privately generations after the decrees. Leaving was never on offer - the practice was the community's inherited order - so they hid it rather than dropped it, and outlasted every campaign aimed at them.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, local_practice_communities, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, local_practice_communities, payer).

% Merchants, tailors, schoolteachers, and demobilized soldiers who moved between cities and countryside carrying the new calendar's market rhythm and the new dress. They supplied the adopted forms, profited from the demand the mandates created, and formed the pathway through which change traveled when it traveled at all. Their position let them leave a district where moods turned.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_diffusion_intermediaries, beneficiary,
    moderate, biographical, mobile, national).

% The ministries and reform bureaus behind the calendar and dress decrees. They drafted the mandates, scheduled the festivals, and reported adoption statistics upward. Each failed campaign consumed budgets and ministerial credibility, and the timeline they had promised their political principals slipped by decades. Changing course meant admitting the program's premise was wrong, which successive governments declined to do.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_programs, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_programs, payer).

% Prefects, police, inspectors, and local registrars tasked with making the decrees bite. They levied the fines and logged the infractions, but many came from the same districts they policed: the record shows officials wearing the mandated hat in the office and the old cap at home, keeping the official decade at the desk and the old Sabbath in the pew. Administering rules they privately broke cost them standing in both worlds.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, decree_enforcement_officials, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, decree_enforcement_officials, agenda_setter).

% Custodians of the liturgical lunar calendar and of dress customs tied to worship. Formally shut out of the reform process - their institutions disestablished or sidelined where the decrees ran - they continued publishing ritual calendars and instructing communities, which is where much of the retained practice was anchored. They had no seat in the drafting rooms and no way to abandon the calendar they kept.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, religious_authorities, excluded,
    organized, generational, trapped, national).

% Students, clerks, conscripts, and railway employees - the young and the state-employed who wore the mandated dress and used the official calendar at work while their families kept the old ways at home. Adoption bought them jobs and standing; refusal cost them both. They paid the switching costs in both directions and belonged fully to neither practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, first_generation_adopters, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, first_generation_adopters, beneficiary).

% Scholars comparing displacement episodes across regimes and centuries. They compiled the adoption statistics, the enforcement ledgers, and the retention evidence, and they sit outside every camp the episodes created - employed by neither ministry nor congregation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, local_practice_communities).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the durability problem of cultural reform: routes intended practice change through voluntary uptake pathways - markets, schools, kin networks, returning migrants - so that changed practice survives the enforcement episode, and concentrates reform effort where uptake is self-sustaining rather than supervised.
% TRANSFER_FUNCTION: Moves visible compliance from subject populations to the decree state in the short run (fines paid, mandated dress worn, official calendar used), while moving durable practice-change capacity to diffusion intermediaries and retaining communities; the net long-run transfer runs from state modernization budgets and enforcement credibility to communal autonomy.
% ABSENT_VOICES: Holders of the displaced practices - village elders, women managing household ritual calendars, clerics - were objects of policy, not participants in it; their objection (that the decrees attacked worship and inheritance) enters the record only as 'resistance to be overcome.' Exogenous-program theorists were likewise absent from the post-failure autopsies: no institutional forum compared decree designs, so the failure was attributed to the necessity thesis by default rather than by test.
% DISAPPEARANCE_RATIONALE: If the internalization requirement ceased to bind overnight - if decree could install practice directly - modernization programs would skip the diffusion decades, enforcement apparatuses would dissolve into ordinary administration, diffusion intermediaries would lose their gatekeeping premium, and retaining communities would face a conversion they currently outwait. The entire institutional division of labor around cultural reform presupposes the requirement.
% FOUNDING_PROBLEM: Post-revolutionary and post-imperial states needed cultural uniformity and a visible break with discredited orders faster than generational replacement allowed; decree displacement (a new calendar, a mandated dress) was built to compress that transition into years.
% FOUNDING_PROBLEM_CORROBORATION: Period diplomatic correspondence and later comparative historiography of peasant integration - conducted outside any beneficiary camp - attest both that the displacement problem was real and that the decree instruments failed. Heir modernization programs dispute that the problem is dead, citing continuing integration projects; the retaining communities never attested the problem as their own, having experienced it purely as imposition.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: the decree regimes took real resources from both sides - fines, monitoring burdens, and surrendered legitimacy from subject populations; budgets, administrative credibility, and a slipped modernization timeline from the state - while delivering little durable displacement; the burden decays as enforcement is abandoned (series 0.66 to 0.46). Suppression 0.38, authored as a raw structural property and never scaled by the engine: dress and calendar nonconformity was punishable where enforcement reached, but enforcement was geographically thin and formally wound down within years; the residual is legal-framework persistence and episodic revival. Theater_ratio 0.60: as displacement failed, enforcement converted into ceremony - official decade festivals nobody kept, mandated hats worn at the desk and shed at home - so a majority of observable compliance activity became performance; the rising series (0.28 to 0.60) is Goodhart drift of the enforcement proxy away from the displacement function. Accessibility_collapse 0.64: once the failure record is accepted, the quick-displacement family of strategies collapses (repeated decree campaigns demonstrably return the same result), leaving the slow diffusion path - but the collapse is incomplete because the sibling readings dispute that the quick path is closed and because political demand for speed keeps regenerating attempted shortcuts. Resistance 0.42: the requirement meets defiance chiefly from modernizing elites and doctrine-builders who reject the necessity thesis and relaunch imposition campaigns, not from the communities it shelters. All three tracked metrics share one seven-point grid (years since decree: 0, 5, 10, 15, 20, 25, 30); suppression_requirement is authored as a declining series because the story's dynamic is enforcement decay, not stable suppression.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From state_modernization_programs the requirement presents as an immovable barrier that defeated every instrument tried against it - the closest thing to natural law in the political record. From local_practice_communities the same regularity is shelter: it converts stubbornness into inevitability and prices enforcement out. decree_enforcement_officials experience a futility machine that splits the public and private self - administering by day what they practiced by night - a different phenomenology from either barrier or shelter. first_generation_adopters sit near symmetric: adoption purchased access and cost belonging in the same act. The engine computes these per-seat classifications from the structural data; the authored mountain claim belongs to the reading's voice, not to any seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (local_practice_communities, urban_diffusion_intermediaries) drive low d for those seats; victim declarations (state_modernization_programs, decree_enforcement_officials) drive high d. first_generation_adopters are deliberately undeclared in the structural arrays and should land near symmetric - their situation trades access against belonging in both directions. One override: the organized power atom is pinned to d=0.22 for religious_authorities, the story's only organized agent, because as an excluded seat they receive no structural derivation from the beneficiary/victim arrays and would otherwise sit at the canonical fallback mid-value; structurally they sit near the beneficiary end - the requirement is what preserved the liturgical order they could not abandon. Spatial scopes are national/regional: verification of compliance was locally observable, which moderates the scope amplification the engine applies to effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy cuts both ways here. Read as eternal natural law, the requirement becomes immune to its own counterexample record - the drift_state below records a substantial, unacknowledged axiom_overriding challenge from rapid-adoption episodes the necessity thesis does not cover - and a doctrine whose founding problem (emergency-speed displacement) has receded persists with a rising theater ratio: the corpus's dead-problem-plus-world_rearranges mismatch should be watched on this family. Read as mere obstruction, the requirement's genuine coordination function disappears: routing durable change through voluntary uptake is the only mechanism in the record that produced change which survived its own enforcement episode. The resolution this story encodes: classify by operative record (decaying enforcement, rising ceremony, decaying extraction) rather than by self-presentation; the mountain claim stands or falls on the counterexample search, not on the reading's insistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_shelter,
    'Is the internalization requirement a genuine invariant of commitment formation, or a constructed regularity whose ''necessity'' shelters identifiable actors (retaining communities, gradualist doctrines) from displacement pressure?',
    'Systematic search for rapid-displacement counterexamples (wartime norm shifts, Meiji-era institutional adoption, postwar institutional transplants) where decree-scale imposition stuck without prior internalization; meta-analysis of displacement episode outcomes conditioned on enforcement magnitude and time horizon.',
    'Robust counterexample classes would break the necessity premise, collapse the natural-law treatment (false-summit reclassification away from mountain), and rehabilitate the exogenous and hybrid programs; confirmation would entrench this reading''s foreclosure of decree-sufficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_shelter, empirical, 'Whether the necessity thesis is a natural law of commitment or an interest-sheltering construct.').

omega_variable(
    kernel_reading_position,
    'This constraint is the endogenous_climb_reading of kernel legitimacy_of_imposed_practice; how would the exogenous_override_reading and hybrid_scaffolding_reading restructure the beneficiary/victim map and epsilon of the same episodes?',
    'Author the sibling stories and compare computed per-seat classifications over the shared referent. The disagreement is located in the causal locus of displacement failure: internal commitment deficit (this reading) versus insufficient enforcement magnitude (exogenous) versus missing reinforcement layer (hybrid).',
    'Under exogenous_override the state becomes the beneficiary seat and reticent populations the target, flipping directionality across every seat; under hybrid_scaffolding beneficiaries and victims split between the mandate layer and the messaging layer. Family-level classification is unstable across readings by design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a three-reading kernel.').

omega_variable(
    private_retention_ambiguity,
    'Does public compliance with private retention (the mandated hat worn in the office and the old cap at home; official calendar use in the market and lunar observance in ritual) evidence incomplete internalization confirming this reading, or strategic dual compliance that would ripen into full adoption under sustained credible enforcement?',
    'Third-generation panel evidence: adoption rates among grandchildren of private retainers after sanctions lapsed. If native adoption followed the lapse of enforcement, decree achieved latent displacement and this reading''s assessment of the regime is overstated.',
    'Confirmation of ripening would collapse the necessity reading''s empirical base and shift family weight toward hybrid_scaffolding; confirmation of durable retention across generations would harden the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_retention_ambiguity, empirical, 'Whether retention signals incomplete internalization or deferred adoption.').

omega_variable(
    pooled_axis_comparability,
    'Do the calendar episode (revolutionary rupture, 1793-1805, lunar and Sunday observance persisting decades after abolition) and the dress episode (nationalist modernization, 1925 onward, urban diffusion with rural private retention) share enough dynamics to pool on one years-since-decree axis?',
    'Episode-separated re-measurement with regime-type covariates; test whether the trajectory shapes (extraction decay, theater rise) survive disaggregation.',
    'If the episodes differ systematically, the authored drift findings may be pooling artifacts and the interval''s endpoint values may misdate type-relevant transitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pooled_axis_comparability, empirical, 'Pooling assumption behind the shared measurement grid.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(loip_eclimb_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(loip_eclimb_tr_t0, observed).
narrative_ontology:measurement(loip_eclimb_tr_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement_basis(loip_eclimb_tr_t5, observed).
narrative_ontology:measurement(loip_eclimb_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(loip_eclimb_tr_t10, observed).
narrative_ontology:measurement(loip_eclimb_tr_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(loip_eclimb_tr_t15, observed).
narrative_ontology:measurement(loip_eclimb_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.53).
narrative_ontology:measurement_basis(loip_eclimb_tr_t20, observed).
narrative_ontology:measurement(loip_eclimb_tr_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(loip_eclimb_tr_t25, observed).
narrative_ontology:measurement(loip_eclimb_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement_basis(loip_eclimb_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(loip_eclimb_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(loip_eclimb_be_t0, observed).
narrative_ontology:measurement(loip_eclimb_be_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(loip_eclimb_be_t5, observed).
narrative_ontology:measurement(loip_eclimb_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(loip_eclimb_be_t10, observed).
narrative_ontology:measurement(loip_eclimb_be_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement_basis(loip_eclimb_be_t15, observed).
narrative_ontology:measurement(loip_eclimb_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(loip_eclimb_be_t20, observed).
narrative_ontology:measurement(loip_eclimb_be_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement_basis(loip_eclimb_be_t25, observed).
narrative_ontology:measurement(loip_eclimb_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(loip_eclimb_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(loip_eclimb_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(loip_eclimb_su_t0, observed).
narrative_ontology:measurement(loip_eclimb_su_t5, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement_basis(loip_eclimb_su_t5, observed).
narrative_ontology:measurement(loip_eclimb_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(loip_eclimb_su_t10, observed).
narrative_ontology:measurement(loip_eclimb_su_t15, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(loip_eclimb_su_t15, observed).
narrative_ontology:measurement(loip_eclimb_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement_basis(loip_eclimb_su_t20, observed).
narrative_ontology:measurement(loip_eclimb_su_t25, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 25, 0.39).
narrative_ontology:measurement_basis(loip_eclimb_su_t25, observed).
narrative_ontology:measurement(loip_eclimb_su_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(loip_eclimb_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: 'legitimacy of imposed practice' is one contested kernel instantiated as three constraints. All three readings assess the SAME standing arrangement - the decree-based displacement episodes (Republican Calendar 1793-1805 with multi-decade lunar/Sunday persistence; Hat Law 1925 with urban adoption and rural private retention) - and author different epsilon over that shared referent: this endogenous reading reads the arrangement as a failed imposition governed by a commitment-forming necessity (epsilon 0.46, decaying); the exogenous reading reads the same record as under-enforced decree; the hybrid reading reads it as scaffoldable partial displacement. Linked as a constraint family; this reading supplies the failure record on which the hybrid reading builds, and the direct logical contrary of the exogenous reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
