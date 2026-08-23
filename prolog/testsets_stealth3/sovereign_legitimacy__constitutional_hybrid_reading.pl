% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Dual-Sourced Legitimacy Settlement: Inherited Ceremony, Delegated Power, Constitutional Boundary
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   KEY AGENTS (by structural relationship): - hereditary_monarch_household:
 *   Primary beneficiary (institutional/identity_locked) - collects retained
 *   status, public income, and immunity norms - elected_government_officials:
 *   Co-beneficiary and boundary administrator (institutional/mobile) - wields
 *   delegated power legitimated by borrowed continuity -
 *   constitutional_court_judices: Agenda setter (institutional/constrained) -
 *   fixes and refixes the boundary through interpretation and precedent -
 *   absolutist_monarchists: Payer (moderate/identity_locked) - pure inherited
 *   rule permanently capped - republican_movement_activists: Payer
 *   (moderate/constrained) - pure delegated rule blocked by the hereditary
 *   remnant - taxpayers_funding_royal_household: Payer (moderate/constrained)
 *   - bears the fiscal cost of retained status -
 *   future_generations_of_subjects: Excluded (powerless/trapped) - bound by
 *   precedents they never consented to - legitimacy_studies_analysts:
 *   Analytical observer (analytical/analytical) - sees the full tri-kernel
 *   structure. The settlement splits legitimate authority into an inherited
 *   ceremonial component and a delegated political component, with
 *   constitutional law policing the boundary. It answers a genuine
 *   coordination problem - continuity of state authority across contested
 *   politics - while retaining asymmetric elements: the royal household
 *   collects funding and immunities without ordinary accountability, and both
 *   pure-form constituencies are permanently outvoted by entrenchment. This
 *   file instantiates ONE reading (constitutional_hybrid_reading) of the
 *   contested kernel sovereign_legitimacy; sibling readings are separate
 *   constraints linked in network.affects_constraints. Claim and metrics are
 *   authored independently: claimed_type states what this reading believes
 *   structurally true; metric values describe observed operation of the
 *   standing hybrid arrangement. Epsilon's referent is the standing
 *   arrangement itself, assessed by this reading's own lights - not any
 *   endorsed alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.46).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.34).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Dual-Sourced Legitimacy Settlement: Inherited Ceremony, Delegated Power, Constitutional Boundary").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, '3fee936a-9101-407a-ae13-727e98abee28').
narrative_ontology:cs_kernel_codification('3fee936a-9101-407a-ae13-727e98abee28', fixed_text).
narrative_ontology:cs_authority_grounding('3fee936a-9101-407a-ae13-727e98abee28', lineage).
narrative_ontology:cs_interpretation_layer_present('3fee936a-9101-407a-ae13-727e98abee28').
narrative_ontology:cs_reading_relation('3fee936a-9101-407a-ae13-727e98abee28', sovereign_legitimacy__monarchical_reading, influences).
narrative_ontology:cs_reading_relation('3fee936a-9101-407a-ae13-727e98abee28', sovereign_legitimacy__republican_reading, influences).
narrative_ontology:cs_axiom('3fee936a-9101-407a-ae13-727e98abee28', foundational, authority_partitionability).
narrative_ontology:cs_axiom_status(authority_partitionability, holdable).
narrative_ontology:cs_axiom_grounding('3fee936a-9101-407a-ae13-727e98abee28', authority_partitionability, conventional).
narrative_ontology:cs_axiom('3fee936a-9101-407a-ae13-727e98abee28', foundational, constitutional_mediation_of_boundary_claims).
narrative_ontology:cs_axiom_status(constitutional_mediation_of_boundary_claims, holdable).
narrative_ontology:cs_axiom_grounding('3fee936a-9101-407a-ae13-727e98abee28', constitutional_mediation_of_boundary_claims, instrumental).
narrative_ontology:cs_reference_frame('3fee936a-9101-407a-ae13-727e98abee28', dual_source_constitutional_mediation).
narrative_ontology:cs_drift_state('3fee936a-9101-407a-ae13-727e98abee28', contemporary_soft_power_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3fee936a-9101-407a-ae13-727e98abee28', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch_household).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_government_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, republican_movement_activists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, taxpayers_funding_royal_household).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the throne by bloodline, performs the ceremonial functions the settlement assigns (state openings, assents, investitures, state visits), receives public funding through civil-list or sovereign-grant appropriations, controls crown asset revenue streams, and enjoys legal-immunity norms surrounding the person of the sovereign. Its political voice is bounded by constitutional convention to consulting, advising, and warning. Exiting would mean abdication, which extinguishes the dynasty's entire meaning rather than relocating it.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch_household, beneficiary,
    institutional, generational, identity_locked, national).

% Wins office through competitive elections, exercises the delegated policy power the settlement reserves to elected hands, approves the royal household's funding, and defends the settlement during boundary crises. After leaving office they return to private life, board positions, or other careers; exit from the arrangement itself is routine and low-cost.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_government_officials, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__constitutional_hybrid_reading, elected_government_officials, agenda_setter).

% Adjudicates disputes over which authority governs which domain: whether a reserve power may be invoked, whether a prerogative belongs to crown or cabinet, where ceremony ends and governance begins. Issues precedential rulings that bind both the household and the elected branches. Holds tenured appointment insulating them from partisan pressure, but the same tenure blocks easy return to political or commercial life.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_court_judices, agenda_setter,
    institutional, biographical, constrained, national).

% Hold that the sovereign's authority should flow downward from inheritance, sanction, and tradition, with the crown ruling rather than reigning. The settlement permanently caps the crown below rulership while keeping its rituals alive, so their loyalty practices persist while their political program sits behind entrenched constitutional barriers that no ordinary politics can move. Leaving the cause would mean abandoning a loyalist identity that constitutes their self-concept.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists, payer,
    moderate, generational, identity_locked, national).

% Campaign to abolish the hereditary office and complete the transfer of all authority to delegated institutions. They are legally free to organize and publish, but structurally unable to win: amendment thresholds, referendum requirements, and cross-jurisdictional succession-law entanglement mean victory requires mass realignment they cannot manufacture. Their constraint is not persecution but permanent outvoting inside an entrenched frame.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, republican_movement_activists, payer,
    moderate, generational, constrained, national).

% Fund the civil list or sovereign grant, palace upkeep, security details, and ceremonial logistics through general taxation, receiving ceremony and continuity as diffuse public goods in return. Individual voices are diluted in the appropriations process; the practical exit is emigration, which carries heavy personal cost and leaves the obligation structure untouched.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, taxpayers_funding_royal_household, payer,
    moderate, biographical, constrained, national).

% Will inherit the settlement, its funding obligations, its deference norms, and its boundary ambiguities without having consented to any of them. They are absent from every adjudication that binds them; each precedent fixed today narrows the choices available to people who do not yet exist.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, future_generations_of_subjects, excluded,
    powerless, civilizational, trapped, national).

% Study legitimacy structures comparatively across monarchies, republics, and hybrids; publish classifications and boundary-dispute case studies; take no seat in the settlement's maintenance or contestation.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, legitimacy_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch_household).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__constitutional_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an unbroken, above-politics locus of state authority across elections and government turnover: succession is solved once by inheritance rather than re-fought at every transition, political conflict is confined to delegated offices so it cannot decapitate the state's symbolic continuity, and a single constitutional frame adjudicates which claims to authority count and where each stops.
% TRANSFER_FUNCTION: Moves public funds and legal immunities from taxpayers to the royal household; moves exclusive policy power, legitimated by borrowed continuity, to elected officeholders; moves adjudicative authority and interpretive prestige to constitutional courts as the boundary arbiter.
% ABSENT_VOICES: Pure-form advocates sit in the conversation as permanent losers rather than absentees: absolutists hold no institutional seat once precedent settles the boundary, and republicans hold none until entrenchment thresholds are crossed. Genuinely absent are future generations of subjects, who inherit the settlement's obligations and ambiguities without consent, and the descendants of peoples over whom the same crown's symbolic authority was historically projected beyond the national frame.
% DISAPPEARANCE_RATIONALE: Overnight removal forces immediate construction of head-of-state selection machinery, strands oath, assent, and dissolution procedures, unwinds succession-law frameworks entangled with treaties and other jurisdictions, and redistributes crown assets; every named seat's situation changes at once. The state would have to rebuild continuity mechanisms from scratch rather than continue without them.
% FOUNDING_PROBLEM: After the delegitimation of divinely sanctioned personal rule, states facing regime reconstruction needed an authority source that neither restored absolute inheritance nor left the headship newly invented and fragile. The hybrid solved this by splitting legitimacy: inherit the symbol to buy continuity, elect the power to buy consent, and let constitutional law referee between them.
% FOUNDING_PROBLEM_CORROBORATION: Attestation that the founding problem was real is strong and independent of the benefiting parties: diplomatic archives, polemics from both loyalist and opposition factions, and modern historiography converge on the succession wars and legitimacy crises the settlement answered. Attestation that the problem REMAINS live is split along interested lines: royal households and continuity-minded jurists argue the legitimation gap recurs at every transition, while republican scholars and comparative-politics evidence (stable elected presidencies in peer states) argue the founding problem is solved and the arrangement persists on inertia and elite interest. No fully neutral body adjudicates the disputed half; the historical record corroborates the problem's reality without settling its persistence.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: genuine coordination (peaceful alternation, insulated continuity, solved succession) coexists with asymmetric retention - dynastic income and immunity norms without accountability, and a permanent structural loss imposed on both pure-form constituencies. Suppression 0.34: enforcement is now overwhelmingly normative and legal (oaths, entrenchment, succession exclusions, precedent discipline) rather than coercive; the founding-era penal apparatus has decayed. Note suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope - only extraction is scaled, by directionality and spatial scope in the engine's computation. Theater ratio 0.35: ceremony remains substantially load-bearing (assent enacts law, openings convene parliaments, ceremonial presence performs real diplomatic work), but the performative share grows as governing functions migrate to elected institutions; it stays below the 0.5 piton band. Accessibility collapse 0.28: exits are demonstrably available - multiple peer polities have converted to republics and others have flirted with restoration - so alternatives do not vanish once the constraint is understood. Resistance 0.42: recurring republican campaigns, monarchist revanchism, and chronic boundary-dispute litigation, almost entirely channelled into lawful forms. The temporal picture: enforcement capacity fell steeply across the first two centuries (military suppression of restoration attempts giving way to constitutional routine) - hence the authored suppression_requirement series, which specifically traces enforcement-infrastructure change; extraction declined as discretionary royal takings were stripped away, then stabilized and ticked up recently amid financing controversies; theatricality rose as function drained from rite. All three series run on ONE shared time grid (points 0, 50, 100, 150, 200, 250, 300, 335) so no metric borrows another's endpoint. Identity-lock operates on two seats: absolutist_monarchists carry ideological fusion (loyalism constitutes self-concept, so exit equals identity death), and the royal household carries institutional identity (house equals function; abdication annuls rather than relocates the role). If either frame broke, the extraction map would shift - absolutists would convert to constitutional conservatives or republicans, and the household would become a negotiable private estate. Suppression mechanism composition is estimated at roughly seventy percent structural, thirty percent internalized; the deference_norm_internalization omega carries the uncertainty. Boundary-dispute incidents cycle at the event level (crisis, settlement, calm, accumulation), but the tracked metrics measure the aggregate arrangement, not the incident cycle, so no cyclical measurement series is asserted.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is structural, not rhetorical. From the hereditary_monarch_household seat the arrangement reads as dignified subsidy with occasional humiliation; from elected_government_officials as a convenience that outsources legitimacy overhead; from constitutional_court_judices as a solvable adjudication stream; from absolutist_monarchists as a gilded cage (their ideal perpetually displayed and perpetually withheld); from republican_movement_activists as a hostage situation for their preferred constitution; from taxpayers as a standing appropriation they never voted item-by-item. Same-level lateral dynamics matter: absolutists and republicans hold nearly identical power atoms and exit profiles yet experience OPPOSITE asymmetries - one wants more inherited authority than the settlement permits, the other wants none at all - and the hybrid taxes them identically in outcome space (neither obtains the pure form) while extracting from each for opposite reasons. The engine computes per-seat types from this structural data; the authored claim does not adjudicate between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: hereditary_monarch_household and elected_government_officials are declared beneficiaries (low directionality, extraction damped or inverted toward subsidy); absolutist_monarchists, republican_movement_activists, and taxpayers_funding_royal_household are declared victims and appear as payers on the stakeholder surface (high directionality, extraction amplified - maximally so for the identity_locked absolutist seat, since trapped targets sit nearer the full-target end than mobile ones). constitutional_court_judices are agenda_setters with no beneficiary or victim declaration, so their directionality takes the canonical fallback near symmetry: they administer the boundary without collecting from it. future_generations_of_subjects carry the excluded role - commentary-grade absence, never correction-grade - and legitimacy_studies_analysts hold the analytical seat outside the extraction computation entirely. Scope amplification applies modestly: the settlement's national scope with cross-jurisdictional entanglements raises verification difficulty slightly above the purely local case.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating reconstructed states after divine-right collapse) is genuinely contested rather than dead: the continuity function still performs real work at every government transition, while the anti-absolutist urgency that originally justified capping the crown is extinct in consolidated democracies. Because founding_problem_status is contested rather than dead, the mismatch consumer (dead-status x rearranges-verdict) does NOT fire a zombie flag - correctly, since the arrangement still rearranges the world if removed. The tangled-rope classification is what prevents mislabeling in both directions: calling this a pure rope erases the dynastic rent stream and the suppressed pure forms; calling it a snare erases the continuity and alternation functions that every seat, including payers, would mourn in the breach. Mandatrophy is not resolved and no sunset clause is declared: the settlement presents itself as a steady-state compromise, and the boundary-dispute stream it generates is chronic rather than terminal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the sovereign_legitimacy kernel governs classification: does the dual-source settlement constitute one coherent constraint, or do rival readings of the same kernel (monarchical_reading: authority flows downward by inherited right and sanction; republican_reading: authority flows upward by delegated consent) instantiate different constraints with different beneficiary and victim sets?',
    'Trace adjudicated boundary outcomes across successive generations: if precedent consistently subordinates inherited claims to delegated ones, the settlement hardens toward the republican-adjacent configuration; if reserve powers and ceremonial influence expand, toward the monarchical-adjacent one. Comparative analysis of hybrid systems under stress supplies the discriminating cases.',
    'Wholesale adoption of a sibling reading changes the constraint itself, not just its label: the monarchical reading raises epsilon sharply (unchecked dynastic extraction, suppressed popular movements), while the republican reading eliminates the retained-status component entirely, producing a different constraint with lower taxpayer burden but higher transition costs. This story''s epsilon is valid only for the hybrid reading''s instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: this constraint is one reading of a contested three-way kernel; sibling readings would restructure the beneficiary and victim sets entirely.').

omega_variable(
    civil_list_rent_or_compensation,
    'Is the royal household''s retained public income and legal-immunity package unearned rent surviving from the pre-constitutional era, or compensation for genuine ceremonial labor the state would otherwise have to procure?',
    'Audit ceremonial duty-hours and operating costs against publicly employed heads of state in comparable republics; decompose household funding into service-delivery versus status-maintenance components.',
    'If the balance is rent, the taxpayer victim seat deepens, epsilon rises, and the constraint drifts toward heavier extraction; if it is compensation, the extraction estimate falls and the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_list_rent_or_compensation, empirical, 'Whether the monarch''s retained income tracks performed function or survives as legacy appropriation.').

omega_variable(
    ambiguity_cost_accumulation,
    'Do unresolved boundary disputes between the inherited and delegated components compound (each unsettled precedent raising the stakes of the next confrontation) or dissipate (each settlement lowering the temperature of subsequent ones)?',
    'Longitudinal severity index of boundary crises: reserve-power invocations, judicial interventions in crown-prerogative matters, prorogation-class confrontations, scored per decade across multiple hybrid systems.',
    'Compounding ambiguity acts as an uncertainty tax on every seat, raising effective extraction and eventually threatening the settlement''s classification stability; dissipating ambiguity confirms the tangled-rope equilibrium as durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_cost_accumulation, empirical, 'Whether the hybrid''s characteristic boundary disputes are self-limiting or cumulative.').

omega_variable(
    deference_norm_internalization,
    'How much of the measured suppression of pure-form advocacy is structural (entrenchment clauses, oath requirements, succession exclusions) versus internalized (deference norms that make republican or restorationist advocacy socially costly even where formally permitted)?',
    'Attitude trajectories in former monarchies that completed abolition: if deference patterns persist after the structural barriers were removed, a large internalized share is indicated; if they decay quickly, suppression was predominantly structural.',
    'An internalized share means effective suppression exceeds the structural measure and travels with populations across constitutional change; a structural share means removing the barriers releases the constraint directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_norm_internalization, empirical, 'Structural versus internalized composition of the settlement''s suppressive force.').

omega_variable(
    convergence_vs_contingency,
    'Is the dual-source pattern a structural attractor that independently recurs wherever personal rule meets mass politics (suggesting a mountain-adjacent regularity dressed as a bargain), or a contingent artifact of specific historical settlements transmitted by imitation?',
    'Comparative institutional genealogy of regime transitions: count cases where the dual-source shape emerged without direct diffusion or imposition from prior hybrid models.',
    'Demonstrated convergence would warrant false-summit scrutiny of the settlement''s presented-as-bargain status; confirmed contingency keeps it a constructed constraint whose classification rests on the authored structural data alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convergence_vs_contingency, empirical, 'Naturality versus construction of the dual-sourcing pattern across political history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 0, 335).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 100, 0.26).
narrative_ontology:measurement(sove_tr_t150, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 150, 0.29).
narrative_ontology:measurement(sove_tr_t200, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(sove_tr_t250, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 250, 0.33).
narrative_ontology:measurement(sove_tr_t300, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 300, 0.34).
narrative_ontology:measurement(sove_tr_t335, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 335, 0.35).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 50, 0.51).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 100, 0.47).
narrative_ontology:measurement(sove_be_t150, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 150, 0.44).
narrative_ontology:measurement(sove_be_t200, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 200, 0.42).
narrative_ontology:measurement(sove_be_t250, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 250, 0.43).
narrative_ontology:measurement(sove_be_t300, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(sove_be_t335, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 335, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 100, 0.53).
narrative_ontology:measurement(sove_su_t150, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 150, 0.46).
narrative_ontology:measurement(sove_su_t200, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 200, 0.41).
narrative_ontology:measurement(sove_su_t250, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 250, 0.37).
narrative_ontology:measurement(sove_su_t300, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 300, 0.35).
narrative_ontology:measurement(sove_su_t335, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 335, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy__republican_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: colloquial talk of 'legitimate authority' conflates three structurally distinct constraints sharing one kernel. The monarchical_reading is the upstream member (historically prior, maximal empirical establishment as the pre-modern default); the republican_reading is the parallel modern claim; this constitutional_hybrid_reading is downstream of both, citing each as foil and evidence - it inherits the dynasty's continuity argument from the monarchical side and the consent argument from the republican side. Epsilon differs across members by construction: the hybrid's compromise profile (low-to-moderate extraction, dual beneficiaries, doubly constrained purists) is incompatible with the monarchical reading's concentrated extraction profile and with the republican reading's abolition of the retained-status component. Each story carries its own epsilon, stakeholders, and classification; the links here propagate contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
