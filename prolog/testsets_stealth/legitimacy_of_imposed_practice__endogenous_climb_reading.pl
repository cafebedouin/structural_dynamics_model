% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Internalization Requirement for Practice Displacement (Endogenous-Climb Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A twentieth-century reforming state decreed a civil (solar) calendar and
 *   Western dress codes to consolidate national legibility, backing the
 *   decrees with inspectors, penalties, and registration requirements. Four
 *   decades on, the record shows the calendar displaced in offices, markets,
 *   and schools but survived intact in homes and congregational life; dress
 *   reform diffused through cities along commercial and administrative
 *   incentives while private retention — indoor veiling, home observance —
 *   persisted wherever enforcement did not reach. This file instantiates the
 *   endogenous_climb_reading of the legitimacy_of_imposed_practice kernel: on
 *   this reading, the standing arrangement under contest — decree-backed
 *   imposition — fails to displace practice because displacement runs through
 *   internalization, and internalization runs through bottom-up adoption
 *   pathways (commerce, schooling, congregational reproduction) that decree
 *   cannot substitute for. Epsilon's referent is the decree-imposition
 *   arrangement itself, assessed by this reading's lights: it consumes
 *   enforcement budgets and compliance labor while producing a stabilized
 *   dual register — public conformity, private continuity — rather than the
 *   conversion it promises. Sibling readings author different values over the
 *   same referent: the exogenous_override_reading reads the same arrangement
 *   as legitimate and functional (low epsilon, compliance counted as
 *   success); the hybrid_scaffolding_reading reads it as partially functional
 *   (mid epsilon, messaging-generated pull). Those siblings are separate
 *   constraint files; this story links them through
 *   network.affects_constraints and carries the contest in its omegas. KEY
 *   AGENTS (by structural relationship): - autonomy_preserving_communities:
 *   Primary beneficiary (organized/identity_locked) — inherit and reproduce
 *   the old register; sheltered by the failed displacement -
 *   state_modernization_planners: Primary target (institutional/constrained)
 *   — bear the failed timeline, sunk enforcement cost, and reporting burden -
 *   religious_authority_networks: Secondary beneficiary
 *   (organized/identity_locked) — collect standing and livelihood from
 *   continued observance - urban_diffusion_intermediaries: Conditional
 *   beneficiary (moderate/mobile) — profit from early adoption and diffuse
 *   the new forms - dress_law_enforcement_agents: Frontline payer
 *   (moderate/constrained) — apply the decrees to neighbors and absorb the
 *   social cost - women_under_dress_decrees: Excluded payer
 *   (powerless/trapped) — bore the coercive edge; absent from every council
 *   that shaped either register - modernization_theorists: Analytical
 *   observer (analytical/analytical) — adjudicate the readings; careers
 *   staked on competing accounts
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.3).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, mountain).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Internalization Requirement for Practice Displacement (Endogenous-Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).
domain_priors:emerges_naturally(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '38624b7e-cbe2-4721-a3d1-85565dfb6271').
narrative_ontology:cs_kernel_codification('38624b7e-cbe2-4721-a3d1-85565dfb6271', distributed).
narrative_ontology:cs_authority_grounding('38624b7e-cbe2-4721-a3d1-85565dfb6271', expertise).
narrative_ontology:cs_interpretation_layer_present('38624b7e-cbe2-4721-a3d1-85565dfb6271').
narrative_ontology:cs_reading_relation('38624b7e-cbe2-4721-a3d1-85565dfb6271', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('38624b7e-cbe2-4721-a3d1-85565dfb6271', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('38624b7e-cbe2-4721-a3d1-85565dfb6271', foundational, displacement_requires_internalization).
narrative_ontology:cs_axiom_status(displacement_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('38624b7e-cbe2-4721-a3d1-85565dfb6271', displacement_requires_internalization, empirically_contingent).
narrative_ontology:cs_axiom('38624b7e-cbe2-4721-a3d1-85565dfb6271', secondary, decreed_compliance_is_not_displacement).
narrative_ontology:cs_axiom_status(decreed_compliance_is_not_displacement, holdable).
narrative_ontology:cs_axiom_grounding('38624b7e-cbe2-4721-a3d1-85565dfb6271', decreed_compliance_is_not_displacement, empirically_contingent).
narrative_ontology:cs_reference_frame('38624b7e-cbe2-4721-a3d1-85565dfb6271', internalization_precedence_framework).
narrative_ontology:cs_drift_state('38624b7e-cbe2-4721-a3d1-85565dfb6271', contemporary_comparative_historical_synthesis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38624b7e-cbe2-4721-a3d1-85565dfb6271', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomy_preserving_communities).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, religious_authority_networks).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_planners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_diffusion_intermediaries).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomy_preserving_communities).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, dress_law_enforcement_agents).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, women_under_dress_decrees).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, endogenous_practice_reproduction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Keep lunar observance and inherited dress norms alive in homes, villages, and congregational networks while performing the mandated public forms wherever enforcement reaches. Continuity is carried by elders, ritual calendars, and mutual expectation rather than by any central organization. Abandoning the inherited practice would break kinship and congregational standing; adopting the new forms fully would break the same bonds on the other side, so households run both registers and absorb the doubled effort, fines, and occasional punishment that come with the public one.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomy_preserving_communities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomy_preserving_communities, payer).

% Draft and promulgate the calendar and dress decrees, budget the inspection apparatus, and report progress up the ministerial chain. Their careers and the state's consolidation timetable assume displacement within years, not generations. Each census and inspection cycle returns the same finding — public conformity, private continuation — and each response has been another circular, another inspectorate, another penalty schedule. They cannot abandon the modernization project without surrendering the state's founding legitimacy claim, and they cannot buy the outcome faster than diffusion allows.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_planners, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_planners, agenda_setter).

% Clerics, prayer leaders, and informal teachers whose standing, livelihoods, and daily schedules are constituted by the lunar calendar and inherited norms. Every decade the decrees survive without displacing observance confirms their authority over the rhythm of communal life. Their exit would dissolve the office they hold; they teach the old calendar to each cohort as a matter of course.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, religious_authority_networks, beneficiary,
    organized, generational, identity_locked, regional).

% Merchants, civil servants, and professionals in the larger cities who adopted the civil calendar and new dress first because commerce, office employment, and travel paid them to. They model the new forms for visitors from smaller towns and profit from the transition — trading, administering, translating between registers. Their adoption is real but situational: it tracks incentive, and it thins quickly past the urban ring.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_diffusion_intermediaries, beneficiary,
    moderate, biographical, mobile, regional).

% Local gendarmes, municipal inspectors, and registry clerks who must apply the decrees to neighbors and kin. They collect salaries from the campaign while absorbing its social costs — resentment, evasion they must pretend not to see, penalty quotas that measure their zeal. Most learn to enforce the letter in front of superiors and relax it out of their sight; a few are punished for either choice.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, dress_law_enforcement_agents, payer,
    moderate, biographical, constrained, local).

% Lived under the dress campaigns' direct coercive edge — fines, arrests, forced unveilings in the street — while holding no seat in the councils that drafted the decrees or in the communal bodies that negotiated responses to them. Their indoor retention of veiling is the sharpest recorded signal that public compliance did not reach conviction. Their options ran through household economy and kinship obligation, neither of which they controlled.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, women_under_dress_decrees, excluded,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, women_under_dress_decrees, payer).

% Comparative historians and sociologists who adjudicate between the readings of the imposition record. Careers, journals, and curricula are staked on competing accounts: some built on state-capacity explanations, others on embedded-practice explanations. They see the full structure — decrees, enforcement ledgers, retention surveys — and their disagreements reproduce the kernel contest inside the academy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, modernization_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, autonomy_preserving_communities).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns durable practice change with the structures that reproduce practice: a calendar or dress norm sticks where commerce, schooling, congregational life, and household routine carry it forward cohort by cohort. The requirement channels would-be reformers toward adoption pathways (markets, schools, urban employment) and gives communities a shared standard for distinguishing genuine from performed change.
% TRANSFER_FUNCTION: Moves enforcement expenditure and compliance labor from state budgets and subject populations into a stabilized dual register: the state pays for public conformity it cannot convert; communities pay doubled effort to run both registers; urban intermediaries collect status and margin from the transition; the modernization timeline pays in decades. Where internalization does occur, the flow reverses — communities carry the change voluntarily and enforcement cost falls.
% ABSENT_VOICES: Women subject to the dress decrees and the unconsulted rural majority were absent from the councils that drafted the decrees and from the communal bodies that negotiated responses; they would object that both registers — lunar and civil, veiled and unveiled — were assigned to them by others. Their absence is shared by all three readings of the kernel: each debates how displacement should happen, none debates whether the displaced should be asked.
% DISAPPEARANCE_RATIONALE: If the internalization requirement vanished overnight — if decree could genuinely displace practice — the modernization playbook inverts: enforcement budgets collapse into announcement, community autonomy strategies lose their shield, clerical authority over communal time evaporates within a generation, and the academic contest between state-capacity and embedded-practice accounts dissolves for lack of a phenomenon. Every named seat's situation is organized around the requirement's holding.
% FOUNDING_PROBLEM: Newly consolidated states needed rapid cultural legibility — one calendar, one dress norm, one administrable population — to bind territory and distinguish the nation from neighboring empires; decree promised in years what diffusion delivered in generations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the planners' own internal memoranda and enforcement ledgers concede shortfall after shortfall (attestation from the paying party against its own method); administrative-history archives quantify the enforcement expenditure returned as unconverted compliance; and post-decay ethnographic and survey series document private retention decades after the decrees. No corroboration comes from the communities that benefit — their attestation would be self-interested, and the record does not need it.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

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
 *   Epsilon (0.58) is authored for the decree-imposition arrangement as this reading assesses it: the arrangement transfers enforcement expenditure and compliance labor into a dual-register equilibrium that shelters the very practice it targets — real cost, real transfer, offset partly by the genuine urban diffusion the decrees did catalyze. Suppression (0.30) is authored as a raw structural property, unscaled by power or scope: the internalization requirement itself coerces almost nothing — its force is to foreclose the decree shortcut — while the coercive force visible in the record belongs to the state's counter-campaigns, which decay across the interval (suppression_requirement series 0.72 to 0.30) as repeated failure teaches partial tolerance and enforcement capacity attrits. Theater_ratio (0.45) prices the public-form/private-practice gap: a large share of observable displacement is performed compliance, though urban commercial adoption is real. Accessibility_collapse (0.55) and resistance (0.65) are the honest mountain-profile failures: alternatives to the reading — decree-sufficiency, scaffolded imposition — remain live and repeatedly retried, and powerful actors spent the whole interval fighting the requirement, which is either evidence it is not a law or evidence it is an expensive one. The claim (mountain) and these metrics are independently authored; their divergence is the measurement. All temporal series share one six-point grid (0, 8, 16, 24, 32, 40) with every tracked metric authored at every point; trajectories are monotonic, not cyclical — no intermittent-reinforcement dynamic is claimed. Rising base_extractiveness on a mountain claim may trip the accumulation-abduction trigger; that hypothesis is welcome as an investigation lead, not tuned toward.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same structure. From the planner seat the requirement is pure obstruction: it voids the state's founding promise on the state's own timetable, and every enforcement ledger reads as proof the instrument is broken. From the community and clerical seats the same structure is shelter: four decades of decrees without displacement confirm that their register cannot simply be legislated away, and each failed campaign raises the price of the next. The enforcement-agent seat splits the difference — salaried by the campaign, taxed by its social costs — which is why the derivation's payer-based directionality for that seat likely overshoots; the story accepts the derived value rather than overriding it and records the wage-offset here. Urban intermediaries sit near the beneficiary end but contingently: their adoption tracks incentive and would reverse if incentives did. The excluded seat experiences the arrangement's coercive edge most directly while collecting the least — the intra-community distribution question is carried in omega autonomy_benefit_distribution. The observer seat sees the contest itself and reproduces it academically.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: autonomy_preserving_communities and religious_authority_networks are subsidized by the requirement's operation — it preserves what they reproduce. The victim declaration drives high directionality: state_modernization_planners bear the constraint's costs directly and cannot exit their own project (constrained exit; national scope amplifies the verification difficulty of their compliance claims). Identity-lock is load-bearing on the beneficiary side: communities and clerics are locked by fusion of practice with kinship, congregation, and office, which places their d nearer the full-beneficiary end than mere preference would. The planner side is locked differently — by the state's founding legitimacy claim, not by belief. Women_under_dress_decrees derive high d from their payer position despite exclusion from deliberation; their exclusion shapes absent_voices, not their structural incidence. No directionality_overrides are authored: the beneficiary/victim plus exit derivation lands in the right region for every seat, and the one known distortion (enforcement agents' salaries damping their d) is documented in commentary rather than patched per-atom, since an override keyed to the moderate atom would also hit the urban intermediaries, whose derived low d is correct.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Against mislabeling as pure extraction: the requirement defeats the state but transfers nothing to a predatory seat — the gains land as preserved communal continuity, and the arrangement's coordination function (aligning durable practice change with the reproduction structures that can carry it) is genuine, which is why the false-summit path routes through a hybrid-coordination reclassification rather than a snare verdict. Against mislabeling as inertial residue: nothing here is theatrically maintained by an administrator who could cheaply change it — the state's enforcement decayed because changing the outcome proved prohibitive, not because anyone stopped caring. The genuine mandatrophy in this story belongs to the decree method, not to the requirement: the founding problem (rapid consolidation of a legible national practice-order) is still live, but the decree instrument built for it has repeatedly failed, and the R5 interview records that split — live problem, aging instrument — without letting the requirement itself be written off as vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_requirement_naturality,
    'Is the requirement that practice displacement run through internalization an invariant structural feature of social change, or a contingent regularity of the enforcement and communication technologies available to twentieth-century reforming states?',
    'Comparative analysis of displacement campaigns under radically higher surveillance and schooling capacity (mass-literacy regimes, digital-era mandates): if decree-plus-capacity achieves durable displacement without community uptake pathways, the requirement is contingent; if such cases still show private retention, it is invariant.',
    'If contingent, the mountain claim fails and the false-summit reclassification stands — the requirement operates as a constructed constraint sheltering community autonomy at the modernization timeline''s expense; if invariant, the claim certifies as natural law and the beneficiary declarations describe incidental incidence rather than capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_requirement_naturality, empirical, 'Natural-law versus constructed status of the internalization requirement (false-summit ambiguity; schema-required omega for a mountain with declared beneficiaries).').

omega_variable(
    displacement_definition_conflation,
    'Does displacement mean public behavioral compliance or internalized conviction-plus-practice — and is the kernel contest partly an artifact of the two readings measuring different dependent variables?',
    'Longitudinal datasets pairing public-compliance records with private-retention measures across the same cohorts; adjudication of which variable each reading''s supporting cases actually report.',
    'If displacement is defined as public compliance, the exogenous reading succeeds trivially and this reading''s epsilon collapses toward zero; if displacement means internalized practice, the failure cases stand and this reading''s structure holds. The disagreement is located in the dependent variable, not the mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_definition_conflation, conceptual, 'Definitional under-determination of the kernel''s dependent variable across readings.').

omega_variable(
    private_retention_signal_validity,
    'Do private-retention signals (indoor veiling, home lunar observance) validly measure incomplete internalization under active suppression?',
    'Post-decay cohort tracking in districts where enforcement relaxed: compare retention trajectories before and after suppression lifted; divergence indicates measurement distortion under policing.',
    'If retention was overstated, internalization was further along than this reading claims and the theater_ratio falls; if understated (hidden abandonment), the reading overstates persistence. Either way the measured gap between public form and private practice — the reading''s core datum — shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_retention_signal_validity, empirical, 'Measurement validity of private-retention evidence gathered under suppression.').

omega_variable(
    autonomy_benefit_distribution,
    'Is preserved practice a benefit for all members of the preserving communities, or does the public-compliance/private-retention split concentrate its costs on members — particularly women — who chose neither register?',
    'Intra-community disaggregated testimony and time-use records separating who performs public compliance, who bears private-retention labor, and who collects standing from continuity.',
    'If retention burdens fall on those without a seat in either register, the beneficiary declaration partially inverts on the dress axis — the same structure that shelters communal autonomy doubles some members'' obligations — pulling affected seats toward the target end and the overall classification toward sharper asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_benefit_distribution, preference, 'Whether preserved autonomy is uniformly a benefit depends on evaluative standpoint and intra-community distribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(loip_endogenous_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(loip_endogenous_tr_t0, observed).
narrative_ontology:measurement(loip_endogenous_tr_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(loip_endogenous_tr_t8, observed).
narrative_ontology:measurement(loip_endogenous_tr_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(loip_endogenous_tr_t16, observed).
narrative_ontology:measurement(loip_endogenous_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(loip_endogenous_tr_t24, observed).
narrative_ontology:measurement(loip_endogenous_tr_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(loip_endogenous_tr_t32, observed).
narrative_ontology:measurement(loip_endogenous_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(loip_endogenous_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(loip_endogenous_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(loip_endogenous_be_t0, observed).
narrative_ontology:measurement(loip_endogenous_be_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(loip_endogenous_be_t8, observed).
narrative_ontology:measurement(loip_endogenous_be_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement_basis(loip_endogenous_be_t16, observed).
narrative_ontology:measurement(loip_endogenous_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(loip_endogenous_be_t24, observed).
narrative_ontology:measurement(loip_endogenous_be_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement_basis(loip_endogenous_be_t32, observed).
narrative_ontology:measurement(loip_endogenous_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(loip_endogenous_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(loip_endogenous_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(loip_endogenous_su_t0, observed).
narrative_ontology:measurement(loip_endogenous_su_t8, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(loip_endogenous_su_t8, observed).
narrative_ontology:measurement(loip_endogenous_su_t16, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(loip_endogenous_su_t16, observed).
narrative_ontology:measurement(loip_endogenous_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(loip_endogenous_su_t24, observed).
narrative_ontology:measurement(loip_endogenous_su_t32, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 32, 0.35).
narrative_ontology:measurement_basis(loip_endogenous_su_t32, observed).
narrative_ontology:measurement(loip_endogenous_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.3).
narrative_ontology:measurement_basis(loip_endogenous_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'can states impose practice change?' decomposes into three structurally distinct claims with different epsilon over the same referent (the decree-imposition arrangement). This file (endogenous_climb_reading) authors epsilon ~0.58 — the arrangement as failing-and-costly; exogenous_override_reading authors low epsilon — the arrangement as legitimate and functional; hybrid_scaffolding_reading authors mid epsilon — partially functional via messaging-generated pull. Linkage direction: the exogenous claim is upstream in statecraft (cited by planners to justify decrees), this reading's documented failure cases are the evidence base the hybrid reading incorporates ('pure decree fails'), and this reading structurally pressures the hybrid without foreclosing it. Each member links the other two via network.affects_constraints; the contest itself is carried in omegas, not averaged into any single epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
