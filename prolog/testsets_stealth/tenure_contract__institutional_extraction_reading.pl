% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Academic Tenure as Permanent-Claim Allocation Regime
 *   domain: economic/labor/higher-education-governance
 *
 * SUMMARY:
 *   A two-tier academic employment structure in which a cohort that won
 *   permanent appointments early holds lifetime claims on salary, research
 *   time, and institutional resources, while the majority of instructional
 *   labor is performed on short-term contracts without those claims.
 *   Enrollment and administrative spending grew for decades while tenure-line
 *   counts stayed roughly flat; the difference was staffed with contingent
 *   instructors. The arrangement's operation is routinely cited as
 *   vindicating internal-labor-market doctrine — insiders insulated by rigid
 *   rules, outsiders confined to a secondary market. Its ε referent is the
 *   standing two-tier arrangement as this account assesses it, not any
 *   preferred alternative arrangement. Claimed type and metrics are authored
 *   independently: the claim is tangled_rope because the arrangement retains
 *   a real coordination function (credible long-horizon commitment, peer-run
 *   evaluation) while operating with substantial asymmetric extraction; the
 *   metrics describe the arrangement's observed operation. KEY AGENTS (by
 *   structural relationship): - tenured_faculty_early_winners: Primary
 *   beneficiary (organized/identity_locked) — holds the permanent claims the
 *   arrangement pays out - university_administrations: Agenda setter with
 *   secondary beneficiary position (institutional/mobile) — runs the process
 *   and collects the flexibility savings - contingent_faculty_adjuncts:
 *   Primary target (powerless/trapped) — bears the flexibility costs -
 *   students_and_families: Cost bearer (moderate/constrained) — pays via
 *   tuition and reduced instructional investment -
 *   doctoral_students_pipeline: Deferred target (powerless/constrained) —
 *   trains for a track that is closing - state_legislatures_public_funders:
 *   External agenda setter (institutional/mobile) — episodic budgetary and
 *   legislative leverage - displaced_scholars_leaving_academe: Excluded voice
 *   (moderate/arbitrage) — experienced the narrowing firsthand, holds no
 *   governance seat - academic_labor_economists: Analytical observer
 *   (analytical/analytical) — measures the structure, holds no vote
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.76).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.58).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.57).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Academic Tenure as Permanent-Claim Allocation Regime").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "economic/labor/higher-education-governance").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '915609b8-164b-49de-ab97-80e972893e03').
narrative_ontology:cs_kernel_codification('915609b8-164b-49de-ab97-80e972893e03', formalized).
narrative_ontology:cs_authority_grounding('915609b8-164b-49de-ab97-80e972893e03', extraction).
narrative_ontology:cs_interpretation_layer_present('915609b8-164b-49de-ab97-80e972893e03').
narrative_ontology:cs_reading_relation('915609b8-164b-49de-ab97-80e972893e03', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('915609b8-164b-49de-ab97-80e972893e03', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('915609b8-164b-49de-ab97-80e972893e03', foundational, employment_security_operates_as_transferable_rent).
narrative_ontology:cs_axiom_status(employment_security_operates_as_transferable_rent, holdable).
narrative_ontology:cs_axiom_grounding('915609b8-164b-49de-ab97-80e972893e03', employment_security_operates_as_transferable_rent, empirically_contingent).
narrative_ontology:cs_axiom('915609b8-164b-49de-ab97-80e972893e03', foundational, rigidity_costs_exceed_stability_benefits).
narrative_ontology:cs_axiom_status(rigidity_costs_exceed_stability_benefits, holdable).
narrative_ontology:cs_axiom_grounding('915609b8-164b-49de-ab97-80e972893e03', rigidity_costs_exceed_stability_benefits, empirically_contingent).
narrative_ontology:cs_axiom('915609b8-164b-49de-ab97-80e972893e03', secondary, contingent_tier_externalizes_protected_costs).
narrative_ontology:cs_axiom_status(contingent_tier_externalizes_protected_costs, holdable).
narrative_ontology:cs_axiom_grounding('915609b8-164b-49de-ab97-80e972893e03', contingent_tier_externalizes_protected_costs, empirically_contingent).
narrative_ontology:cs_reference_frame('915609b8-164b-49de-ab97-80e972893e03', permanent_claim_registry).
narrative_ontology:cs_drift_state('915609b8-164b-49de-ab97-80e972893e03', contingent_majority_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('915609b8-164b-49de-ab97-80e972893e03', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty_early_winners).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, university_administrations).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty_adjuncts).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students_and_families).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, doctoral_students_pipeline).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, internal_labour_market_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments awarded after a one-time evaluation typically completed in their thirties. Salary, research time, and course releases continue regardless of subsequent performance or institutional need; dismissal requires lengthy cause proceedings that rarely conclude. Many serve on the committees that evaluate the next cohort. Voluntary departure means surrendering the accumulated claim, and their professional standing, publication networks, and daily routines are built around the protected position.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty_early_winners, beneficiary,
    organized, generational, identity_locked, national).

% Run the tenure process, control how many tenure lines open each year, and decide how enrollment growth gets staffed. Over recent decades they have held tenure-line counts roughly flat while expanding instructor pools hired semester-to-semester at lower cost and without permanent claims. They defend the tenure system publicly — it anchors rankings, accreditation, and donor narratives — while relying on the flexible tier to balance budgets. Administrative careers advance by moving between institutions, carrying the same staffing playbook along.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administrations, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administrations, beneficiary).

% Teach a large and growing share of courses on per-course or one-year contracts, at a fraction of protected-track pay, often commuting between multiple campuses. Office space, benefits, and a voice in curriculum are scarce or absent. Years on these contracts erode research records and references, so the door back to the protected track narrows with each year served. Unionization drives have met sustained institutional resistance.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty_adjuncts, payer,
    powerless, immediate, trapped, national).

% Pay rising tuition that funds both protected salaries and the administrative apparatus around them, while an increasing fraction of instruction is delivered by the lowest-paid tier. They can choose among institutions, but the two-tier staffing pattern is sector-wide, so exit means leaving higher education rather than escaping the structure.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students_and_families, payer,
    moderate, biographical, constrained, national).

% Spend six to nine years in funded training for a protected track that opens fewer slots than there are graduates. They teach introductory courses as part of their funding packages, absorbing instructional load at the bottom of the same hierarchy, and their career planning assumes a lottery whose odds they can observe directly.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, doctoral_students_pipeline, payer,
    powerless, biographical, constrained, national).

% Appropriate public funds to university systems and periodically legislate on faculty employment — post-tenure review mandates, financial-exigency definitions, reporting requirements. Several have attempted to weaken or eliminate permanent appointments; university lobbying and accreditation consequences have blunted most attempts. Their leverage is budgetary and episodic rather than day-to-day.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, state_legislatures_public_funders, agenda_setter,
    institutional, biographical, mobile, regional).

% Trained researchers who concluded the protected track would never open and left for industry, government, or other sectors. They hold direct experience of the pyramid's narrowing but have no seat in faculty governance, collective bargaining, or accreditation conversations; their testimony enters only through exit surveys and journalism.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, displaced_scholars_leaving_academe, excluded,
    moderate, biographical, arbitrage, national).

% Study the two-tier structure with panel data on hiring lines, wage gaps, and resource reallocation. They publish estimates of the arrangement's costs and beneficiaries but hold no vote in any governance body that maintains or reforms it.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty_early_winners).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes the institution's long-horizon employment promise credible: once granted, the appointment cannot be revoked at will, so scholars invest in slow research programs and institutions retain scarce expertise through political and budgetary turbulence. It also stabilizes a senior labor market in which evaluation is performed by peers rather than managers.
% TRANSFER_FUNCTION: Moves permanent employment security, salary floor, and control over research time from the institution to whoever wins a tenure line early in adulthood; moves instructional flexibility costs, per-course precarity, and benefit gaps onto contingent faculty; moves tuition and forgone instructional investment onto students and families.
% ABSENT_VOICES: Contingent faculty sit outside most senates and bargaining units that set the terms; displaced scholars who left have no seat anywhere; students are surveyed on satisfaction but not consulted on staffing structure. They are on per-course contracts scattered across campuses, outside the accreditation and governance rooms where the arrangement is maintained.
% DISAPPEARANCE_RATIONALE: If permanent appointments vanished overnight, institutions would re-staff instruction on renewable contracts, senior compensation would reprice toward market rates, mid-career mobility would open, and research agendas would shift toward shorter-horizon projects responsive to funders and administrators — the academic labor market, internal promotion ladders, and the tuition cost structure would all reorganize within a decade.
% FOUNDING_PROBLEM: Early twentieth-century professors were dismissed for research findings or opinions that offended donors, trustees, and politicians; the arrangement was built to make dismissal for ideas procedurally impossible and long-horizon scholarly careers bankable.
% FOUNDING_PROBLEM_CORROBORATION: Historical documentation of pre-1940 donor and trustee dismissals corroborates the founding problem, as do contemporary reprisal cases involving instructors without permanent appointments, reported by journalistic and legal-monitoring organizations outside the university. Labor economists corroborate that the protective function and the extraction function coexist in the same contract. The strongest attestations of the problem's persistence come from outside the benefiting cohort; the cohort's own institutions attest it chiefly when defending the arrangement.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.76) because the permanent claim decouples compensation and resource control from ongoing performance and institutional need, and because the flexibility costs of that rigidity are carried by the lowest-paid tier. Suppression (0.58) is a raw structural property, unscaled by power or scope: it reflects blocked alternatives — the narrowing door back to the protected track, identity fusion with the protected role, sector-wide staffing uniformity limiting student exit — rather than active coercion alone. Theater ratio (0.46) reflects review processes that perform merit-maintenance (dossiers, collegiality letters, post-tenure reviews) while the binding allocation decision — how many lines to open — is made elsewhere. Accessibility collapse (0.52) is partial: industry and alt-academic exits exist, but within-academia alternatives to the two-tier structure have largely disappeared. Resistance (0.57) is real and growing: adjunct unionization campaigns, legislative reform attempts, and student cost politics. The three measurement series share one time grid (points 0–36 at steps of 6) so every metric is authored at every examined point. Trajectories are monotonic ratchets, not cycles: extraction accumulates as lines freeze, theater grows as review ritualizes, and suppression_requirement rises because this story specifically tracks enforcement-capacity buildout — HR compliance machinery, anti-union campaigns, legislative-defense lobbying — which is why that series is authored at all rather than left to the static scalar.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical facts. From the tenured seat the arrangement is deferred compensation honestly earned through a probationary tournament — extraction near zero, protection paramount. From the contingent seat the same contract is the wall that keeps them in per-course precarity — maximal extraction. From the administrative seat it is a legitimacy asset and a flexibility instrument simultaneously: publicly defended, privately routed around. From the student seat it is tuition paid for a product increasingly delivered by the cheapest tier. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing in the claimed type adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: tenured faculty hold the permanent claims (near the beneficiary end); administrations hold a secondary beneficiary position — they collect the flexibility savings the rigidity licenses — pulling their derived d toward the beneficiary side despite running the system. Victim declarations drive high d: contingent faculty (trapped exit) sit nearest the full-target end; students and the doctoral pipeline (constrained exit) sit high but slightly below, since degrees retain option value outside the sector. No directionality_overrides are authored: the beneficiary/victim data plus exit options produce the correct ordering, and the override mechanism keys on power atoms, which cannot separate doctoral aspirants (victims whose opposition is dampened by aspirational identification with the protected seat) from adjuncts (victims with no such identification) — that distinction is carried in an omega instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making dismissal-for-ideas impossible and long-horizon careers bankable — is contested, not dead: political reprisal against unprotected instructors remains documented, so the protective demand persists even as the same contract functions as a rent instrument. Classifying the arrangement as tangled_rope keeps both facts alive: calling it a snare would erase the genuine commitment-credibility function that still protects inquiry at the margin; calling it a rope would erase the documented asymmetry by which early winners' permanent claims are financed by a contingent tier and rising tuition. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the world would rearrange because enormous accumulated claims depend on the arrangement, not because the founding problem is solved. Mandatrophy is therefore not declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the tenure_contract kernel. Would instantiating the academic_freedom_reading on the same facts yield a structurally different constraint — lower ε, a different victim set — or does the extraction evidence dominate any protective accounting?',
    'Cross-reading comparison on a shared referent: estimate the insurance value of protection against documented reprisal cases and price it against measured rent and rigidity costs; whichever accounting survives contact with line-conversion behavior (institutions acting as if permanence is a cost to minimize) indicates the operative frame.',
    'If the protective accounting dominates, ε falls substantially and the arrangement computes nearer a defended coordination mechanism; if the extraction accounting dominates, this file''s classification stands and the protective reading is demoted to legitimation cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the extraction frame or the protective frame captures the arrangement''s operative structure.').

omega_variable(
    counterfactual_reallocation_baseline,
    'If permanent claims were unwound, would the freed resources reach instruction and students, or would they be absorbed by administrative growth?',
    'Natural experiments: systems that imposed post-tenure review or financial-exigency conversions — trace where marginal dollars went afterward.',
    'Determines whether students are true victims of this constraint or of a separate administrative-capture constraint that would persist either way; the latter forces a decomposition into two linked stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_reallocation_baseline, empirical, 'Counterfactual incidence of freed resources under unwinding.').

omega_variable(
    causality_disinvestment_vs_rigidity,
    'Is contingent-tier growth caused by tenure rigidity, or by state disinvestment that would have produced contingent labor under any employment regime?',
    'Panel comparison of systems with differing tenure protections exposed to similar funding shocks; if contingent share tracks funding shocks independent of tenure strength, rigidity is not the binding cause.',
    'If disinvestment dominates, tenure''s ε contribution shrinks and the story decomposes into tenure-plus-defunding linked constraints; if rigidity dominates, this file stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_disinvestment_vs_rigidity, empirical, 'Attribution of contingent growth between rigidity and defunding.').

omega_variable(
    rent_vs_compensating_differential,
    'What share of the tenured compensation package is economic rent rather than compensating differential for the risks and lower early-career wages borne before the award?',
    'Compensating-differential estimation against comparable doctorate holders in non-permanent tracks, controlling for ability selection and the probabilistic value of the award.',
    'Recalibrates ε: the differential component is coordination cost, not extraction; only the excess is rent. A small excess would pull the arrangement toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_vs_compensating_differential, empirical, 'Rent share versus deferred-risk compensation in the tenured package.').

omega_variable(
    aspirational_identity_dampening,
    'Does the doctoral pipeline''s aspirational identification with the protected seat dampen its measured resistance below the level its structural position implies?',
    'Cohort tracking of attrition-stage attitudes: whether opposition to the two-tier structure rises after the point where the protected track becomes unreachable.',
    'If dampened, authored resistance understates structural opposition and effective suppression on that seat is higher than the scalar suggests; classification consequences concentrate on the pipeline seat rather than the whole arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aspirational_identity_dampening, conceptual, 'Aspirational identity lock dampening victim-seat resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_extraction_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tenure_extraction_tr_t0, observed).
narrative_ontology:measurement(tenure_extraction_tr_t6, tenure_contract__institutional_extraction_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(tenure_extraction_tr_t6, observed).
narrative_ontology:measurement(tenure_extraction_tr_t12, tenure_contract__institutional_extraction_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement_basis(tenure_extraction_tr_t12, observed).
narrative_ontology:measurement(tenure_extraction_tr_t18, tenure_contract__institutional_extraction_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement_basis(tenure_extraction_tr_t18, observed).
narrative_ontology:measurement(tenure_extraction_tr_t24, tenure_contract__institutional_extraction_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(tenure_extraction_tr_t24, observed).
narrative_ontology:measurement(tenure_extraction_tr_t30, tenure_contract__institutional_extraction_reading, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(tenure_extraction_tr_t30, observed).
narrative_ontology:measurement(tenure_extraction_tr_t36, tenure_contract__institutional_extraction_reading, theater_ratio, 36, 0.46).
narrative_ontology:measurement_basis(tenure_extraction_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(tenure_extraction_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(tenure_extraction_be_t0, observed).
narrative_ontology:measurement(tenure_extraction_be_t6, tenure_contract__institutional_extraction_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(tenure_extraction_be_t6, observed).
narrative_ontology:measurement(tenure_extraction_be_t12, tenure_contract__institutional_extraction_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(tenure_extraction_be_t12, observed).
narrative_ontology:measurement(tenure_extraction_be_t18, tenure_contract__institutional_extraction_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement_basis(tenure_extraction_be_t18, observed).
narrative_ontology:measurement(tenure_extraction_be_t24, tenure_contract__institutional_extraction_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement_basis(tenure_extraction_be_t24, observed).
narrative_ontology:measurement(tenure_extraction_be_t30, tenure_contract__institutional_extraction_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement_basis(tenure_extraction_be_t30, observed).
narrative_ontology:measurement(tenure_extraction_be_t36, tenure_contract__institutional_extraction_reading, base_extractiveness, 36, 0.76).
narrative_ontology:measurement_basis(tenure_extraction_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenure_extraction_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(tenure_extraction_su_t0, observed).
narrative_ontology:measurement(tenure_extraction_su_t6, tenure_contract__institutional_extraction_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(tenure_extraction_su_t6, observed).
narrative_ontology:measurement(tenure_extraction_su_t12, tenure_contract__institutional_extraction_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement_basis(tenure_extraction_su_t12, observed).
narrative_ontology:measurement(tenure_extraction_su_t18, tenure_contract__institutional_extraction_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement_basis(tenure_extraction_su_t18, observed).
narrative_ontology:measurement(tenure_extraction_su_t24, tenure_contract__institutional_extraction_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(tenure_extraction_su_t24, observed).
narrative_ontology:measurement(tenure_extraction_su_t30, tenure_contract__institutional_extraction_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(tenure_extraction_su_t30, observed).
narrative_ontology:measurement(tenure_extraction_su_t36, tenure_contract__institutional_extraction_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement_basis(tenure_extraction_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% 'Tenure' is a colloquial label covering at least three structurally distinct claims instantiated from the tenure_contract kernel: protection of inquiry (academic_freedom_reading), compositional gatekeeping (demographic_reproduction_reading), and permanent-claim rent allocation (this file). The readings differ in ε, victim sets, and failure modes, so each is authored as its own constraint and linked here. Upstream/downstream structure: the freedom reading supplies the legitimacy vocabulary this arrangement trades on; this reading's rent and rigidity findings create downstream pressure on both siblings — every documented line conversion erodes the protective cover and sharpens the gatekeeping question — without logically foreclosing either.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
