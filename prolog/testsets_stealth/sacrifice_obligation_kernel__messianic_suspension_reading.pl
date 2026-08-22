% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrificial Obligation in Messianic Suspension — Readiness-Maintenance Reading
 *   domain: religious law / commitment-system dynamics
 *
 * SUMMARY:
 *   After the destruction of the Second Temple (70 CE), the rabbinic
 *   tradition settled the status of the Torah's sacrificial legislation with
 *   a ruling this story instantiates as ONE READING of a contested kernel:
 *   the obligation remains in force but is divinely suspended — not
 *   transformed, not fulfilled by substitutes, not dissolved — until
 *   messianic restoration, and the community's study of the sacrificial
 *   corpus maintains operational readiness for that resumption. The ε
 *   referent is the standing suspension-plus-study arrangement itself,
 *   assessed by this reading's own lights: nobody sacrifices, nobody is
 *   coerced into studying, and no victim class exists during the suspension
 *   period. The mild extraction that remains is the communal and (after 1948)
 *   state resourcing of the study-and-readiness apparatus, the status rents
 *   of the scholarly class, and the preserved role-claims of priestly
 *   lineages. Claim and metrics are authored independently: claimed_type is
 *   scaffold because the arrangement is explicitly transitional — its own
 *   declared terminus is the restoration — while the metrics describe low but
 *   nonzero, slowly accumulating extraction with a post-1967 enforcement
 *   intensification against premature-performance activism.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_structure: agenda-setter (institutional / identity_locked) — issues and maintains the suspension rulings, sets curricula, adjudicates restoration-conditions questions; its authority is exercised through the arrangement it administers
 *   - sacrificial_law_scholars: primary beneficiary (moderate / identity_locked) — maintain readiness through study of the sacrificial order; receive communal and state support, vocation, and status
 *   - priestly_lineages: beneficiary (organized / identity_locked) — hold hereditary claims on the restored service; maintain purity disciplines and training in anticipation
 *   - future_generations_of_the_community: beneficiary (powerless / trapped) — inherit the preserved operational capacity and, with it, the suspended obligation itself
 *   - communal_funders: payer (organized / constrained) — fund academies, stipends, and the readiness apparatus through donations, tithes, and taxes
 *   - temple_mount_activists: excluded (organized / constrained) — reject the suspension as premature despair and seek performance now; their project is what the enforcement machinery restrains
 *   - comparative_religion_historians: observer (analytical / analytical) — study the arrangement as a case of long-duration obligation maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.3).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.18).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrificial Obligation in Messianic Suspension — Readiness-Maintenance Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious law / commitment-system dynamics").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '2eeaa3cd-d468-4a1d-bbac-63592e564be6').
narrative_ontology:cs_kernel_codification('2eeaa3cd-d468-4a1d-bbac-63592e564be6', fixed_text).
narrative_ontology:cs_authority_grounding('2eeaa3cd-d468-4a1d-bbac-63592e564be6', lineage).
narrative_ontology:cs_interpretation_layer_present('2eeaa3cd-d468-4a1d-bbac-63592e564be6').
narrative_ontology:cs_reading_relation('2eeaa3cd-d468-4a1d-bbac-63592e564be6', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('2eeaa3cd-d468-4a1d-bbac-63592e564be6', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eeaa3cd-d468-4a1d-bbac-63592e564be6', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('2eeaa3cd-d468-4a1d-bbac-63592e564be6', foundational, obligation_suspended_not_transformed).
narrative_ontology:cs_axiom_status(obligation_suspended_not_transformed, holdable).
narrative_ontology:cs_axiom_grounding('2eeaa3cd-d468-4a1d-bbac-63592e564be6', obligation_suspended_not_transformed, theological).
narrative_ontology:cs_axiom('2eeaa3cd-d468-4a1d-bbac-63592e564be6', foundational, study_preserves_operational_readiness).
narrative_ontology:cs_axiom_status(study_preserves_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('2eeaa3cd-d468-4a1d-bbac-63592e564be6', study_preserves_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('2eeaa3cd-d468-4a1d-bbac-63592e564be6', sinaitic_obligation_in_divine_suspension).
narrative_ontology:cs_drift_state('2eeaa3cd-d468-4a1d-bbac-63592e564be6', contemporary_post_1967_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('2eeaa3cd-d468-4a1d-bbac-63592e564be6', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_the_community).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, sacrificial_law_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_structure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_the_community).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, communal_funders).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, operational_readiness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and maintains the rulings that hold the sacrificial obligation in abeyance: that performance awaits restoration, that purity prerequisites are unmet, that Mount access is restricted, and that study of the sacrificial order is required in the interim. Sets yeshiva curricula, adjudicates restoration-conditions questions, and staffs the academies that keep the corpus current. Its standing as halakhic authority is exercised and renewed through this administration; stepping back would mean surrendering the frame through which it teaches, rules, and is recognized.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_structure, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority_structure, beneficiary).

% Devote their study to the sacrificial order — the tractates of Kodshim, purity law, and Temple service — teach it, and publish within it. They receive livelihood, communal standing, and state-supported stipends through the institutions that maintain this study. The capacity they keep alive is framed as the community's, but the position, income, and scholarly identity are theirs; leaving the field would cost them the specialization their standing rests on.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, sacrificial_law_scholars, beneficiary,
    moderate, biographical, identity_locked, global).

% Families of priestly descent hold the hereditary claim on service in a restored Temple. They maintain the disciplines that keep the claim live — lineage records, purity observance, training of sons — and receive recognition as the service's future custodians. Their stake is deferred: nothing material flows now except standing and preparation, and everything depends on a restoration they cannot hasten within the framework's own rules.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages, beneficiary,
    organized, generational, identity_locked, global).

% Will inherit whatever this arrangement preserves: the studied corpus, the trained personnel, the priestly readiness, and the suspended obligation itself. They are present only as the arrangement's intended recipients; no deliberation includes them, and whether the inheritance is a gift of capacity or a burden of resumed duty is decided entirely by others.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_the_community, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations_of_the_community, payer).

% Pay for the arrangement's upkeep: donations, tithes, and — since Israeli state support of yeshivot — taxes fund the academies, stipends, and the readiness apparatus. In return they receive continuity of the tradition, a liturgy that references the service as anticipated rather than lost, and membership in a community that expects restoration. They can redirect giving or leave the observant community at the cost of social ties and identity.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, communal_funders, payer,
    organized, biographical, constrained, global).

% Organized groups, concentrated in Israel, that reject the suspension as premature despair and work to restore sacrifice now: ascending the Mount where access permits, breeding red heifers, fabricating vessels, training priests on site. The ruling framework bars their project — majority halakhic opinion holds the purity prerequisites unmet. They operate inside the community's world but outside its ruling consensus, and their activity is what the enforcement machinery exists to restrain.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, temple_mount_activists, excluded,
    organized, immediate, constrained, regional).

% Study the arrangement from outside as a case of long-duration obligation maintenance: how a community keeps a law's operational apparatus alive across two millennia of non-performance, and what that maintenance does to and for the maintainers. They bear no costs and collect no benefits; their seat is analytic.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, comparative_religion_historians, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, sacrificial_law_scholars).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the community's operative relationship to the sacrificial law intact across a period when performance is impossible: the corpus is studied and taught, hypothetical cases adjudicated, priestly lines and purity disciplines maintained, so that knowledge, personnel, and procedure remain available if restoration comes. It also coordinates communal expectation — liturgy and calendar reference a service the community anticipates rather than one it treats as lost.
% TRANSFER_FUNCTION: Moves communal donations, tithes, and (after 1948) state funds to academies, stipends, and the readiness apparatus; moves scholarly vocation and standing to the study class and preserved service-claims to priestly lineages; returns to the community the continuity of the sacrificial tradition and a maintained anticipation of restoration.
% ABSENT_VOICES: Temple Mount activists are present in the world but outside the ruling consensus — the question 'should performance resume now?' has no legitimate seat inside the framework that answers it. Historically, mass restoration movements (the Sabbatean crisis foremost) forced the question and were excluded and suppressed rather than accommodated. Secular and Reform communities, for whom the corpus carries no operative claim, are likewise outside the conversation that sets the arrangement's terms.
% DISAPPEARANCE_RATIONALE: Study curricula would drop the sacrificial order from operative law within a generation; priestly readiness disciplines would lapse without their frame; Temple Mount activism would lose its halakhic counterweight and the question of performance would reopen in the streets rather than the responsa; and the community's relationship to the corpus would reorganize around one of the sibling readings. The arrangement is load-bearing for how the holding communities relate to the fixed text.
% FOUNDING_PROBLEM: After 70 CE the community carried a binding sacrificial obligation whose central rite had become impossible: how to remain bound to a law one cannot perform — neither voiding the law nor attempting an impossible performance — until restoration makes performance possible again.
% FOUNDING_PROBLEM_CORROBORATION: The gap the arrangement answers is attested from outside the beneficiary set: the Mishnah and Talmud's post-destruction discussions were composed across dispersed communities and include voices of those who lost most by the destruction; the rival readings (performance-only, study-as-exercise, symbolic-archive) all concede the performance gap while disputing its resolution, so the problem's existence does not rest on this reading's own testimony; and academic historiography of the post-70 period independently documents the adaptation. No party to the kernel contest denies the gap exists; the dispute is over what occupies it — which is itself corroboration that the founding problem, not merely the arrangement, is live.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low and rises slowly (0.06 → 0.30 across the interval): the arrangement coerces no performance and no study, but it channels real communal and state resources into study institutions and, after 1967, into a concrete readiness apparatus, and it preserves hereditary role-claims whose benefit is deferred. Suppression (scalar 0.18) is structural and low: participants may exit the observant community at social cost, and nothing within the arrangement traps them. The suppression_requirement series (0.05 → 0.33) tracks a different quantity — the enforcement machinery aimed at the excluded premature-performance fringe: trivial while performance was physically impossible, hardened after the Sabbatean crisis (the 1750 point), decaying as that movement collapsed (1948), then intensifying after 1967 when the Mount's capture made premature performance newly imaginable. Theater is low-moderate and rising: the study function genuinely preserves a corpus and a cadre, but the readiness apparatus increasingly performs readiness (vessel reconstruction, red-heifer programs) for a community whose restoration expectations it also sustains. All three series share one time grid (70, 250, 550, 850, 1150, 1450, 1750, 1948, 1967, 2025) so every metric is authored at every examined point. Accessibility collapse is low (0.30): the sibling readings remain live and practical alternatives — premature performance, abandonment — persist. Resistance is low (0.22): organized resistance comes only from the restoration-accelerationist fringe and episodic messianic movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as faithful custody of a received decree: the suspension is obedience, the study is service, the enforcement is protection of the restoration's preconditions. The excluded activist seat experiences the same structure as enforced deferral — a ruling that blocks the community's central rite indefinitely and brands acceleration as violation. The funder seat experiences it as a communal expense that returns identity and continuity; the scholar and priest seats experience it as vocation and inherited role; future generations cannot speak at all. The engine computes these per-seat divergences from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (future generations, priestly lineages, scholars, and secondarily the rabbinic authority structure) derive low d: the arrangement subsidizes them. Communal funders sit near symmetric — they pay for the apparatus but receive the continuity and identity the apparatus returns. Future generations are the subtlest case: beneficiaries of the preserved capacity who also inherit the suspended obligation itself, a prospective burden they never chose; their derived d sits near the beneficiary end, with the reservation documented in the inherited_obligation_burden omega. Temple Mount activists are excluded rather than extracted-from: the constraint's cost to them is the suppression of their project, which the scalar suppression and the enforcement series capture but the victim set does not (the reading's own lights declare no victim class during suspension, and descriptively no seat is deprived of what it is owed). One directionality override is declared: the institutional power atom (held by the rabbinic authority structure alone) is set to d=0.25 because the derivation would read a declared beneficiary with identity-locked exit as near-pure beneficiary (~0.1), while the structure also bears the adjudication and enforcement costs of holding the suspension against activist pressure — net beneficiary, materially offset.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a binding obligation whose central rite became impossible — is live: the Temple remains unbuilt and the declared terminus has not arrived, so founding_problem_status is live, no mandatrophy is declared, and the (live × world_rearranges) cell raises no zombie flag. The scaffold claim is what prevents misreading in both directions: reading the arrangement as a pure rope would erase its transitional self-understanding (its own justification is the coming restoration, not the steady state), while reading it as a snare would fabricate a victim set the suspension period does not contain. The eschatological_sunset_indeterminacy omega carries the real lifecycle risk: if the terminus is nominal, 'transition' hardens into steady state and the scaffold claim decays toward rope or, if readiness atrophies while maintenance continues, toward piton. That drift, not present extraction, is this arrangement's open question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eschatological_sunset_indeterminacy,
    'Does a condition-based sunset whose triggering event (messianic restoration) has no determinate date function as a genuine transitional terminus, or does indefinite deferral convert the arrangement into a de facto steady state?',
    'Comparative analysis of condition-based sunsets across institutions: examine whether communities holding this reading across fifty-plus generations exhibit terminus-approaching behavior (curricula contracting as readiness completes, readiness claims being retired) or steady-state institutionalization (constant curricula, expanding readiness apparatus).',
    'If the sunset is nominal, the arrangement drifts from scaffold toward rope (steady-state coordination) or, if readiness atrophies while maintenance continues, toward piton; if the terminus functions genuinely, the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eschatological_sunset_indeterminacy, conceptual, 'Whether the messianic terminus functions as a real sunset or a nominal one.').

omega_variable(
    readiness_function_genuineness,
    'What proportion of the readiness apparatus — sacrificial-law curricula, vessel reconstruction, priestly training, red-heifer programs — preserves genuine operational capacity versus performing readiness for the community''s own benefit?',
    'Expert halakhic assessment of whether the maintained corpus and training would suffice to resume the service, benchmarked against historical cases where long-lapsed practices were actually resumed.',
    'If readiness is largely nominal, theater_ratio is understated and the arrangement drifts toward piton; if genuine, the instrumental account of study holds and the low-extraction scaffold profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_function_genuineness, empirical, 'Genuine capacity preservation versus performative readiness.').

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the messianic_suspension_reading of sacrifice_obligation_kernel; the sibling readings locate the disagreement in the obligation''s status during the performance gap — what structural changes would adopting a sibling produce?',
    'Engine-side comparison across the four sibling stories: per-seat classifications, epsilon, and beneficiary/victim structures under each reading of the same kernel.',
    'study_as_exercise would remove the abeyance (study fulfills; no readiness function; different epsilon referent); symbolic_archive would remove the halakhic claim (heritage preservation, rope-like); performance_only would remove the suspension''s legitimacy (a standing unfulfilled obligation, higher suppression, likely a victim set of the conscientiously bound).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of a four-way kernel contest.').

omega_variable(
    premature_performance_enforcement_trajectory,
    'Will enforcement against premature performance (Mount access restrictions, readiness prerequisites) intensify with activist capacity, or decay as restoration politics gain salience?',
    'Track post-1967 ruling frequency, litigation over Mount access, and readiness-apparatus funding as proxies for the enforcement trajectory.',
    'Intensification raises suppression and pushes the arrangement toward enforced coordination with an excluded party; decay would lower suppression and shrink the excluded seat''s grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(premature_performance_enforcement_trajectory, empirical, 'Trajectory of enforcement against restoration accelerationism.').

omega_variable(
    inherited_obligation_burden,
    'Do future generations, who inherit both the preserved operational knowledge and the suspended obligation itself, hold a net beneficiary position or a burdened one?',
    'Intergenerational-preference analysis: whether descendants affirm or shed the inherited arrangement when exit becomes cheap, as in the emancipation-era defections.',
    'If the inheritance is net burden, the arrangement acquires a diffuse prospective victim class and future-generation d rises above the beneficiary-end derivation; if net benefit, the current profile stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inherited_obligation_burden, preference, 'Whether the preserved obligation is a gift or a burden to its inheritors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sok_messianic_suspension_tr_t70, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 70, 0.04).
narrative_ontology:measurement(sok_messianic_suspension_tr_t250, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 250, 0.08).
narrative_ontology:measurement(sok_messianic_suspension_tr_t550, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 550, 0.1).
narrative_ontology:measurement(sok_messianic_suspension_tr_t850, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 850, 0.13).
narrative_ontology:measurement(sok_messianic_suspension_tr_t1150, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1150, 0.15).
narrative_ontology:measurement(sok_messianic_suspension_tr_t1450, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1450, 0.16).
narrative_ontology:measurement(sok_messianic_suspension_tr_t1750, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement(sok_messianic_suspension_tr_t1948, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(sok_messianic_suspension_tr_t1967, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1967, 0.27).
narrative_ontology:measurement(sok_messianic_suspension_tr_t2025, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(sok_messianic_suspension_be_t70, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 70, 0.06).
narrative_ontology:measurement(sok_messianic_suspension_be_t250, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 250, 0.12).
narrative_ontology:measurement(sok_messianic_suspension_be_t550, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 550, 0.16).
narrative_ontology:measurement(sok_messianic_suspension_be_t850, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 850, 0.2).
narrative_ontology:measurement(sok_messianic_suspension_be_t1150, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1150, 0.22).
narrative_ontology:measurement(sok_messianic_suspension_be_t1450, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1450, 0.2).
narrative_ontology:measurement(sok_messianic_suspension_be_t1750, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1750, 0.15).
narrative_ontology:measurement(sok_messianic_suspension_be_t1948, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(sok_messianic_suspension_be_t1967, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1967, 0.27).
narrative_ontology:measurement(sok_messianic_suspension_be_t2025, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2025, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(sok_messianic_suspension_su_t70, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 70, 0.05).
narrative_ontology:measurement(sok_messianic_suspension_su_t250, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 250, 0.06).
narrative_ontology:measurement(sok_messianic_suspension_su_t550, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 550, 0.07).
narrative_ontology:measurement(sok_messianic_suspension_su_t850, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 850, 0.08).
narrative_ontology:measurement(sok_messianic_suspension_su_t1150, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1150, 0.1).
narrative_ontology:measurement(sok_messianic_suspension_su_t1450, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1450, 0.12).
narrative_ontology:measurement(sok_messianic_suspension_su_t1750, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1750, 0.2).
narrative_ontology:measurement(sok_messianic_suspension_su_t1948, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1948, 0.16).
narrative_ontology:measurement(sok_messianic_suspension_su_t1967, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(sok_messianic_suspension_su_t2025, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2025, 0.33).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'the sacrificial obligation' decomposes, after the Temple's destruction, into at least four structurally distinct constraints — one per reading of the kernel — with different epsilon referents, beneficiary/victim structures, and types. This story is the messianic_suspension member: obligation in divinely decreed abeyance, study instrumental. performance_only sits closest to the pre-destruction reference frame and is upstream of the others; this reading is the post-destruction halakhic settlement the siblings respond to; symbolic_archive is downstream and claims least. Family links are declared in affects_constraints; each sibling file carries the reciprocal note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
