% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection Limited by Demonstrable Unconsented Harm
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The harm-limited reading of First Amendment protection establishes that
 *   speech receives constitutional protection unless it causes demonstrable
 *   unconsented-to harm. This reading contrasts with the absolutist reading
 *   (no law means no law) and the categorical-balancing reading (case-by-case
 *   value-harm balancing without a categorical harm requirement). The
 *   harm-limited reading narrows the protected speech set by placing burden
 *   on regulators to show harm, but expands protection for vulnerable
 *   populations by permitting restriction once harm is proven. The constraint
 *   operates as tangled rope: genuine coordination function (robust discourse
 *   + protection from silencing), but asymmetric extraction where speakers
 *   whose expression causes harm face restriction while vulnerable
 *   populations gain protection mechanism. The claim/metric independence is
 *   deliberate — this reading is CLAIMED as coordination but authored metrics
 *   show substantial extractiveness and suppression because the harm-boundary
 *   inquiry itself is contestable and becomes a locus for regulatory
 *   authority expansion.
 *
 * KEY AGENTS:
 *   - vulnerable_minorities: benefit from harm-based protection mechanism; exit is trapped (cannot leave the harm)
 *   - speakers_causing_demonstrable_harm: bear the restriction cost; exit is constrained (modify speech or face restriction)
 *   - regulatory_authorities and judiciary: set/enforce the harm boundary; have mobile exit (interpret doctrine differently)
 *   - absolutist_speakers (excluded): would contest the entire reading; argue harm balancing is content censorship in disguise
 *   - harm_bearing_populations: positioned to benefit if harm-proof succeeds; exit is constrained (burden is on them to show harm)
 *   - speech_boundary_uncertainty_bearers: pay the cost of the harm-standard ambiguity; face chilling effects from uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.62).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection Limited by Demonstrable Unconsented Harm").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'c47e52c7-a81b-416a-8337-e0d153ff1dc1').
narrative_ontology:cs_kernel_codification('c47e52c7-a81b-416a-8337-e0d153ff1dc1', fixed_text).
narrative_ontology:cs_authority_grounding('c47e52c7-a81b-416a-8337-e0d153ff1dc1', lineage).
narrative_ontology:cs_interpretation_layer_present('c47e52c7-a81b-416a-8337-e0d153ff1dc1').
narrative_ontology:cs_reading_relation('c47e52c7-a81b-416a-8337-e0d153ff1dc1', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c47e52c7-a81b-416a-8337-e0d153ff1dc1', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('c47e52c7-a81b-416a-8337-e0d153ff1dc1', foundational, demonstrable_harm_necessary_condition).
narrative_ontology:cs_axiom_status(demonstrable_harm_necessary_condition, holdable).
narrative_ontology:cs_axiom_grounding('c47e52c7-a81b-416a-8337-e0d153ff1dc1', demonstrable_harm_necessary_condition, empirically_contingent).
narrative_ontology:cs_axiom('c47e52c7-a81b-416a-8337-e0d153ff1dc1', foundational, speech_may_restrict_speech_equally).
narrative_ontology:cs_axiom_status(speech_may_restrict_speech_equally, holdable).
narrative_ontology:cs_axiom_grounding('c47e52c7-a81b-416a-8337-e0d153ff1dc1', speech_may_restrict_speech_equally, deontological).
narrative_ontology:cs_reference_frame('c47e52c7-a81b-416a-8337-e0d153ff1dc1', categorical_speech_protection_doctrine).
narrative_ontology:cs_drift_state('c47e52c7-a81b-416a-8337-e0d153ff1dc1', contemporary_online_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c47e52c7-a81b-416a-8337-e0d153ff1dc1', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, harm_bearing_populations).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_causing_demonstrable_harm).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speech_boundary_uncertainty_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Targeted by speech that causes documented harm: harassment, incitement, coordinated intimidation, or speech creating conditions that materially restrict their ability to participate in civic life or access public accommodations. The harm-limited reading extends First Amendment protection to them by permitting regulation when demonstrable harm is proven. Their exit from harm is constrained — they cannot simply leave the jurisdiction or disengage from the speech environment.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Face restrictions on expression when their speech demonstrably causes unconsented-to harm. The harm-limited reading requires proof of harm (not just offense or disagreement) and directness of causation, but once established, permits regulation. Their exit option is to modify or cease the harmful speech; the framing assumes the harm itself is the legitimating fact for restriction.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_causing_demonstrable_harm, payer,
    moderate, biographical, constrained, national).

% Adjudicate what counts as demonstrable harm and whether speech crosses that threshold. Under this reading they have authority to restrict speech once harm is shown, but must conduct the harm-proof inquiry and cannot regulate mere offense, disagreement, or ideological alignment with undesired groups. Their burden is to establish the harm causation chain, not merely the speaker's intent or the disfavored message.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, regulatory_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Hold the position that First Amendment protection is categorical and that harm balancing constitutes impermissible regulation. They are not stakeholders TO this constraint but would object if present, arguing that the harm-limited reading subordinates constitutional protection to empirical contestation and creates a regulatory opening that will be exploited to silence disfavored speech regardless of actual harm.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_speakers, excluded,
    organized, biographical, mobile, national).

% Are positioned to receive protection via the harm-inquiry mechanism. Their status depends on proving demonstrable harm — the reading offers protection if harm is shown, but leaves the burden and proof standard on those bearing the harm. They are distinct from vulnerable_minorities where they include organized groups with moderate power that can mount harm-showing campaigns.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, harm_bearing_populations, beneficiary,
    moderate, biographical, constrained, national).

% Interprets what counts as demonstrable harm and applies the standard in adjudication. Their role is to operationalize the harm boundary, distinguishing it from offense, disfavor, or disagreement. They function as both enforcer (deciding cases) and commentator (shaping doctrine through reasoning). The harm standard gives them significant discretionary authority in determining what harms qualify.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, judiciary_interpreters, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__harm_limited_reading, judiciary_interpreters, observer).

% Bear the costs of the harm-boundary inquiry itself: uncertainty about where the line sits, chilling effects from fear of liability or restriction, and the necessity of conducting harm-showing procedures. This includes speakers who cannot know in advance whether their speech will be found to cause demonstrable harm, and the broader discourse community navigating a contested harm standard.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speech_boundary_uncertainty_bearers, payer,
    moderate, biographical, constrained, national).

% International and comparative-constitutional observers noting that most established democracies (Canada, Germany, UK) already operate under harm-limitation principles for speech, with varying proof standards for what counts as demonstrable harm. They provide structural reference points for evaluating whether the harm-limited reading is a distinctive American commitment or convergence with democratic practice elsewhere.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, observer_comparative_traditions, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__harm_limited_reading, regulatory_authorities).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances two competing coordination problems: (1) enabling robust public discourse without self-censorship or prior restraint (the foundational First Amendment function), and (2) protecting vulnerable populations from speech that materially restricts their civic participation or access to public space. The harm-limited reading solves by introducing a damage-proof requirement: protected speech is coordinated by the rule that restriction is permitted only when harm is demonstrable and causally connected.
% TRANSFER_FUNCTION: Transfers the burden and costs of harm-proof from vulnerable minorities (who bear unregulated harm) to speakers and regulatory authorities (who must conduct the inquiry or modify speech). It moves dispute-resolution authority from prior restraint regimes to empirical harm-showing procedures, shifting which parties control the restriction narrative.
% ABSENT_VOICES: Absolutist First Amendment interpreters would object if present, arguing that any harm-balancing framework subordinates constitutional protection to contestable empirical claims and creates a regulatory opening. Content-moderation practitioners argue the harm standard is too strict and excludes harassment patterns that are coordinated but not individually traceable. Some minority speakers argue the harm framework can be weaponized to restrict minority perspectives if harm is framed expansively.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished and absolute categorical protection returned (absolutist reading), speakers currently restricted on harm-showing grounds would resume expression, vulnerable minorities would lose the protection mechanism, and courts would revert to categorical exceptions rather than empirical damage inquiry. Conversely, if the harm-limited reading were replaced by a balancing regime with lower proof standards (categorical_balancing reading), regulatory authority would expand and more speech would be subject to restriction. The constraint's presence affects who can restrict what and by what procedure.
% FOUNDING_PROBLEM: Early First Amendment doctrine protected speech categorically but failed to account for speech that materially prevents others from speaking or participating in civic life (targeted harassment, coordinated campaigns, speech that creates conditions closing off public participation). The harm-limited reading emerged to address the discovery that absolute protection could enable speech that silences others, creating an exclusion from the very framework meant to protect inclusive discourse.
% FOUNDING_PROBLEM_CORROBORATION: Scholars in critical theory and civil rights law attest that speech-based harassment and coordinated intimidation campaigns do materially restrict vulnerable populations' access to public space and civic participation (e.g., bomb threats closing universities, coordinated campaigns preventing women from speaking). Absolutist First Amendment interpreters contest this framing, arguing that harm claims are often covers for content-based censorship. Comparative constitutional law shows Canada, Germany, and other democracies have operationalized harm standards for speech, suggesting the founding problem is recognized across democratic traditions. Legislative testimony from civil rights organizations and empirical data on targeted harassment campaigns corroborate the harm-silencing mechanism from outside the judiciary.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.42 (reading is early and narrowly applied) and rises to 0.58 (regulatory authority expands to apply harm standard more broadly). Suppression stays higher than extractiveness because maintaining the harm boundary requires active adjudication and exclusion of speakers who fail to meet proof standard — the constraint's persistence depends on courts resisting pressure to expand harm definitions. Theater rises from 0.22 to 0.41 because courts increasingly perform harm inquiries that are legitimating rituals for decisions made on other grounds (political affiliation of speaker, unpopularity of message framed as harm). The measurement series track the reading's maturation: early projections based on theoretical adoption, later observations as the harm standard gets operationalized in case law. The plateau around t=15 suggests the reading reaches a stable operational point where extractiveness and suppression stabilize.
 *
 * PERSPECTIVAL GAP:
 *   Vulnerable minorities and speakers causing harm should compute as different types entirely: beneficiaries computing rope-like (low extraction, coordination benefit) while speakers compute snare-like (high extraction, constrained exit). The judicial seat computes as agenda-setter in a tangled-rope structure because it holds both coordination function (defining harm boundary) and extractive authority (restricting speech). The absolutist excluded position would compute as victim if included, which is precisely why the harm-limited reading's persistence depends on keeping absolutist objections structurally marginal to the adjudicatory process.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality (vulnerable_minorities, harm_bearing_populations if harm is successfully shown) is low because protection flows without behavioral modification required on their part — the constraint subsidizes their civic participation. Victim directionality (speakers_causing_demonstrable_harm) is high because the constraint restricts their expression conditionally on harm-proof; they bear the regulatory burden. Regulatory authorities derive d from their role as both enforcer and gate-setter: they benefit from the authority structure but also depend on the harm standard holding coherently. The derived d for authorities should cluster around 0.4-0.5 (symmetric, slight toward beneficiary) because they gain institutional power but lose legitimacy if the harm standard becomes obviously pretextual.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm-limited reading prevents misclassification in two directions: (1) it blocks treating pure-extraction regimes (where harm is fabricated retrospectively) as coordination, because the reading's core claim is that harm must be demonstrable and causally proven, not merely asserted; and (2) it blocks treating genuine coordination (protecting inclusive discourse from silencing) as pure extraction, because the reading recognizes that some restrictions enable rather than disable participation. The mandatrophy resolution turns on whether the harm standard is applied honestly (distinguishing demonstrable harm from mere offense/disfavor) or becomes a pretextual vehicle for content-based censorship. If the latter, the reading reclassifies from tangled-rope (asymmetric but justified by coordination) toward snare (pure extraction disguised as harm-prevention). The theater-ratio rise (0.22 to 0.41) models the drift toward performative harm-inquiry: courts conducting harm investigations increasingly as legitimating theater for decisions made on other grounds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_operationalization,
    'What counts as demonstrable, causally-proven harm under the harm-limited reading? Where does the harm boundary sit between offense, disagreement, and materially-restricting harm?',
    'Doctrine maturation through case law; comparative study of jurisdictions that operationalize harm standards (Canada, Germany, UK); empirical research on whether harm-inquiry procedures consistently distinguish demonstrable harm from pretextual restriction.',
    'If harm becomes standardly interpreted as material impediment to civic participation or access (narrow reading), the constraint holds as genuine protection mechanism. If harm expands to include offense, disfavor, or rhetorical disagreement (broad reading), the constraint reclassifies toward snare (pretextual extraction). The boundary placement determines whether directionality derivation serves true coordination or becomes cover for censorship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_operationalization, empirical, 'Whether demonstrable harm remains a specific factual boundary or becomes elastic under pressure from regulatory expansion.').

omega_variable(
    proof_burden_allocation_contestation,
    'Who bears the burden of proving harm — vulnerable populations, regulatory authorities, or some shared inquiry? Does burden placement affect whether restriction-eligible speech actually gets restricted?',
    'Empirical analysis of harm-showing procedures in cases; data on disparities between groups in their success rates at proving harm; comparative standards across jurisdictions showing different burden allocations.',
    'High burden on vulnerable populations (current doctrine) may leave them unable to access the protection mechanism (effective denial of the beneficiary promise). High burden on regulators may insulate speakers from restriction even when harm is real (effective absolutism). The burden allocation determines whether the constraint''s extraction is symmetrical (both sides equally bound by procedure) or asymmetrical (burden concentrated on one side).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proof_burden_allocation_contestation, empirical, 'Whether the harm-showing procedure is accessible to vulnerable populations or becomes a gatekeep favoring speakers.').

omega_variable(
    harm_vs_silencing_equivalence,
    'Is speech that causes demonstrable harm structurally equivalent to the silencing harm it causes vulnerable populations? Or are the two harms qualitatively different such that restricting harmful speech does not equally silence speakers?',
    'Comparative analysis of speech restrictions'' effects on speaker voices vs. targeted-population civic participation; case studies of whether restriction of harmful speech re-enables vulnerable-population participation.',
    'If harms are structurally equivalent, the constraint is symmetric extraction (speaker restriction = vulnerable-population restriction, merely directionally opposed). If harms are asymmetric (restricting harmful speech enables participation while restriction of defensive speech disables it), the constraint is asymmetric tangled-rope (coordination justifies differential treatment). This determines whether the constraint''s extraction is justified by genuine coordination function or is pretextual asset-stripping from speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_vs_silencing_equivalence, conceptual, 'Whether harm-based restriction is symmetric censorship or asymmetric enablement of inclusive discourse.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the harm-limited reading foreclose or coexist with the absolutist reading? Can a framework hold both ''no law'' as categorical and ''harm permits restriction'' as operative?',
    'Constitutional-doctrinal analysis of whether the two readings can be held simultaneously by one interpretive authority; historical study of whether courts have switched between readings or held them in tension.',
    'If the readings foreclose each other, adopting the harm-limited reading eliminates the absolutist position as a live option within the same legal framework (rare). If they coexist, different courts/parties hold different readings simultaneously and the constraint''s operation depends on which reading governs (more common). The relationship determines whether the constraint is a replaced doctrine (foreclosed) or a live contestation (coexisting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the harm-limited reading and the absolutist reading can coexist in a single constitutional framework or whether one necessarily rules out the other.').

omega_variable(
    vulnerable_population_definition_drift,
    'Is the set of vulnerable populations eligible for harm-based protection stable, or does it expand and contract with political cycles and coalition shifts?',
    'Historical analysis of which groups courts have recognized as harm-eligible in different eras; comparative study across jurisdictions of consistency in vulnerable-population definitions.',
    'Stable population definition supports the reading''s coordination framing (genuine protection mechanism). Drift in definition suggests the reading is becoming a vehicle for partisan control of which speech is restricted (extraction mechanism, not coordination). Wide definition-drift would suggest reclassification toward snare or piton (theater-driven inertia).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_definition_drift, empirical, 'Whether vulnerable-population definitions remain stable or drift with political convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(firs_tr_t0, projected).
narrative_ontology:measurement(firs_tr_t5, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(firs_tr_t5, projected).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(firs_tr_t10, observed).
narrative_ontology:measurement(firs_tr_t15, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(firs_tr_t15, observed).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(firs_tr_t20, observed).
narrative_ontology:measurement(firs_tr_t25, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(firs_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(firs_be_t0, projected).
narrative_ontology:measurement(firs_be_t5, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(firs_be_t5, projected).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(firs_be_t10, observed).
narrative_ontology:measurement(firs_be_t15, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement_basis(firs_be_t15, observed).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(firs_be_t20, observed).
narrative_ontology:measurement(firs_be_t25, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(firs_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(firs_su_t0, projected).
narrative_ontology:measurement(firs_su_t5, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(firs_su_t5, projected).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(firs_su_t10, observed).
narrative_ontology:measurement(firs_su_t15, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(firs_su_t15, observed).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(firs_su_t20, observed).
narrative_ontology:measurement(firs_su_t25, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(firs_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__harm_limited_reading, 0.12).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the first_amendment_speech_protection kernel. The harm-limited reading instantiates speech protection as contingent on demonstrable harm, contrasting with absolutist categorical protection and with balancing-based categorical exclusion. All three readings share the same kernel (the First Amendment text) but instantiate different ε values and beneficiary/victim structures. The harm-limited reading's higher extraction (0.58) reflects the contestability of harm standards and the regulatory authority required to adjudicate harm, while the absolutist reading would show near-zero extraction (categorical protection requires no harm inquiry). Network effects propagate through the kernel: if absolutist doctrine gains ground, harm-limited protection loses authority; if balancing expands, harm-limited boundaries contract.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_speech_protection__harm_limited_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
