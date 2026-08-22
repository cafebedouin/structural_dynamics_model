% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy Reading of War Powers Allocation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the war_powers_allocation kernel:
 *   the congressional-primacy claim that military force beyond immediate
 *   defense requires explicit congressional authorization as a constitutional
 *   necessity. The epsilon referent is the standing arrangement under contest
 *   — the operative executive-dominant war-initiation practice of the last
 *   half-century — assessed by this reading's own lights, NOT the
 *   authorization-requiring arrangement the reading endorses (which would
 *   score near zero by construction). Under that referent the reading sees a
 *   structure with a genuine coordination core (rapid unified response, the
 *   immediate-defense exception) entangled with systematic transfer of the
 *   war-initiation decision from the legislature to the executive, actively
 *   maintained through doctrinal innovation, precedent accumulation, and
 *   veto-backed defiance of termination instruments. The claim/metric
 *   independence is deliberate: claimed_type is authored from this reading's
 *   structural assessment (tangled_rope — both coordination and extraction,
 *   actively enforced), while the metrics are authored from the arrangement's
 *   observed operation. Suppression is authored as a raw structural property
 *   and is not scaled by power or scope; only extractiveness is scaled, by
 *   the engine, from directionality and scope. The three kernel readings are
 *   separate files linked through network.affects_constraints; this file does
 *   not average over them.
 *
 * KEY AGENTS:
 *   - - incumbent_presidency: Primary beneficiary and agenda-setter (institutional/arbitrage) — collects war-initiation discretion, administers the arrangement through Justice Department opinions and veto threats
 *   - - congress_as_institution: Primary target (institutional/trapped) — bears the transfer of war-initiation power each time force is used without its consent
 *   - - national_security_bureaucracy: Secondary beneficiary (institutional/constrained) — receives mission expansion and autonomy as the authorization step recedes
 *   - - deployed_service_members: Target (powerless/trapped) — bear the human costs of wars entered without collective deliberation
 *   - - taxpaying_public: Target (moderate/trapped) — bears fiscal and blood costs with consent solicited only retrospectively
 *   - - populations_in_strike_zones: Excluded party (powerless/trapped) — no seat in the allocation conversation their safety hangs on
 *   - - federal_courts: Analytical observer (institutional/analytical) — declines the core question as political, adjudicates margins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.78).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.75).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy Reading of War Powers Allocation").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '0dea8040-957f-45de-8e46-b94164b0e3fc').
narrative_ontology:cs_kernel_codification('0dea8040-957f-45de-8e46-b94164b0e3fc', fixed_text).
narrative_ontology:cs_authority_grounding('0dea8040-957f-45de-8e46-b94164b0e3fc', lineage).
narrative_ontology:cs_interpretation_layer_present('0dea8040-957f-45de-8e46-b94164b0e3fc').
narrative_ontology:cs_reading_relation('0dea8040-957f-45de-8e46-b94164b0e3fc', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('0dea8040-957f-45de-8e46-b94164b0e3fc', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('0dea8040-957f-45de-8e46-b94164b0e3fc', foundational, prior_congressional_authorization_constitutional_necessity).
narrative_ontology:cs_axiom_status(prior_congressional_authorization_constitutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('0dea8040-957f-45de-8e46-b94164b0e3fc', prior_congressional_authorization_constitutional_necessity, conventional).
narrative_ontology:cs_axiom('0dea8040-957f-45de-8e46-b94164b0e3fc', secondary, immediate_defense_sole_carveout).
narrative_ontology:cs_axiom_status(immediate_defense_sole_carveout, holdable).
narrative_ontology:cs_axiom_grounding('0dea8040-957f-45de-8e46-b94164b0e3fc', immediate_defense_sole_carveout, deontological).
narrative_ontology:cs_reference_frame('0dea8040-957f-45de-8e46-b94164b0e3fc', founding_congressional_war_initiation).
narrative_ontology:cs_drift_state('0dea8040-957f-45de-8e46-b94164b0e3fc', contemporary_unauthorized_force_routine, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0dea8040-957f-45de-8e46-b94164b0e3fc', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, incumbent_presidency).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, national_security_bureaucracy).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, congress_as_institution).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, deployed_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, taxpaying_public).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, inherent_article_ii_war_powers_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, political_question_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates and sustains military operations, decides when to seek authorization and when to proceed without it, and shapes the governing interpretation through Justice Department opinions, signing statements, and veto threats. Collects the discretion the arrangement confers: each unauthorized operation adds precedent. It cannot exit the constitutional structure it operates within, but it arbitrates its own boundaries by choosing which lawyers' opinions govern.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, incumbent_presidency, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, incumbent_presidency, beneficiary).

% Plans and executes the operations the arrangement enables; receives budget growth, mission expansion, and institutional autonomy as the prior-authorization step recedes. Career officers and civilians implement whichever interpretation prevails; individual exit means leaving service, and the institution as such has none.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, national_security_bureaucracy, beneficiary,
    institutional, generational, constrained, global).

% Holds the enumerated war powers — declarations, authorizations, army-raising, funding — and watches them migrate: operations begin without its consent, notification arrives after commitment, and its termination instruments (reporting clocks, funding levers) go unused or are defied without consequence. Its remedies — suit, defunding, veto override — are each blocked by doctrine, politics, or vote arithmetic. It cannot leave the structure; its recourse is the next election cycle and the next resolution.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress_as_institution, payer,
    institutional, generational, trapped, national).

% Execute the resulting operations under command authority they cannot decline. They bear injury, death, and post-service burdens from wars entered without collective deliberation, and their families absorb the remainder. Individual exit is legally unavailable short of desertion; collective voice surfaces only episodically through veterans' organizations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deployed_service_members, payer,
    powerless, biographical, trapped, global).

% Finances the operations and supplies the volunteers; learns of most commitments after they begin. Its consent is solicited retrospectively through elections years removed from the decisions. Exit means emigration; voice is diluted across a continental electorate with short attention horizons.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, taxpaying_public, payer,
    moderate, biographical, trapped, national).

% Live where the resulting operations land — drone and airstrike geographies across several regions. They are targeted, displaced, or bereaved by decisions made entirely outside any forum they can address, with no standing, representation, or notice. Their exclusion is total: they enter the process only as casualty figures or collateral estimates.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, populations_in_strike_zones, excluded,
    powerless, biographical, trapped, regional).

% Adjudicate the margins — detention, immunity, statutory construction of old authorizations — while declining the core allocation question as political; dismissed suits from legislators, soldiers, and taxpayers leave the arrangement's central bargain unreviewed. They observe the whole structure from a seat with no enforcement lever over it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, incumbent_presidency).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates the capacity to initiate military action in a single actor able to move within hours: unified command, operational secrecy, and rapid response to fast-moving threats are provided once, centrally, instead of through 535-member deliberation; the immediate-defense exception preserves instantaneous self-protection.
% TRANSFER_FUNCTION: Moves the war-initiation decision — and with it the attendant costs in lives, treasury, and political capital — from the legislature to the executive each time force is used without prior authorization; moves precedent and interpretive authority to the executive through signing statements, Justice Department opinions, and fait accompli deployments.
% ABSENT_VOICES: Populations in strike zones have no seat anywhere in the allocation conversation; rank-and-file legislators outside party leadership learn of operations after commitment; the constituency that once forced deliberation through conscription risk is structurally absent because the all-volunteer force insulates most households from the costs that used to compel attention.
% DISAPPEARANCE_RATIONALE: If the standing arrangement vanished overnight — if every use of force beyond immediate defense required prior explicit authorization, enforced — planned contingencies would stall pending congressional action, the executive would lose unilateral strike capability, alliance commitments premised on American dispatch would be repriced, and Congress would need standing rapid-authorization machinery it has never built. Nearly every seat rearranges: the presidency loses daily-exercised discretion, the bureaucracy loses mission tempo, the legislature inherits a decision load it has avoided for half a century.
% FOUNDING_PROBLEM: Enabling the nation to meet fast-moving external threats — nuclear-age surprise attack, then transnational terrorism — without the delay of assembling 535 deliberators, after Korea and Vietnam exposed both the dangers of executive free-wheeling and Congress's reluctance to own war decisions it had constitutionally inherited.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 1973 War Powers Resolution's findings and its passage over presidential veto attest both the speed-versus-deliberation problem and Congress's rival account of it; the Pacificus–Helvidius exchange records the founding-generation dispute in the first officeholders' own words; federal-court opinions acknowledge the allocation problem while declining to resolve it; former defense secretaries of both parties have testified that advance statutory authorization strengthens rather than hampers major operations. No corroboration exists for declaring the problem dead: the executive attests threat tempo, Congress attests a deliberation deficit, and they disagree on weight — which is precisely the contested finding recorded here.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the war-initiation decision has migrated decisively: Libya 2011 proceeded under a 'hostilities' definition engineered to sidestep the 60-day termination clock, later operations stretched two-decade-old authorizations past recognition, and no unauthorized use has ever been undone by congressional instrument. Suppression is high (0.75) because persistence depends on active maintenance — Office of Legal Counsel opinions, precedent stacking, veto threats against termination resolutions, and the political cost of opposing a deployed commander — not on participant preference; this scalar is authored unscaled, as a structural property. Theater is moderate-high (0.65): reporting filings, hearing cycles, and messaging resolutions consume most war-powers activity while producing almost no binding outcomes — the 2018 Yemen termination resolution passing both chambers and dying to veto is the emblematic case. Accessibility collapse is moderate (0.60): once the arrangement is understood, alternatives (judicial enforcement via standing, mid-operation defunding, override arithmetic) are mostly foreclosed, but Congress retains episodic real leverage — it granted authorizations in 1991, 2001, and 2002, and conditions funding at the margins. Resistance is moderate-high (0.58): the 1973 statute itself passed over a veto, and privileged resolutions recur every few years; the resistance is persistent and persistently ineffective. The temporal series run on one shared eight-point grid (1973–2026) so every metric is authored at every examined time point; the small 1991 dip in both extractiveness and suppression reflects the Gulf War's explicit authorization temporarily depressing the measured transfer — an episodic correction inside a long-run ratchet, not a cyclical oscillation. Trajectory drivers: post-1973 re-expansion (Grenada, Lebanon), the sweeping 2001 authorization, contested 2003 constructions, the 2011 hostilities redefinition, and the 2018–2026 normalization of veto-proof-by-fatigue unilateralism.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the presidency's position the arrangement is faithful execution of the command function under threat tempo it did not choose; from the legislature's position the same structure is serial dispossession of an enumerated power; from the bench it is a non-justiciable political question reviewed only at the margins; from the ranks it is simply orders. Same constitutional text, four incompatible experiences — the engine derives this divergence from the structural data (opposed roles at equal institutional power, radically different exit options), and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the presidency and the national-security bureaucracy toward the beneficiary end of directionality; victim declarations drive the legislature, service members, and the public toward the target end. Trapped exit pushes the targets further toward the full-target position — the legislature cannot secede from the structure, service members cannot decline deployment, the public cannot exit the polity — while the presidency's arbitrage-grade position (it selects which legal interpretation binds itself) holds it near the beneficiary end despite bearing nominal constitutional constraint. No directionality overrides are authored: the per-agent beneficiary/victim declarations already differentiate same-power seats (an institutional-power victim in the legislature versus an institutional-power beneficiary in the executive), which is exactly what the derivation chain reads; a power-atom-level override would collide across those seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents mislabeling in both directions. A pure-extraction reading would erase the arrangement's real coordination function — rapid unified response and the immediate-defense exception that even this reading concedes — and would predict abolition pressure that the historical record does not show (Congress keeps granting authorizations for major wars). A pure-coordination reading would erase the legislature's documented victimhood and the routinized defiance of termination instruments. The founding problem (speed versus deliberation under modern threat tempo) is contested rather than dead — both branches attest live versions of it — so no zombie-mandate flag fires, and the arrangement is not maintained as performance alone: the coordination core still functions, which is why theater_ratio at 0.65 signals decaying oversight rather than a fully hollowed shell. The live risk this story hands the drift detector is monotonic: extraction and suppression have risen together for five decades with no correcting instrument ever firing, which is the signature the engine should watch for transition toward pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the congressional_primacy_reading of the war_powers_allocation kernel — would the sibling readings (inherent_executive_reading, functional_accommodation_reading) restructure the victim set and the extraction assessment?',
    'Author the sibling stories as separate files and compare computed classifications: under the inherent-executive sibling the legislature exits the victim set entirely (bypass is legitimate exercise), and under the accommodation sibling the legislature enters the victim set only for prolonged unauthorized campaigns.',
    'Under the inherent-executive sibling, effective extraction collapses toward coordination-cost levels and the legislative victim structure disappears; under the accommodation sibling, extraction is confined to the prolonged-campaign zone. Only this reading yields the full legislative-victim structure authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; siblings are separate constraints sharing the referent.').

omega_variable(
    epsilon_referent_fixation,
    'Is epsilon authored for the standing executive-dominant arrangement (the referent under contest) rather than for the authorization-requiring arrangement this reading endorses?',
    'Re-read the beneficiary/victim declarations against the referent rule: they must describe who gains and who pays under the arrangement as it actually operates, not under the reading''s preferred alternative.',
    'If the referent drifted to the endorsed arrangement, epsilon would fall toward zero and the story would stop measuring the contest; fixation on the standing arrangement keeps the reading-indexed value comparable across sibling stories that share the referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_fixation, conceptual, 'Guards the reading-indexed-epsilon-over-fixed-referent rule for kernel stories.').

omega_variable(
    foreclosure_relation_stability,
    'Does the forecloses relation to inherent_executive_reading hold, or can ''immediate defense'' and ''imminent threat'' be reconciled so that the two readings coexist?',
    'Doctrinal analysis of whether any single constitutional framework can hold both ''explicit authorization is a categorical necessity beyond immediate defense'' and ''inherent authority to deploy without prior authorization''; test against the Youngstown concurrence structure and later separation-of-powers case law treating the premises as mutually exclusive.',
    'If reconcilable, the relation downgrades to coexists_with and the kernel contest becomes a pure preference dispute; if not, the engine''s foreclosure computation stands and the inherent-authority premise is structurally displaced within this reading''s framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_relation_stability, conceptual, 'Stability of the forecloses edge to the inherent-executive sibling reading.').

omega_variable(
    breach_vs_structure_extraction,
    'Is the measured extraction a property of the standing arrangement''s structure, or an accumulation of episodic breaches that the constitutional text itself already condemns?',
    'Count unauthorized uses of force sustained past congressional objection across the interval; if sustained breaches are rare and corrected, extraction is breach-noise over a coordination baseline; if routinized (Libya 2011 onward), extraction is structural.',
    'If breach-noise, epsilon drops toward coordination-cost levels and the arrangement computes nearer pure coordination; if structural, the tangled-rope assessment holds and drift toward pure extraction becomes the live risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breach_vs_structure_extraction, empirical, 'Whether routinized unauthorized force is structural extraction or correctable breach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1973, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1973, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1973, 0.22).
narrative_ontology:measurement_basis(war__tr_t1973, observed).
narrative_ontology:measurement(war__tr_t1983, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1983, 0.28).
narrative_ontology:measurement_basis(war__tr_t1983, observed).
narrative_ontology:measurement(war__tr_t1991, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1991, 0.26).
narrative_ontology:measurement_basis(war__tr_t1991, observed).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2001, 0.36).
narrative_ontology:measurement_basis(war__tr_t2001, observed).
narrative_ontology:measurement(war__tr_t2003, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2003, 0.42).
narrative_ontology:measurement_basis(war__tr_t2003, observed).
narrative_ontology:measurement(war__tr_t2011, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2011, 0.55).
narrative_ontology:measurement_basis(war__tr_t2011, observed).
narrative_ontology:measurement(war__tr_t2018, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2018, 0.62).
narrative_ontology:measurement_basis(war__tr_t2018, observed).
narrative_ontology:measurement(war__tr_t2026, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2026, 0.65).
narrative_ontology:measurement_basis(war__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1973, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1973, 0.45).
narrative_ontology:measurement_basis(war__be_t1973, observed).
narrative_ontology:measurement(war__be_t1983, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1983, 0.5).
narrative_ontology:measurement_basis(war__be_t1983, observed).
narrative_ontology:measurement(war__be_t1991, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1991, 0.46).
narrative_ontology:measurement_basis(war__be_t1991, observed).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement_basis(war__be_t2001, observed).
narrative_ontology:measurement(war__be_t2003, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2003, 0.66).
narrative_ontology:measurement_basis(war__be_t2003, observed).
narrative_ontology:measurement(war__be_t2011, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement_basis(war__be_t2011, observed).
narrative_ontology:measurement(war__be_t2018, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement_basis(war__be_t2018, observed).
narrative_ontology:measurement(war__be_t2026, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2026, 0.78).
narrative_ontology:measurement_basis(war__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1973, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement_basis(war__su_t1973, observed).
narrative_ontology:measurement(war__su_t1983, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1983, 0.46).
narrative_ontology:measurement_basis(war__su_t1983, observed).
narrative_ontology:measurement(war__su_t1991, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1991, 0.43).
narrative_ontology:measurement_basis(war__su_t1991, observed).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2001, 0.55).
narrative_ontology:measurement_basis(war__su_t2001, observed).
narrative_ontology:measurement(war__su_t2003, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2003, 0.61).
narrative_ontology:measurement_basis(war__su_t2003, observed).
narrative_ontology:measurement(war__su_t2011, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement_basis(war__su_t2011, observed).
narrative_ontology:measurement(war__su_t2018, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement_basis(war__su_t2018, observed).
narrative_ontology:measurement(war__su_t2026, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2026, 0.75).
narrative_ontology:measurement_basis(war__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'war powers' conflates three structurally distinct claims about one kernel (Article I section 8 enumeration versus Article II section 2 command). Decomposed per the epsilon-invariance principle: this file instantiates the congressional-primacy claim (categorical authorization necessity; legislature enters the victim set when bypassed); the inherent-executive sibling claims no prior-authorization requirement at all (legislature never a victim); the functional-accommodation sibling claims context-variance (victimhood confined to prolonged unauthorized campaigns). All three share the referent — the standing executive-dominant arrangement — and must be compared reading-indexed; upstream/downstream pressure runs from this reading's enforcement demands to the accommodation sibling's line-drawing problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
