% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   human_readable: Congressional Authorization Requirement for Military Force Beyond Immediate Defense (Congressional Primacy Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates the congressional-primacy reading of the
 *   war-powers kernel: military force beyond immediate defense requires
 *   explicit congressional authorization as a constitutional necessity. The
 *   standing arrangement under contest — the epsilon referent, assessed by
 *   this reading's own lights — is the allocation as it actually operates: an
 *   unambiguous textual grant to Congress (Article I, section 8), an
 *   enforcement statute (the 1973 War Powers Resolution, passed over Nixon's
 *   veto), and five decades of executive practice that routes around both —
 *   Korea as a United Nations 'police action,' Vietnam through the Tonkin
 *   Gulf resolution's pre-delegation, Kosovo and Libya 2011 through
 *   'hostilities' definitions, the 2001 authorization stretched across dozens
 *   of countries and two decades, and the 2025 strikes resting on narrow
 *   Article II theories. Under this reading the legislative branch is the
 *   bypass's victim: executive unilateral action extracts the war-initiation
 *   power the text vested in Congress. Claim and metrics are authored
 *   independently: the claim is tangled_rope — the arrangement carries a
 *   genuine deliberative-coordination function that has operated for major
 *   wars, layered with asymmetric extraction in the standing arrangement, and
 *   requiring active, costly enforcement to hold — while the metrics describe
 *   the arrangement's actual operation. Assumptions stated: the interval maps
 *   T=0 to 1950 (Korea, the first large bypass of the modern era) and T=75 to
 *   2025; the epsilon referent is the standing arrangement (text plus statute
 *   plus practice), never this reading's endorsed ideal; the sibling readings
 *   are separate constraint stories and appear nowhere inside this one's
 *   structural data.
 *
 * KEY AGENTS:
 *   - national_legislature: Designed beneficiary and bypass victim (institutional/trapped) — holds the war power the requirement allocates to it; loses it to executive bypass
 *   - executive_branch: Agenda-setter and standing-arrangement collector (institutional/arbitrage) — initiates force, defines its legal character, collects the extracted initiation power
 *   - deliberative_public: Designed beneficiary, gray-zone excluded (moderate/trapped) — the deliberative representation the requirement promises
 *   - deployed_service_members: Primary payer (powerless/trapped) — bear unauthorized deployments in person
 *   - federal_courts: Observer (institutional/analytical) — justiciability abstention closes the enforcement venue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.7).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Authorization Requirement for Military Force Beyond Immediate Defense (Congressional Primacy Reading)").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'c221296d-2047-4b5d-af7a-f1dcb828e294').
narrative_ontology:cs_kernel_codification('c221296d-2047-4b5d-af7a-f1dcb828e294', fixed_text).
narrative_ontology:cs_authority_grounding('c221296d-2047-4b5d-af7a-f1dcb828e294', lineage).
narrative_ontology:cs_interpretation_layer_present('c221296d-2047-4b5d-af7a-f1dcb828e294').
narrative_ontology:cs_reading_relation('c221296d-2047-4b5d-af7a-f1dcb828e294', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('c221296d-2047-4b5d-af7a-f1dcb828e294', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('c221296d-2047-4b5d-af7a-f1dcb828e294', foundational, war_initiation_excluded_from_commander_in_chief_grant).
narrative_ontology:cs_axiom_status(war_initiation_excluded_from_commander_in_chief_grant, holdable).
narrative_ontology:cs_axiom_grounding('c221296d-2047-4b5d-af7a-f1dcb828e294', war_initiation_excluded_from_commander_in_chief_grant, conventional).
narrative_ontology:cs_axiom('c221296d-2047-4b5d-af7a-f1dcb828e294', foundational, deliberative_consent_required_for_offensive_war).
narrative_ontology:cs_axiom_status(deliberative_consent_required_for_offensive_war, holdable).
narrative_ontology:cs_axiom_grounding('c221296d-2047-4b5d-af7a-f1dcb828e294', deliberative_consent_required_for_offensive_war, deontological).
narrative_ontology:cs_reference_frame('c221296d-2047-4b5d-af7a-f1dcb828e294', founding_era_congressional_war_primacy).
narrative_ontology:cs_drift_state('c221296d-2047-4b5d-af7a-f1dcb828e294', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c221296d-2047-4b5d-af7a-f1dcb828e294', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, national_legislature).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, deliberative_public).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, national_legislature).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, deployed_service_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, article_one_war_powers_exclusivity).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, commander_in_chief_subordination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the Article I war powers: declaring war, raising and supporting armies, regulating captures, funding the military. When the authorization requirement operates, it deliberates and votes on committing force, as it did in 1941 and 2001. When the executive initiates force without seeking authorization, its remedies are costly: conditioning appropriations is attacked as endangering troops already deployed, litigation is dismissed under political-question and standing doctrine, and censure is symbolic. It cannot exit the arrangement at all — it is a constitutional organ, not a participant with an outside.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, national_legislature, beneficiary,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, national_legislature, payer).

% Initiates military operations, defines their legal character through Office of Legal Counsel opinions and signing statements, and decides what counts as hostilities or immediate defense. Each operation launched without prior authorization and sustained past congressional objection adds to the initiation power it practically holds. Its way around the authorization demand is definitional arbitrage: stretching standing authorizations to new theatres, characterizing campaigns as limited, or resting on narrow Article II theories instead of a general inherent war power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, executive_branch, beneficiary).

% The constituency the authorization requirement was designed to serve: broad deliberative representation in the decision to go to war. It receives that representation for large wars — the 1941 declaration, the 2001 and 2002 debates — but for gray-zone operations (Kosovo, Libya 2011, the drone campaigns, the 2025 strikes) no deliberative moment arrives; whatever debate occurs happens after the fact between the White House and congressional leadership. Its recourse is the franchise: it can vote, but it cannot compel an authorization vote.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deliberative_public, beneficiary,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__congressional_primacy_reading, deliberative_public, excluded).

% Bear unauthorized conflicts in person: they deploy under stretched authorizations or none, bound by the Uniform Code of Military Justice, where refusal means court-martial. They have no seat in the decision that deploys them and no individual exit; their voice is collective — veterans' organizations, military-family voting blocs — and has not converted into an enforcement coalition.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, deployed_service_members, payer,
    powerless, biographical, trapped, global).

% Adjudicate separation-of-powers challenges to unauthorized force and dismiss them on political-question and standing grounds (Dellums v. Bush, Campbell v. Clinton). Their abstention closes the venue that would otherwise arbitrate the allocation, which makes their doctrine a load-bearing part of the standing arrangement even though they collect nothing from it and pay nothing for it.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates the war-initiation decision in a 535-member deliberative body so that committing the nation's blood and treasure requires broad assent, public debate, and recorded votes; it solves the collective-action problem that a single executive, facing crisis incentives and operational tempo, will otherwise set the nation's war posture alone.
% TRANSFER_FUNCTION: When honored, it moves the war decision from the executive to the legislature — decision authority, deliberation, and political accountability flow to Congress. In the standing arrangement the flow inverts: executive unilateral action moves initiation authority (and its political cover) from Congress to the presidency, while the costs of unauthorized conflicts — fiscal, human, reputational — flow to the public and to deployed forces.
% ABSENT_VOICES: Backbench legislators who would compel authorization votes but lack procedural vehicles (discharge petitions rarely reach the floor); the publics of gray-zone conflicts — Kosovo, Libya 2011, the drone campaigns, the 2025 strikes — for whom no deliberative moment ever arrived; deployed service members, who have no seat in the decision that deploys them. When an authorization conversation happens at all, it happens post-hoc between the White House and congressional leadership.
% DISAPPEARANCE_RATIONALE: War-initiation would consolidate in the presidency within one crisis cycle. Congress's declare-war and army-raising clauses would become ceremonial, the War Powers Resolution's clock would be meaningless, and the funding fights that occasionally bite (the 1973 Indochina cutoff, the Clark Amendment) would lose their constitutional anchor. The deliberative gate that produced the 1941 declaration and the 2001-2002 authorizations would vanish; every commitment of force would be one office's judgment, with no recorded vote and no institutional counterweight.
% FOUNDING_PROBLEM: The founding generation's problem was executive war-making: historical experience with monarchs commencing wars for personal or dynastic aggrandizement led the convention to reject vesting the war power in the president. Madison's convention notes record the determination that the power to commence war belongs to the legislature, with the executive limited to repelling sudden attacks; the arrangement was built so that committing the nation to war requires the deliberation of the branch closest to the people and structurally incapable of unilateral action.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the congressional beneficiary set: Hamilton's Federalist 69 concedes the president's command authority operates in subordination to the legislative war power; Madison's Helvidius essays attest the anti-aggrandizement purpose; and the modern executive corroborates it by practice — administrations that dispute the requirement's boundary still seek authorizations for wars they expect to be large (2001, 2002), conceding that major war requires congressional assent. No source inside the beneficiary set is needed to establish the founding problem's liveness.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.65: substantial but not total — the deliberative gate has operated where the executive expected large wars (the 1941 declaration, the 1991 and 2002 authorizations), while a persistent gray zone (Korea, Kosovo, Libya, the 2001 authorization's drift, the 2025 strikes) transfers initiation power to the presidency. Suppression is 0.70 and is a raw structural property, unscaled by power or scope: the requirement forecloses the open inherent-authority claim heavily — no modern administration rests a major campaign on a general executive war power; even unilateralist justifications dress themselves in authorization-adjacent language. That suppression coexists with decaying enforcement: the theater series rises (0.20 to 0.55) as notification ritualizes and the 60-day clock, never once triggered into withdrawal in fifty-two years, becomes performance — Goodhart drift from authorization to compliance signaling. Accessibility collapse is 0.45: alternatives persist asymmetrically — the executive's evasion channel (definitional arbitrage, authorization stretching) is wide open while Congress's enforcement routes (courts closed by political-question doctrine) are collapsed. Resistance is 0.60: the executive resists the requirement persistently; Congress, scholars, and litigants resist the bypass. The three series share one time grid and show one reform-relaxation-accumulation cycle rather than periodic oscillation: Vietnam crisis, the 1973 reform (extraction dips at T=30 while enforcement peaks), post-Cold-War relaxation, post-9/11 accumulation. The suppression series tracks the open claim's foreclosure, which strengthens even as enforcement capacity decays — the arrangement wins the doctrinal war while losing the practical one, and compliance performance substitutes for compliance.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from the same structure. From Congress's seat the arrangement is a violated allocation — the power the text vested in it is exercised by another branch, with its enforcement venues closed; from the executive's seat the same structure is an unworkable formalism that operational tempo forces it to evade, and its arbitrage exits make the requirement's bite negotiable. Deployed service members experience commitment under contested authority with no exit; the public experiences a deliberative promise honored only for large wars; the courts, by abstaining, hold the seat whose doctrine keeps the dispute non-justiciable. Congress and the executive hold the same nominal power atom (institutional) but different exits — trapped versus arbitrage — which is what separates their computed positions despite equal standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress appears in both structural arrays by design: it is the requirement's designed beneficiary (when honored, it holds and exercises the war power, damping its effective extraction) and the standing arrangement's victim (when bypassed, its war power is what gets taken, amplifying it toward the target end). Net, under this reading's assessment of current practice, Congress sits nearer the target end. The executive is agenda-setter with a beneficiary secondary role and arbitrage-grade exit: it collects the initiation power through bypass and exits the requirement's bite through definitional moves, placing it near the beneficiary end. Deployed service members are full targets: they pay in person with no exit; their coalition potential (veterans' organizations, military-family electoral weight) is real but historically unrealized, and the victims are institutionally fragmented rather than concentrated. The deliberative public is a damped beneficiary whose benefit is intermittently withheld — its excluded secondary role marks the gray-zone operations where the promised deliberation never arrives. The courts collect and pay nothing; their abstention is structural context, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure coordination would hide the bypass extraction that the victim declarations keep visible — the deliberative gate is real, but a growing share of the arrangement's activity defends and performs compliance rather than delivering it (theater 0.55 and rising). Reading it as pure extraction would erase the coordination function that still operates for major wars and would misdate the arrangement's decay. The founding problem — one office committing the nation to war — is live, and the arrangement matters if it vanished (world_rearranges), so there is no mandatrophy: the mandate has not outlived its function. What has decayed is enforcement capacity while doctrinal suppression strengthened — a divergence, not an obsolescence. The theater series exists to catch the moment that divergence completes and the arrangement becomes performance; the fixing_cost judgment (prohibitive) records why no seat has repaired it: enforcement requires overcoming veto thresholds, filibuster, and the political cost of constraining a commander mid-deployment, and the constitutional-amendment route is unreachable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This story is the congressional-primacy reading of the war_powers_allocation kernel; the disagreement with its siblings is located in the commander-in-chief clause''s scope and the fixity of the immediate-defense boundary — which allocation does the standing arrangement actually instantiate?',
    'Cross-reading corpus comparison of the three sibling stories under identical practice data; a justiciable separation-of-powers decision, or sustained congressional enforcement success, would reveal which allocation the practice has converged on.',
    'If the inherent-executive reading is structurally dominant, Congress exits the victim set, extraction inverts (congressional constraint attempts become the extractive move), and this story''s tangled_rope claim misdescribes the kernel''s settled state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Which kernel reading the standing arrangement instantiates; sibling readings relocate the victim set.').

omega_variable(
    immediate_defense_boundary_ambiguity,
    'Where does ''immediate defense'' end? Every bypass rides this carve-out — Korea as a United Nations ''police action,'' Libya 2011 as ''no hostilities,'' the 2025 strikes as self-defense — so is the exception a narrow repelling-sudden-attacks boundary (Madison''s gloss) or an open-ended imminence doctrine?',
    'Doctrinal elaboration by Congress (statutory hostilities definitions) or judicial construction in a justiciable case; empirical tracking of which operations the executive itself treats as authorization-requiring.',
    'A narrow boundary raises measured extraction toward the pure-extraction end (nearly everything bypassed); a broad boundary lowers it toward coordination (bypass confined to genuine defense), moving the computed type accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immediate_defense_boundary_ambiguity, conceptual, 'The load-bearing ambiguity of the immediate-defense carve-out that every bypass rides.').

omega_variable(
    doctrinal_suppression_vs_enforcement_decay,
    'Does the constraint''s high suppression of inherent-authority claims rest on self-enforcing doctrinal consensus, or on enforcement machinery that is measurably decaying while the open claim stays suppressed?',
    'Track whether administrations seek authorization when they believe Congress would refuse: if bypass proceeds identically regardless of doctrinal posture, the suppression rests on nothing enforcement-independent.',
    'If enforcement-dependent, the rising suppression series overstates durability and the enforcement layer decays toward a shell even as the claim stays suppressed; if doctrinally self-enforcing, the suppression is durable and the theater series is the only decay signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_suppression_vs_enforcement_decay, empirical, 'Whether suppression of inherent-authority claims rests on doctrine or on decaying enforcement machinery.').

omega_variable(
    wpr_clock_functionality,
    'Is the War Powers Resolution''s machinery functional or theatrical — the 60-day clock has never once forced withdrawal in fifty-two years, yet notification imposes real timing and publicity costs on operations.',
    'Counterfactual comparison of gray-zone operations before and after 1973 enactment, and of notified versus unnotified (covert) operations: if notified and unnotified operations show the same shape and duration, the machinery is performance.',
    'If purely theatrical, the theater_ratio is understated and the enforcement layer is an inert shell inside the arrangement; if functional, part of the measured theater is real constraint and the notification costs are genuine coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wpr_clock_functionality, empirical, 'Whether the War Powers Resolution machinery is functional constraint or theatrical compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wp_primacy_tr_t0, war_powers_allocation__congressional_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(wp_primacy_tr_t0, observed).
narrative_ontology:measurement(wp_primacy_tr_t5, war_powers_allocation__congressional_primacy_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(wp_primacy_tr_t5, observed).
narrative_ontology:measurement(wp_primacy_tr_t15, war_powers_allocation__congressional_primacy_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(wp_primacy_tr_t15, observed).
narrative_ontology:measurement(wp_primacy_tr_t30, war_powers_allocation__congressional_primacy_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(wp_primacy_tr_t30, observed).
narrative_ontology:measurement(wp_primacy_tr_t45, war_powers_allocation__congressional_primacy_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement_basis(wp_primacy_tr_t45, observed).
narrative_ontology:measurement(wp_primacy_tr_t60, war_powers_allocation__congressional_primacy_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(wp_primacy_tr_t60, observed).
narrative_ontology:measurement(wp_primacy_tr_t68, war_powers_allocation__congressional_primacy_reading, theater_ratio, 68, 0.52).
narrative_ontology:measurement_basis(wp_primacy_tr_t68, observed).
narrative_ontology:measurement(wp_primacy_tr_t75, war_powers_allocation__congressional_primacy_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement_basis(wp_primacy_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(wp_primacy_be_t0, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(wp_primacy_be_t0, observed).
narrative_ontology:measurement(wp_primacy_be_t5, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(wp_primacy_be_t5, observed).
narrative_ontology:measurement(wp_primacy_be_t15, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(wp_primacy_be_t15, observed).
narrative_ontology:measurement(wp_primacy_be_t30, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(wp_primacy_be_t30, observed).
narrative_ontology:measurement(wp_primacy_be_t45, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 45, 0.54).
narrative_ontology:measurement_basis(wp_primacy_be_t45, observed).
narrative_ontology:measurement(wp_primacy_be_t60, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(wp_primacy_be_t60, observed).
narrative_ontology:measurement(wp_primacy_be_t68, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 68, 0.63).
narrative_ontology:measurement_basis(wp_primacy_be_t68, observed).
narrative_ontology:measurement(wp_primacy_be_t75, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement_basis(wp_primacy_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(wp_primacy_su_t0, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(wp_primacy_su_t0, observed).
narrative_ontology:measurement(wp_primacy_su_t5, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(wp_primacy_su_t5, observed).
narrative_ontology:measurement(wp_primacy_su_t15, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(wp_primacy_su_t15, observed).
narrative_ontology:measurement(wp_primacy_su_t30, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(wp_primacy_su_t30, observed).
narrative_ontology:measurement(wp_primacy_su_t45, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement_basis(wp_primacy_su_t45, observed).
narrative_ontology:measurement(wp_primacy_su_t60, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(wp_primacy_su_t60, observed).
narrative_ontology:measurement(wp_primacy_su_t68, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 68, 0.69).
narrative_ontology:measurement_basis(wp_primacy_su_t68, observed).
narrative_ontology:measurement(wp_primacy_su_t75, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 75, 0.7).
narrative_ontology:measurement_basis(wp_primacy_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'war powers' decomposes into three structurally distinct readings of one kernel (war_powers_allocation): congressional primacy (this file), inherent executive authority, and functional accommodation. Their epsilon values differ because their victim sets differ: this reading places the legislative branch in the victim set when bypassed and locates extraction in executive unilateralism; the inherent-executive reading would remove Congress from the victim set entirely and, where it registers extraction at all, would locate it in congressional attempts to constrain the commander; the accommodation reading makes the victim set context-dependent on the imminence/protraction boundary. Structural topology: this reading and the inherent-executive reading are the two poles; the accommodation reading is the negotiated middle whose boundary moves with the poles' relative doctrinal strength. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
