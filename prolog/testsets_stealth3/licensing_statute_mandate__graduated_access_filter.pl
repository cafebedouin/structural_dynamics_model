% ============================================================================
% CONSTRAINT STORY: licensing_statute_mandate__graduated_access_filter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_licensing_statute_mandate__graduated_access_filter, []).

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
 *   constraint_id: licensing_statute_mandate__graduated_access_filter
 *   human_readable: Statutory Credential Mandates as Graduated Access Filter
 *   domain: labor economics/regulatory policy/public administration
 *
 * SUMMARY:
 *   Occupational licensing statutes require a state-granted credential as a
 *   condition of lawful practice in a widening roster of occupations. This
 *   story instantiates the graduated_access_filter reading of that statutory
 *   kernel: it treats the operative function of the credential mandate as a
 *   tiered access filter whose binding incidence falls along class and
 *   prior-resource lines — who can finance the required clock-hours, survive
 *   years of unpaid supervised practice, and clear discretionary character
 *   gates — rather than along any measured risk the occupation poses. On this
 *   reading the public-safety rationale functions as the arrangement's public
 *   justification while the enforced mechanism sorts entrants by wealth and
 *   background. The epsilon authored here (0.70) is assessed, per the
 *   reading's own lights, against the standing arrangement — the statutes as
 *   they operate today — and not against the narrower, harm-evidenced
 *   licensure regime this reading would regard as legitimate. Sibling
 *   readings of the same kernel (public_safety_coordination,
 *   rent_seeking_suppression) are separate constraint files with their own
 *   epsilon and victim sets; they are linked through the network, not
 *   averaged into this one. KEY AGENTS (by structural relationship): -
 *   licensed_incumbent_practitioners: Primary beneficiary
 *   (organized/identity_locked) — collects the wage premium the entry gate
 *   sustains - accredited_training_programs: Secondary beneficiary
 *   (institutional/mobile) — sells the statutorily mandated preparation -
 *   professional_licensing_boards: Agenda setter (institutional/arbitrage) —
 *   defines scope, grants and revokes, enforces - state_legislatures:
 *   Co-agenda setter (institutional/arbitrage) — enacts and amends the
 *   statutes - low_income_license_aspirants: Primary target
 *   (powerless/trapped) — bears the barrier's full cost -
 *   returning_citizens_with_records: Secondary target (powerless/trapped) —
 *   barred at the final discretionary gate - license_dependent_consumers:
 *   Near-symmetric party (moderate/constrained) — buys the assurance signal,
 *   pays restricted-supply prices - out_of_state_credentialed_practitioners:
 *   Excluded peer (moderate/constrained) — equally trained, barred by
 *   non-recognition - labor_economists: Analytical observer
 *   (analytical/analytical) — measures incidence, premia, and reform outcomes
 *
 * KEY AGENTS:
 *   - licensed_incumbent_practitioners: Primary beneficiary (organized/identity_locked) — collects the wage premium the entry gate sustains
 *   - accredited_training_programs: Secondary beneficiary (institutional/mobile) — sells the statutorily mandated preparation
 *   - professional_licensing_boards: Agenda setter (institutional/arbitrage) — defines scope, grants and revokes, enforces
 *   - state_legislatures: Co-agenda setter (institutional/arbitrage) — enacts and amends the statutes
 *   - low_income_license_aspirants: Primary target (powerless/trapped) — bears the barrier's full cost
 *   - returning_citizens_with_records: Secondary target (powerless/trapped) — barred at the final discretionary gate
 *   - license_dependent_consumers: Near-symmetric party (moderate/constrained) — buys the assurance signal, pays restricted-supply prices
 *   - out_of_state_credentialed_practitioners: Excluded peer (moderate/constrained) — equally trained, barred by non-recognition
 *   - labor_economists: Analytical observer (analytical/analytical) — measures incidence, premia, and reform outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, 0.7).
domain_priors:suppression_score(licensing_statute_mandate__graduated_access_filter, 0.78).
domain_priors:theater_ratio(licensing_statute_mandate__graduated_access_filter, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, extractiveness, 0.7).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(licensing_statute_mandate__graduated_access_filter, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(licensing_statute_mandate__graduated_access_filter, snare).
narrative_ontology:human_readable(licensing_statute_mandate__graduated_access_filter, "Statutory Credential Mandates as Graduated Access Filter").
narrative_ontology:topic_domain(licensing_statute_mandate__graduated_access_filter, "labor economics/regulatory policy/public administration").

domain_priors:requires_active_enforcement(licensing_statute_mandate__graduated_access_filter).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(licensing_statute_mandate__graduated_access_filter, 'a79053f5-e714-4dea-80f4-5d8c5f87b0e6').
narrative_ontology:cs_kernel_codification('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', formalized).
narrative_ontology:cs_authority_grounding('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', extraction).
narrative_ontology:cs_interpretation_layer_present('a79053f5-e714-4dea-80f4-5d8c5f87b0e6').
narrative_ontology:cs_reading_relation('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', licensing_statute_mandate__public_safety_coordination, coexists_with).
narrative_ontology:cs_reading_relation('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', licensing_statute_mandate__rent_seeking_suppression, coexists_with).
narrative_ontology:cs_axiom('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', foundational, credential_barriers_sort_by_prior_resources).
narrative_ontology:cs_axiom_status(credential_barriers_sort_by_prior_resources, holdable).
narrative_ontology:cs_axiom_grounding('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', credential_barriers_sort_by_prior_resources, empirically_contingent).
narrative_ontology:cs_axiom('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', foundational, market_access_denial_requires_harm_evidence).
narrative_ontology:cs_axiom_status(market_access_denial_requires_harm_evidence, holdable).
narrative_ontology:cs_axiom_grounding('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', market_access_denial_requires_harm_evidence, deontological).
narrative_ontology:cs_reference_frame('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', harm_evidenced_minimal_licensure).
narrative_ontology:cs_drift_state('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', contemporary_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a79053f5-e714-4dea-80f4-5d8c5f87b0e6', '').
narrative_ontology:cs_kernel_id(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, licensed_incumbent_practitioners).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, accredited_training_programs).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, low_income_license_aspirants).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, returning_citizens_with_records).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(licensing_statute_mandate__graduated_access_filter, license_dependent_consumers).
narrative_ontology:constraint_victim(licensing_statute_mandate__graduated_access_filter, license_dependent_consumers).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, state_police_power_doctrine).
narrative_ontology:constraint_vindicates(licensing_statute_mandate__graduated_access_filter, meritocratic_gatekeeping_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold licenses that legally reserve their occupation's market. Their wages run above comparable unlicensed trades, a premium sustained because new entry must first clear training hours, examinations, and fees. Many sit on the boards that write the rules. Leaving the occupation would forfeit years of invested training and professional standing, so defending the license requirement is indistinguishable from defending their own career capital.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, licensed_incumbent_practitioners, beneficiary,
    organized, biographical, identity_locked, national).

% Sell the courses, clock-hours, and examination preparation the statutes require before anyone may sit for licensure. Enrollment is guaranteed by the mandate itself, and tuition scales with required hours. Programs advocate for scope additions that lengthen curricula; if mandates loosened, pivoting to elective or unregulated subjects remains available.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, accredited_training_programs, beneficiary,
    institutional, generational, mobile, national).

% Appointed bodies, majority sitting licensees, that define scope of practice, set passing scores, grant and revoke licenses, and pursue unlicensed practice through cease-and-desist orders and penalties. They adjust exemptions, grandfathering, and interstate compact reciprocity under political pressure while keeping the entry gate intact; their authority depends on the statutes remaining broad.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, professional_licensing_boards, agenda_setter,
    institutional, generational, arbitrage, national).

% Enact and amend the credential statutes, occasionally adding sunrise reviews or recognizing out-of-state licenses. Repeal bills face concentrated opposition from the affected occupations and diffuse, unorganized benefit from would-be entrants, so wholesale retrenchment rarely survives committee even where fiscal analyses favor it.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, state_legislatures, agenda_setter,
    institutional, biographical, arbitrage, national).

% Want to work in licensed occupations but must finance tuition, examination fees, and hundreds to thousands of unpaid supervised hours before earning anything at the licensed rate. Credit constraints and family obligations put the upfront investment out of reach for many; those who start and stall carry debt with no credential, and practicing without the license brings fines or misdemeanor charges.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, low_income_license_aspirants, payer,
    powerless, biographical, trapped, regional).

% Complete the required training, then meet character-and-fitness questions and conviction-based denial at the final gate. The investment is already spent when the discretionary bar closes; appeals are slow, and the same record that blocks licensure pushes work into the informal economy, where incomes are lower and penalties for unlicensed practice compound the original conviction.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, returning_citizens_with_records, payer,
    powerless, biographical, trapped, regional).

% Receive a common, state-backed signal that a provider met a minimum standard, plus a recourse path through the board. They pay for it in restricted supply: fewer providers, longer waits, and higher prices than in neighboring unlicensed markets, with the sharpest shortfalls in lower-income neighborhoods where provider density was thinnest to begin with.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, license_dependent_consumers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(licensing_statute_mandate__graduated_access_filter, license_dependent_consumers, payer).

% Hold licenses earned under another state's requirements and find them unrecognized or re-examined at the border. Identical training, different paperwork: until universal-recognition acts spread, moving means months without income or repeating coursework already passed.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, out_of_state_credentialed_practitioners, excluded,
    moderate, biographical, constrained, continental).

% Measure wage premia, employment and minority-employment effects, and pass-rate demographics across states and over time; document that barrier stringency tracks occupation income and prestige more closely than injury or harm rates; and evaluate reform episodes such as derecognition and universal recognition for their labor-market consequences.
narrative_ontology:constraint_stakeholder(licensing_statute_mandate__graduated_access_filter, labor_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(licensing_statute_mandate__graduated_access_filter, licensed_incumbent_practitioners).
narrative_ontology:fixing_cost_class(licensing_statute_mandate__graduated_access_filter, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives consumers a uniform, state-backed signal of minimum training in personal-service and health occupations, gives practitioners a portable (if unevenly recognized) marker of standing, and gives boards a defined scope within which to police practice.
% TRANSFER_FUNCTION: Moves income upward through restricted supply — higher prices paid by consumers and higher wages collected by license holders — and moves upfront costs (tuition, examination and license fees, unpaid supervised hours) from aspiring workers to training programs and boards; foregone earnings during qualification fall on the aspirant.
% ABSENT_VOICES: Would-be entrants priced out at the application stage never appear in board minutes or sunrise hearings; informal practitioners working below the radar have no recorded position; residents of the service deserts created by thin provider supply are unrepresented at scope hearings, which are dominated by sitting licensees.
% DISAPPEARANCE_RATIONALE: Overnight repeal would expand legal entry immediately: prices in newly opened occupations would compress toward unlicensed-market levels, voluntary certification and insurer credentialing would replace the statutory signal within a few years, boards would lose their enforcement caseload, and training programs would retool toward elective curricula — the labor market for these occupations would reorganize around private reputation and liability rules rather than statutory gates.
% FOUNDING_PROBLEM: Progressive-era cities faced recognizable quackery and fatal amateur practice in medicine, pharmacy, and a handful of trades; information asymmetry between expert provider and lay customer was severe, and states responded by codifying minimum training and examination as a condition of lawful practice.
% FOUNDING_PROBLEM_CORROBORATION: Public-health agencies and the malpractice and quality-of-care literature attest a live harm-prevention function in clinical occupations — corroboration from outside the beneficiary set. For the broadened roster of personal-service occupations, state sunset commissions, competition-authority comments, and the labor-economics literature attest that no demonstrable harm differential supports the barrier. No source outside the benefiting parties attests the founding problem live across the whole covered universe, and none attests it at all for cosmetology-tier fields.
narrative_ontology:disappearance_verdict(licensing_statute_mandate__graduated_access_filter, world_rearranges).
narrative_ontology:founding_problem_status(licensing_statute_mandate__graduated_access_filter, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(licensing_statute_mandate__graduated_access_filter, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(licensing_statute_mandate__graduated_access_filter, 'none', 1).
narrative_ontology:epsilon_provenance(licensing_statute_mandate__graduated_access_filter, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(licensing_statute_mandate__graduated_access_filter_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(licensing_statute_mandate__graduated_access_filter, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(licensing_statute_mandate__graduated_access_filter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70) because the measured transfers — wage premia to license holders, tuition to mandated training programs, and the foregone earnings of everyone the gate excludes — are large relative to any demonstrable harm reduction for most covered occupations. Suppression (0.78) is a raw structural property, unscaled by power or scope: the gate is statutory prohibition backed by fines and misdemeanor charges for unlicensed practice, and the informal-economy alternative carries its own legal exposure. Theater ratio (0.35) reflects machinery that performs diligence — continuing-education hours, renewal paperwork, public board hearings — while the binding element is simply the statute's existence; the performance share has grown as requirements lengthened. Accessibility collapse (0.55) is partial: adjacent unlicensed occupations, informal work, and interstate moves remain, but each carries a wage discount or legal risk, so alternatives degrade rather than vanish. Resistance (0.62) is real and growing: antitrust and civil-rights litigation, fair-chance campaigns, universal-recognition adoption, and sunset-review regimes. The measurement series run on one shared time grid (points 0–30 at intervals of 6) so every tracked metric is authored at every examined point; the suppression_requirement series is authored because the story specifically traces enforcement-capacity change — board staffing and penalty escalation building through the middle of the interval, then plateauing as budgets stabilized and reform pressure mounted. The extractiveness trajectory is a ratchet with late deceleration rather than a cycle: expansions accumulate, each grandfathering its sitting beneficiaries, while late-interval reform blunts but does not reverse the slope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute very differently. From the aspirant's position the arrangement is a wall: the occupation is legally closed until an unaffordable sequence is completed, and failure at any stage forfeits everything spent. From the incumbent's position the same statutes are professional obligation and quality stewardship — the license is experienced as earned standing, not as a toll others pay. Boards experience a protection mandate; legislatures experience manageable interest-group politics in which concentrated opposition reliably beats diffuse benefit. Coalition potential among the powerless victims exists — aspirants are numerous, and fair-chance and worker-center organizing have scored localized wins — but collective-action costs are high because each exclusion is individual, the payoff to organizing is deferred, and returning citizens carry compounded stigma that suppresses participation. The engine computes these per-seat divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for incumbents and training programs; victim declarations drive high directionality for aspirants and returning citizens, amplified by trapped exit — neither group can reach the licensed rate without clearing the gate, and the second group hits a discretionary bar after its investment is sunk. Incumbents' identity_locked exit matters on the subsidy side: their career capital is fused to the credential, so defense of the arrangement is indistinguishable from self-defense, which stabilizes the beneficiary coalition beyond what fee flows alone would predict. License-dependent consumers sit near symmetric through their dual beneficiary/payer declaration — they receive the assurance signal and pay the restricted-supply markup, with the sharpest shortfalls in low-income service deserts. Out-of-state credentialed practitioners are targets despite moderate power: non-recognition aims the gate at them specifically. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms place every seat correctly, and an override keyed to a shared power atom (three institutional actors with different relationships) would flatten distinctions the structural data already encodes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — quackery and fatal amateur practice in a handful of expert professions — is plausibly still live for clinical occupations and dead or never-operative for the broadened personal-service roster, hence founding_problem_status: contested. The arrangement persists and grows across both halves, which is the signature the R5 mismatch consumer watches: not the flagged dead-plus-world_rearranges zombie combination, but adjacent to it. Classification prevents mislabeling in both directions here. Reading the whole apparatus as rope accepts the safety cover story wholesale and misses the class-sorting mechanism this reading isolates; reading all of it as pure extraction with no coordination content would erase the corroborated clinical-safety core that the sibling public_safety_coordination reading legitimately registers. Keeping the epsilon referent fixed on the standing arrangement — statutes as operated — also guards against the advocacy-reading failure mode in which a critic's preferred alternative (narrow, harm-evidenced licensure) silently substitutes for the arrangement under evaluation and drives epsilon toward zero.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story is the graduated_access_filter reading of the licensing_statute_mandate kernel; would the public_safety_coordination or rent_seeking_suppression readings of the same statutes yield a different epsilon, victim set, or computed type?',
    'Author and classify the sibling stories against the same statutory referent and compare per-seat outputs; divergent computed types across readings locate the disagreement in victim-set identification and barrier-incidence attribution rather than in the statutes themselves.',
    'Adopting the safety reading would lower epsilon toward coordination-cost levels and likely compute a rope-side type; adopting the rent-seeking reading would widen the victim set to all blocked entrants and raise measured extraction further.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: one statutory kernel, three readings, three constraints.').

omega_variable(
    harm_evidence_gap,
    'Do barrier stringency and measurable consumer-harm reduction correlate across licensed occupations?',
    'Cross-state dose-response comparison of occupations licensed in some states and unlicensed in others, matched on service type and provider mix.',
    'No harm differential confirms the safety rationale as cover and stabilizes the snare computation; a strong differential in specific occupations splits the family, with those segments computing closer to a coordinated standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_evidence_gap, empirical, 'Whether the coordination cover story has empirical substance.').

omega_variable(
    incidence_attribution,
    'Is the class gradient in licensure completion caused by the credential mandate itself or by correlated background disadvantage?',
    'Natural experiments from state derecognition and universal-recognition adoptions: if demographic entry gaps close when statutes lift, the statute carries the gradient.',
    'Statute-caused gradients support the structural-exclusion reading; background-caused gradients would relocate the binding mechanism outside this constraint''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidence_attribution, empirical, 'Causal attribution of the class-sorting pattern.').

omega_variable(
    aspirant_suppression_mechanism,
    'Is aspirant non-entry driven by the structural cost of the gate or by internalized expectation that licensure is unattainable?',
    'Compare completion rates among funded applicants (employer-sponsored, scholarship) against self-financed applicants; persistence of non-application after fee waivers indicates internalized residue.',
    'An internalized component raises effective suppression above the statutory measure and persists after reform; a purely structural component predicts rapid entry response to cost relief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aspirant_suppression_mechanism, empirical, 'Structural versus internalized suppression among excluded aspirants.').

omega_variable(
    grandfathering_ratchet,
    'Does each scope expansion become irreversible because sitting practitioners are exempted while new entrants remain bound?',
    'Track repeal and rollback episodes for durability: whether delicensed occupations stay open or mandates return within a decade.',
    'A durable ratchet means reform requires prospective sunset architecture rather than one-time repeal; absence of a ratchet keeps rollback cheap and persistence contingent on ongoing defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grandfathering_ratchet, conceptual, 'Irreversibility of expansions via grandfathering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(licensing_statute_mandate__graduated_access_filter, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lice_tr_t0, licensing_statute_mandate__graduated_access_filter, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(lice_tr_t0, observed).
narrative_ontology:measurement(lice_tr_t6, licensing_statute_mandate__graduated_access_filter, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(lice_tr_t6, observed).
narrative_ontology:measurement(lice_tr_t12, licensing_statute_mandate__graduated_access_filter, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(lice_tr_t12, observed).
narrative_ontology:measurement(lice_tr_t18, licensing_statute_mandate__graduated_access_filter, theater_ratio, 18, 0.31).
narrative_ontology:measurement_basis(lice_tr_t18, observed).
narrative_ontology:measurement(lice_tr_t24, licensing_statute_mandate__graduated_access_filter, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(lice_tr_t24, observed).
narrative_ontology:measurement(lice_tr_t30, licensing_statute_mandate__graduated_access_filter, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(lice_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(lice_be_t0, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 0, 0.56).
narrative_ontology:measurement_basis(lice_be_t0, observed).
narrative_ontology:measurement(lice_be_t6, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(lice_be_t6, observed).
narrative_ontology:measurement(lice_be_t12, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(lice_be_t12, observed).
narrative_ontology:measurement(lice_be_t18, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 18, 0.68).
narrative_ontology:measurement_basis(lice_be_t18, observed).
narrative_ontology:measurement(lice_be_t24, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(lice_be_t24, observed).
narrative_ontology:measurement(lice_be_t30, licensing_statute_mandate__graduated_access_filter, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(lice_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(lice_su_t0, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(lice_su_t0, observed).
narrative_ontology:measurement(lice_su_t6, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(lice_su_t6, observed).
narrative_ontology:measurement(lice_su_t12, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 12, 0.73).
narrative_ontology:measurement_basis(lice_su_t12, observed).
narrative_ontology:measurement(lice_su_t18, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 18, 0.76).
narrative_ontology:measurement_basis(lice_su_t18, observed).
narrative_ontology:measurement(lice_su_t24, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 24, 0.77).
narrative_ontology:measurement_basis(lice_su_t24, observed).
narrative_ontology:measurement(lice_su_t30, licensing_statute_mandate__graduated_access_filter, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(lice_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(licensing_statute_mandate__graduated_access_filter, enforcement_mechanism).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__public_safety_coordination).
narrative_ontology:affects_constraint(licensing_statute_mandate__graduated_access_filter, licensing_statute_mandate__rent_seeking_suppression).

% DUAL FORMULATION NOTE:
% The colloquial label 'occupational licensing' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one statutory kernel: consumer-harm prevention (public_safety_coordination), incumbent supply restriction (rent_seeking_suppression), and class-indexed access tiering (this file). Each reading assigns its own epsilon to the same standing arrangement — the statutes as operated — because each reads the binding mechanism differently; measuring the arrangement by harm-reduction observables yields low extraction, while measuring it by barrier-incidence observables yields high extraction, which is precisely the signal that the label conflated distinct constraints. Family linkage runs through affects_constraints; the upstream safety reading is the one cited as justification by the enforcement structure, and the two critical readings draw on overlapping empirical programs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
