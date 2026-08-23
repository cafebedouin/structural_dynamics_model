% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Vaccine Mandate Balance — Public Health Primary Reading
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the vaccine mandate regime: the
 *   machinery by which states, employers, and schools convert a consent-based
 *   vaccination decision into a compulsory contribution to community coverage
 *   when voluntary uptake falls short and vulnerable populations face lethal
 *   exposure. This story instantiates the public_health_primary reading of
 *   the vaccine_mandate_balance kernel, which endorses that override under
 *   its trigger conditions while honestly accounting for what the enforcement
 *   machinery extracts. The structural declarations follow the reading's
 *   lights in one respect and the structural record in another, deliberately:
 *   the immunocompromised-exposed are declared the harmed party (their
 *   residual lethal exposure is the standing harm the arrangement exists to
 *   answer and does not fully answer), while the unvaccinated-coerced — who
 *   undeniably bear the penalties, exclusions, and compelled intervention —
 *   are seated as cost-bearers but withheld from the declared harmed set,
 *   because this reading subordinates their consent claim to necessity. The
 *   claim and the metrics are authored independently: claimed_type records
 *   the structural truth (a genuine free-rider coordination function fused
 *   with asymmetric, actively enforced cost-bearing), and the metrics record
 *   the arrangement's actual operation, including its high enforcement
 *   extraction and its crisis-driven oscillation between expansion and
 *   retreat. Time points run in years, 2014 through 2026.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter (institutional/constrained) — declares triggers, administers orders, receives compliance and authority
 *   - unvaccinated_consent_objectors: primary cost-bearers (organized/identity_locked) — bear penalties and exclusion; withheld from the declared harmed set under this reading's consent-subordination
 *   - immunocompromised_exposed: declared harmed party (powerless/trapped) — bear residual lethal exposure the standing arrangement does not eliminate; also receive its partial protection
 *   - healthcare_workers: secondary cost-bearers (powerless/constrained) — employment-conditioned compulsion binds the reluctant minority
 *   - general_public: diffuse beneficiary (moderate/mobile)
 *   - vaccine_manufacturers: concentrated beneficiary (institutional/arbitrage)
 *   - state_legislatures: agenda-setters with repeal power (powerful/mobile)
 *   - residents_of_mandate_ban_jurisdictions: excluded seat (powerless/trapped)
 *   - constitutional_courts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.72).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.6).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Vaccine Mandate Balance — Public Health Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '5ddd30f0-5315-488c-ae4f-2de24a69feda').
narrative_ontology:cs_kernel_codification('5ddd30f0-5315-488c-ae4f-2de24a69feda', distributed).
narrative_ontology:cs_authority_grounding('5ddd30f0-5315-488c-ae4f-2de24a69feda', distributed).
narrative_ontology:cs_reading_relation('5ddd30f0-5315-488c-ae4f-2de24a69feda', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('5ddd30f0-5315-488c-ae4f-2de24a69feda', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('5ddd30f0-5315-488c-ae4f-2de24a69feda', foundational, collective_survival_duties_override_consent_claims).
narrative_ontology:cs_axiom_status(collective_survival_duties_override_consent_claims, holdable).
narrative_ontology:cs_axiom_grounding('5ddd30f0-5315-488c-ae4f-2de24a69feda', collective_survival_duties_override_consent_claims, deontological).
narrative_ontology:cs_axiom('5ddd30f0-5315-488c-ae4f-2de24a69feda', secondary, lethal_exposure_trigger_justifies_compulsion).
narrative_ontology:cs_axiom_status(lethal_exposure_trigger_justifies_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('5ddd30f0-5315-488c-ae4f-2de24a69feda', lethal_exposure_trigger_justifies_compulsion, empirically_contingent).
narrative_ontology:cs_reference_frame('5ddd30f0-5315-488c-ae4f-2de24a69feda', collective_protection_primacy).
narrative_ontology:cs_drift_state('5ddd30f0-5315-488c-ae4f-2de24a69feda', post_covid_mandate_retreat, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5ddd30f0-5315-488c-ae4f-2de24a69feda', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, general_public).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, unvaccinated_consent_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, healthcare_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assess community coverage and outbreak risk, declare when voluntary uptake has fallen short of the protective threshold, issue vaccination orders, and run the exemption and enforcement machinery — school exclusions, employment conditions, penalty schedules. They receive compliance data, expanded emergency authority, and budget appropriations tied to immunization programs. They cannot step away when coverage drops: their statutory mission binds them to the population's protection, so exit would mean ceding the field during the next outbreak.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Authorize, limit, or prohibit vaccine mandates in statute. Several have enacted broad exemption regimes or bans on employer and school mandates; others have expanded compulsion. They can repeal or restrict the arrangement at will, but each move carries electoral feedback from both objector and vulnerable constituencies, so their exit is real but politically priced.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, state_legislatures, agenda_setter,
    powerful, biographical, mobile, national).

% Decline vaccination on medical, religious, or philosophical grounds and face the enforcement consequences: exclusion from schools and workplaces, termination, fines, and barred entry to venues. Many have organized into litigation networks and political movements. For a large share, refusal is fused with religious or political identity, so the available exits — vaccinate, claim an exemption where one exists, homeschool, or relocate — each require abandoning the conviction or paying a heavy substitute cost. They bear the arrangement's penalties directly; under the operative rule their consent claims carry no veto.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinated_consent_objectors, payer,
    organized, biographical, identity_locked, national).

% Work under employer and licensing conditions that require vaccination as a condition of employment. Most complied voluntarily long before the conditions were imposed; the conditions bind the reluctant minority, who face termination or reassignment away from patient contact. Individual exit means leaving the profession or the employer; collective challenges through unions have largely failed, and each worker bears the condition alone.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, healthcare_workers, payer,
    powerless, biographical, constrained, national).

% Cannot be fully protected by vaccination itself — their immune response is blunted — so their safety depends on the coverage of the people around them. When community coverage falls short of the protective threshold, they bear the exposure directly: elevated mortality, shielding regimes, and exclusion from ordinary life. They gain whatever protection the achieved coverage provides, and they pay in residual risk for every gap the arrangement leaves — exemptions, prohibition jurisdictions, waning coverage. Exit from their vulnerability does not exist.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, immunocompromised_exposed, beneficiary).

% Live under the achieved coverage: lower transmission, reduced outbreak risk, schools and hospitals that stay open. Their own vaccination was mostly voluntary and freely chosen, so the arrangement's marginal demand on them is small; their benefit arrives as a background condition they never had to purchase individually.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Sell every dose the arrangement compels or encourages. Coverage requirements guarantee demand and smooth procurement planning; liability regimes and government purchase contracts insulate them from much of the demand-side risk. Their gains are derivative of any vaccination policy but scale with compulsion; they take no part in administering the rules.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Live in jurisdictions that have prohibited mandates outright. They hold no seat in the arrangement's administration and no exemption process to petition; when outbreaks move through under-covered communities, they bear the exposure the operative rule was built to prevent, and relocation is the only exit most of them have.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, residents_of_mandate_ban_jurisdictions, excluded,
    powerless, biographical, trapped, regional).

% Adjudicate the boundary between police power and bodily integrity — reviewing mandate orders, exemption schemes, and legislative bans. They collect no compliance and bear no exposure; their output is doctrine that raises or lowers the enforcement machinery's clearance rate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, constitutional_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the free-rider problem in immunization coverage: protection against epidemic spread is substantially a public good, voluntary uptake undershoots the coverage threshold that shields people who cannot be protected by their own vaccination, and the mandate internalizes the externality by making contribution to coverage compulsory rather than optional.
% TRANSFER_FUNCTION: Moves vaccination compliance (and with it the small residual risk of adverse events and the autonomy cost of compelled medical intervention) from the unvaccinated minority into population coverage; moves penalties, exclusions, and termination costs onto objector households; and moves procurement revenue toward manufacturers as coverage requirements expand.
% ABSENT_VOICES: The immunocompromised in prohibition jurisdictions and under-served areas hold no seat in trigger declarations — their exposure is registered only after outbreaks. Residents of mandate-prohibiting jurisdictions are absent from the arrangement's administration entirely and would object that its protection never reaches them. Future patients during the next outbreak have no voice in the present calibration. Objector movements, by contrast, are loudly present in courts and legislatures — the absent voices on this reading are on the protection side, not the consent side.
% DISAPPEARANCE_RATIONALE: If the principle vanished overnight, mandate regimes would collapse to voluntary programs; coverage would re-equilibrate at trust-contingent levels and fall below protective thresholds in low-trust regions; exposure risk for the immunocompromised would rise sharply; school-entry and healthcare employment regimes would need re-founding from scratch. Agencies would lose their principal tool for closing coverage gaps, and objector constituencies would gain an unqualified consent right.
% FOUNDING_PROBLEM: Lethal epidemic disease with a safe, effective, injectable preventive: communities facing smallpox-scale mortality could not reach elimination through voluntary uptake alone, because protection is a public good and refusal is individually rational. The founding arrangement (the Jacobson-era compulsory vaccination statutes) made contribution to community coverage enforceable.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological surveillance and outbreak investigations — measles resurgences in under-covered communities, mortality differentials by coverage during COVID — corroborate from outside the administering agencies that under-coverage produces lethal exposure. Courts have repeatedly re-examined and re-bounded the compulsion power. Objector constituencies attest that disease risk exists but dispute its magnitude and the safety profile. The status is contested because the problem recurs while its weight and the remedy's terms remain disputed by parties outside the beneficiary set.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the arrangement's operative mechanism is compulsion: exclusion from school and employment, termination, fines, and compelled medical intervention, applied to a minority that has refused consent — the extraction is real even on this reading's own endorsement of it. Suppression (0.60) is a raw structural property, unscaled by power or scope in the engine's computation: the alternatives to compliance (exemptions where available, homeschooling, relocation) are narrowed but not closed, and the narrowing is enforced rather than emergent. Theater (0.35) reflects a protective function that is real — coverage does rise and outbreaks do recede under compulsion — mixed with ritualized components: exemption hearings that rarely grant, rules that persist on paper after enforcement decays, and emergency orders litigated into symbolic existence. Accessibility collapse (0.50) is moderate: exits exist at cost, and collapse is near-total only for the trapped seats. Resistance (0.75) is among the highest recorded for any public health measure: sustained litigation, legislative bans, exemption movements, and open noncompliance. The measurement series runs on one shared time grid and shows one full crisis cycle — standing school-entry regimes (low theater, moderate extraction), COVID-era expansion (extraction and suppression spiking together), and partial retreat with institutional residue (employment conditions and school-entry expansions that outlive the emergency). The oscillation is partly the mechanism: each cycle normalizes conditions during crisis that persist after it, a ratchet the retreat never fully reverses.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agency seat the arrangement is mission fulfillment: coverage achieved, outbreaks contained, authority exercised as designed. From the objector seat the same machinery is compulsion applied to their bodies and livelihoods without their consent — this reading's lights call that subordination rather than harm, but the seat's computed position is target-side regardless. From the immunocompromised seat the arrangement is simultaneously shield and exposure: it produces the coverage they depend on and leaves them paying for every gap in it. Same-level divergence: unvaccinated objectors (organized, identity-locked) and healthcare workers (individually powerless, constrained) face the same nominal compulsion with different exit structures — objectors can mobilize politically but cannot exit their conviction; workers cannot mobilize effectively but can, at career cost, leave. The engine computes these as different seats from the structural data; this reading's endorsement does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. Declared beneficiaries: general_public, public_health_agencies, vaccine_manufacturers — low d, damped or inverted effective extraction. Declared harmed party: immunocompromised_exposed — high d, amplified. The overrides exist because the derivation chain cannot see three things. First, the unvaccinated-coerced are deliberately withheld from the harmed declaration (this reading subordinates their consent claim), so without an override their seat would fall to a symmetric fallback; the organized-atom override (0.85) records their actual position: they bear the arrangement's penalties, exclusions, and compelled intervention in full, and their identity-locked exit places them at the trapped end of the target range. Second, the immunocompromised are dual-positioned — they bear the residual exposure (harmed declaration) and receive the partial protection — so the powerless-atom override (0.72) damps the near-maximal d that a trapped harmed declaration alone would derive. Third, the institutional seats are undifferentiated by the derivation: public_health_agencies and vaccine_manufacturers both hold the institutional atom and both sit at the beneficiary end (0.15), while state_legislatures (powerful atom, 0.35) carry electoral exposure that damps their beneficiary position. General_public needs no override: the beneficiary declaration plus mobile exit derives a low d. Suppression remains an unscaled structural property throughout; only extractiveness is scaled, by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline bites in three places. First, against the rope-mislabel: this reading's temptation is to present the mandate as pure coordination in which everyone nets out protected; the tangled_rope claim keeps the asymmetric cost-bearing on the books alongside the genuine free-rider function — the coordination is real and so is the compulsion. Second, against the snare-mislabel: the coordination function is not cover — the free-rider problem is textbook, the protection is delivered, and the harmed declaration sits on the protection side; a snare reading would have to explain why the arrangement's output is precisely the public good it names. Third, against the scaffold-mislabel: the conditional trigger resembles a sunset clause, and the omega on trigger-sunset ambiguity keeps that question open; but the historical record shows mandates persisting past their triggering conditions (school-entry requirements retained at elimination-level coverage), so the arrangement is not self-liquidating and its persistence is enforced rather than transitional. The R5 mismatch check runs clean: founding status contested with disappearance world_rearranges — the founding problem recurs with each outbreak, so no zombie flag; the residue risk is per-mandate (specific requirements outliving their triggers) rather than per-principle, and the trigger-opportunism omega is the tripwire for that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the public_health_primary reading of the vaccine_mandate_balance kernel — what would the sibling readings change structurally, and where exactly does the disagreement sit?',
    'Comparison against the sibling files: bodily_autonomy_primary moves the unvaccinated-coerced into the declared harmed set and empties the protection-side beneficiary structure; proportionality_reading makes epsilon conditional on threshold satisfaction and adds exemption-robustness to the enforcement picture.',
    'Under the bodily_autonomy reading, the declared harmed set inverts (objectors in, exposure class out) and the extraction story shifts to compulsion itself; under the proportionality reading, epsilon becomes regime-conditional rather than standing and the enforcement metrics vary with threshold audits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located in victim-set assignment and the justificatory weight of consent against lethal collective risk.').

omega_variable(
    coerced_cost_bearer_status,
    'This reading refuses harmed status to the unvaccinated-coerced on the ground that consent is subordinated to necessity — does that refusal eliminate their cost-bearing or only relabel it?',
    'Normative analysis plus comparative classification: run the same structural data under a consent-primary frame and compare which seats compute as bearing the arrangement''s costs.',
    'If cost-bearing stands as harm, this reading''s beneficiary and harmed structure converges toward the bodily_autonomy sibling''s, and the endorsement rests on a contested moral subordination rather than a structural absence of harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coerced_cost_bearer_status, conceptual, 'Whether consent-subordination removes the coerced from the harmed set or renames their position.').

omega_variable(
    trigger_condition_sunset_ambiguity,
    'Does the conditional trigger — compulsion justified only while voluntary compliance fails and exposure is lethal — function as a sunset clause, or is it honored in the breach as mandates persist after their triggering conditions lapse?',
    'Cross-jurisdiction comparison of mandate persistence against trigger lapse: school-entry requirements retained for diseases at elimination-level coverage, employment conditions outliving emergency declarations.',
    'If the trigger functions as a sunset, the arrangement is transitional-support-like and should decay with each achieved threshold; if honored in the breach, persistence-by-inertia is confirmed and residue accumulates across crisis cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_condition_sunset_ambiguity, conceptual, 'Whether the conditional justification operates as a self-limiting device or as cover for standing coercion.').

omega_variable(
    trigger_declaration_opportunism,
    'Are trigger declarations calibrated to genuine coverage-and-lethality data, or timed opportunistically to institutional or political benefit?',
    'Audit of trigger declarations against contemporaneous surveillance thresholds and independent epidemiological assessment.',
    'Systematic opportunism would thin the coordination story — the free-rider problem would remain real but declared triggers would stop tracking it, pushing the arrangement toward extraction with coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_declaration_opportunism, empirical, 'Whether trigger declarations track the stated epidemiological conditions.').

omega_variable(
    residual_exposure_attribution,
    'How much of the immunocompromised-exposed''s residual risk is attributable to the standing arrangement''s gaps (exemptions, prohibition jurisdictions, waning coverage) versus intrinsic limits of vaccination itself?',
    'Attributable-risk comparison across high-coverage, exemption-heavy, and prohibition jurisdictions, controlling for pathogen circulation.',
    'If most residual risk tracks the arrangement''s gaps, the declared harmed position is the arrangement''s doing and the asymmetry is structural; if it is intrinsic, the harmed declaration measures background mortality and the arrangement''s asymmetry rests on the coerced seats alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_exposure_attribution, empirical, 'Attribution of the protected class''s residual risk between arrangement gaps and vaccine limits.').

omega_variable(
    identity_lock_vs_stable_conviction,
    'Is the objectors'' identity-locked exit genuine identity fusion, or stable conviction that would persist even if every mandate were repealed?',
    'Post-repeal uptake trajectories in prohibition jurisdictions and survey panel data on refusal stability across policy changes.',
    'If fusion is genuine, their target position is durable regardless of enforcement intensity; if conviction is stable but policy-contingent, their exit options widen when enforcement relaxes and the effective extraction on their seat falls with the enforcement cycle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_stable_conviction, empirical, 'Whether refusal is identity-fused or policy-contingent conviction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t2, vaccine_mandate_balance__public_health_primary, theater_ratio, 2, 0.16).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__public_health_primary, theater_ratio, 4, 0.18).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_balance__public_health_primary, theater_ratio, 6, 0.3).
narrative_ontology:measurement(vacc_tr_t7, vaccine_mandate_balance__public_health_primary, theater_ratio, 7, 0.42).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__public_health_primary, theater_ratio, 8, 0.45).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_balance__public_health_primary, theater_ratio, 10, 0.4).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(vacc_be_t2, vaccine_mandate_balance__public_health_primary, base_extractiveness, 2, 0.51).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__public_health_primary, base_extractiveness, 4, 0.53).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_balance__public_health_primary, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(vacc_be_t7, vaccine_mandate_balance__public_health_primary, base_extractiveness, 7, 0.8).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__public_health_primary, base_extractiveness, 8, 0.78).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_balance__public_health_primary, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vacc_su_t2, vaccine_mandate_balance__public_health_primary, suppression_requirement, 2, 0.51).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__public_health_primary, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_balance__public_health_primary, suppression_requirement, 6, 0.74).
narrative_ontology:measurement(vacc_su_t7, vaccine_mandate_balance__public_health_primary, suppression_requirement, 7, 0.85).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__public_health_primary, suppression_requirement, 8, 0.8).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_balance__public_health_primary, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the vaccine mandate debate' covers three structurally distinct constraints that share one kernel (vaccine_mandate_balance): this public_health_primary reading (compulsion justified by lethal collective risk; epsilon high and standing, referent = the mandate regime itself), bodily_autonomy_primary (compulsion impermissible; the coerced as the harmed party), and proportionality_reading (compulsion conditional on thresholds; epsilon regime-conditional). Each file carries its own epsilon, harmed set, and classification; this file links its siblings per the family rule. The upstream/downstream structure runs through empirical findings: transmission and safety data feed both the proportionality thresholds and this reading's trigger conditions, which is why this reading influences (but does not foreclose) the proportionality sibling while foreclosing the consent-inviolable sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, powerless, 0.72).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, organized, 0.85).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, institutional, 0.15).
constraint_indexing:directionality_override(vaccine_mandate_balance__public_health_primary, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
