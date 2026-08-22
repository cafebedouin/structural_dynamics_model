% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: Decree-Sufficiency Doctrine of Practice Displacement (Exogenous Override Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   A recurring instrument of state formation holds that a sovereign decree,
 *   backed by penalty, is by itself enough to displace entrenched practice:
 *   abolish the old calendar by statute and the population will run on the
 *   new one; mandate approved dress and the old garments will disappear. The
 *   historical record behind this reading is mixed in a characteristic way —
 *   administrative surfaces comply quickly (tax ledgers, court dockets,
 *   school registers run on the new calendar within months), while household
 *   and ritual practice persists for decades wherever enforcement cannot
 *   reach, and dress compliance reverts whenever inspection lapses. This file
 *   instantiates the exogenous_override_reading of the
 *   legitimacy_of_imposed_practice kernel: the claim that legal mandate
 *   suffices regardless of internalization. The epsilon referent is the
 *   standing arrangement under contest — decree-backed imposition with its
 *   enforcement machinery — assessed by this reading's own lights, which
 *   concede real adjustment costs borne without consultation while treating
 *   the mechanism itself as legitimate. Claim and metrics are authored
 *   independently: the reading is claimed as a tangled_rope (real
 *   standardization function, asymmetric unconsulted costs,
 *   enforcement-dependent persistence) while the metrics describe what the
 *   compliance record shows. KEY AGENTS (by structural relationship): -
 *   state_modernization_apparatus: agenda-setting beneficiary
 *   (institutional/arbitrage) — drafts and enforces the decrees, collects the
 *   yield - urban_administrative_elites: secondary beneficiary
 *   (powerful/mobile) — staff the new order at minimal personal cost -
 *   central_fiscal_authority: incidental beneficiary
 *   (institutional/constrained) — gains one time-grid for tax, debt, and
 *   logistics - rural_peasant_populations: primary target (powerless/trapped)
 *   — bear relearning, wardrobe, and penalty costs without consultation -
 *   rural_clergy_and_ritual_specialists: identity-bound target
 *   (moderate/identity_locked) — offices fused to the abolished calendar -
 *   traditional_garment_artisans: economic target (powerless/trapped) —
 *   livelihoods tied to now-banned goods - village_customary_councils:
 *   excluded voice (moderate/constrained) — historic negotiators of practice
 *   transition, never seated - comparative_social_historians: analytical
 *   observer (analytical/analytical) — document displacement rates and
 *   reversion across cases
 *
 * KEY AGENTS:
 *   - state_modernization_apparatus: agenda-setting beneficiary (institutional/arbitrage) — drafts and enforces the decrees, collects the administrative, fiscal, and symbolic yield, and can revise any decree at will
 *   - urban_administrative_elites: secondary beneficiary (powerful/mobile) — staff the new order; urban practice already matches the mandate, so costs stay elsewhere
 *   - central_fiscal_authority: incidental beneficiary (institutional/constrained) — gains a single time-grid for taxation, debt service, and military logistics
 *   - rural_peasant_populations: primary target (powerless/trapped) — bear relearning, wardrobe, and penalty costs with no consultation channel
 *   - rural_clergy_and_ritual_specialists: identity-bound target (moderate/identity_locked) — offices fused to the abolished calendar; compliance is ritual rupture
 *   - traditional_garment_artisans: economic target (powerless/trapped) — livelihoods tied to now-banned goods; enforcement and compliance both close the workshop
 *   - village_customary_councils: excluded voice (moderate/constrained) — historic negotiators of practice transition, never seated; their absence is the doctrine applied
 *   - comparative_social_historians: analytical observer (analytical/analytical) — document displacement rates and reversion across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.55).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.78).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "Decree-Sufficiency Doctrine of Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, 'f0082d7e-0057-40dc-84da-c99129ae67b5').
narrative_ontology:cs_kernel_codification('f0082d7e-0057-40dc-84da-c99129ae67b5', formalized).
narrative_ontology:cs_authority_grounding('f0082d7e-0057-40dc-84da-c99129ae67b5', self_enforcing).
narrative_ontology:cs_reading_relation('f0082d7e-0057-40dc-84da-c99129ae67b5', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('f0082d7e-0057-40dc-84da-c99129ae67b5', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('f0082d7e-0057-40dc-84da-c99129ae67b5', foundational, decree_sufficiency_for_displacement).
narrative_ontology:cs_axiom_status(decree_sufficiency_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('f0082d7e-0057-40dc-84da-c99129ae67b5', decree_sufficiency_for_displacement, conventional).
narrative_ontology:cs_axiom('f0082d7e-0057-40dc-84da-c99129ae67b5', secondary, customary_practice_yields_to_statute).
narrative_ontology:cs_axiom_status(customary_practice_yields_to_statute, holdable).
narrative_ontology:cs_axiom_grounding('f0082d7e-0057-40dc-84da-c99129ae67b5', customary_practice_yields_to_statute, conventional).
narrative_ontology:cs_reference_frame('f0082d7e-0057-40dc-84da-c99129ae67b5', decree_confers_displacement_authority).
narrative_ontology:cs_drift_state('f0082d7e-0057-40dc-84da-c99129ae67b5', post_compliance_record_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f0082d7e-0057-40dc-84da-c99129ae67b5', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, central_fiscal_authority).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_peasant_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_clergy_and_ritual_specialists).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_garment_artisans).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, decree_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__exogenous_override_reading, legal_positivist_compliance_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and promulgates the decrees: abolishes the customary calendar for civil use, mandates approved dress in public space, and builds the inspection and penalty machinery that backs both. Collects what the program yields — regular tax and conscription schedules, expanded bureaucratic remit, and the visible authority of having remade daily life. Can amend, suspend, or quietly drop any decree, and periodically does when enforcement costs outrun returns.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus, beneficiary).

% Staff the ministries, courts, and schools that administer the new order. Urban practice already approximates the mandated norms, so the transition costs them little personally, while the program opens careers, salaries, and status. Their position depends on the state, not on any locality that might resist, so moving between posts and programs is easy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, urban_administrative_elites, beneficiary,
    powerful, biographical, mobile, national).

% Gains a single time-grid for taxation, debt service, and military logistics across a previously heterogeneous territory. Bears a modest internal cost of converting its own ledgers. Cannot easily revert: reverting would forfeit the scheduling regularity the treasury now plans around.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, central_fiscal_authority, beneficiary,
    institutional, generational, constrained, national).

% Must relearn timekeeping for markets, taxes, and labor obligations, replace or hide customary garments, and absorb fines, confiscations, and occasional imprisonment for visible non-compliance. No consultation preceded the decrees and no channel exists to renegotiate them. Exit means flight or concealment; the old practices continue privately where inspectors cannot see.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_peasant_populations, payer,
    powerless, generational, trapped, regional).

% Officiate life-cycle and seasonal rites keyed to the abolished calendar and to garments now restricted. Complying means rupturing the ritual sequence their office exists to maintain; refusing means penalties. Their vocation and the old practice are the same thing from the inside, so abandoning the old practice is not a decision available to them in the ordinary sense.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, rural_clergy_and_ritual_specialists, payer,
    moderate, biographical, identity_locked, regional).

% Earn their living producing the robes, headwear, and textiles the decrees ban or stigmatize. Skills and stock do not convert to sanctioned goods on the reform's timescale. Enforcement visits destroy inventory; compliance destroys the trade. Either way the workshop closes.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, traditional_garment_artisans, payer,
    powerless, immediate, trapped, local).

% Elders and notables who historically managed changes in dress and timekeeping through negotiated, phased adoption — trading, marrying, and farming across practice boundaries for generations. They were never asked. Had they been seated, they would have proposed longer phase-ins, local exemptions for ritual dates, and compensation for artisans, and they carried the standing to have made those terms stick locally.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, village_customary_councils, excluded,
    moderate, generational, constrained, local).

% Compile compliance records, court dockets, and ethnographic reports across the decree episodes of several states. They measure how far practice actually moved, how fast it reverted when enforcement lapsed, and what the enforcement itself cost. They collect nothing from the arrangement and sit outside every national archive's chain of command.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__exogenous_override_reading, comparative_social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real unification problem: a territory with dozens of local calendars and dress conventions cannot run unified taxation, conscription, courts, or rail timetables; a single civil calendar and a legible public-appearance standard give administrators, merchants, and recruits one grid to coordinate on.
% TRANSFER_FUNCTION: Moves adjustment costs — relearning, wardrobe replacement, ritual disruption, fines — from the state budget onto rural households, and moves symbolic authority over time and the body from customary and religious institutions to the central state.
% ABSENT_VOICES: Village customary councils and the clergy of affected communities would have objected and proposed phased, compensated adoption; rural women, disproportionately targeted by dress enforcement in several campaigns, had no seat at all. They are absent because the doctrine's premise is that consultation is unnecessary — their exclusion is not an oversight but the theory applied.
% DISAPPEARANCE_RATIONALE: If decree authority stopped being treated as sufficient, every state pursuing cultural unification would need negotiated adoption pathways, compensation schemes, and customary institutions seated at the table; the enforcement bureaucracies would shrink, and the timetable of unification would stretch from years to generations. The whole imposition model — and the careers and budgets built on it — rearranges.
% FOUNDING_PROBLEM: Post-revolutionary and post-imperial states inherited territories whose populations ran on different calendars, dressed by different conventions, and answered to different ritual authorities — heterogeneity that obstructed taxation, conscription, administration, and the project of making one nation out of many provinces.
% FOUNDING_PROBLEM_CORROBORATION: Administrative historians outside the benefiting parties corroborate the original heterogeneity problem from fiscal and conscription archives, and the same literature attests that pure decree repeatedly failed to displace practice — rural dual-calendar use persisted for decades, and dress compliance collapsed wherever inspectors withdrew. The state apparatus attests the problem is live and decree effective; the corroborating sources support the problem's reality while disputing the doctrine's solution.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__exogenous_override_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the compliance record shows substantial unconsulted costs (fines, confiscated stock, ritual rupture, decades of dual-calendar bookkeeping) falling on seats with no voice, discounted from a critic-level estimate by this reading's own legitimizing frame, which treats the mechanism as the legitimate price of unification. Suppression is high (0.78) and unscaled by design — it is the raw structural fact that the arrangement's persistence depends on inspectors, fines, and periodic campaigns rather than participant preference; the moment enforcement withdraws, practice reverts. Theater is moderate (0.35): enforcement is mostly functional (it does move administrative surfaces), but a growing share of activity is ceremonial — staged adoptions, loyalty festivals, public burnings of banned garments — signaling compliance faster than it produces it. Accessibility collapse is 0.4: alternatives to decree-based displacement (phased adoption, local opt-outs, market diffusion) remained available and argued-for throughout, and old practices persist in parallel pockets, so understanding the arrangement does not close the option set. Resistance is 0.6: rural non-compliance, concealment, artisan flight, and occasional unrest are documented across every major episode; coalition potential among the payer seats existed but was fragmented by region, language, and the absence of any representation channel. The measurement series run on one shared grid (all three metrics at all seven points); the trajectory is the envelope of episodic enforcement campaigns — crackdown, relaxation, accumulation — averaged to the monotone ratchet the record shows, with the oscillation itself functioning as a control instrument (intermittent reinforcement) rather than noise. End-state values match the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the apparatus seat the arrangement is a coordination achievement it built and can tune — standardization it wanted, costs it priced as acceptable, enforcement it calibrates. From the peasant and artisan seats the same structure is a bill presented without consultation and collected by inspectors. The clergy seat adds an amplifier the others lack: identity lock makes compliance cost something no fine schedule measures, so its experienced burden exceeds what the penalty table implies. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the apparatus, the fiscal authority, and the urban elites — the arrangement subsidizes all three, and the elites' mobility and the apparatus's arbitrage-grade control over the decrees push them toward the beneficiary end. Victim declarations drive high directionality for peasants, clergy, and artisans; trapped exit for peasants and artisans and identity lock for the clergy push all three toward the full-target end, with the clergy seat nearest it. The excluded councils enter no derivation — authored absence is commentary-grade, not correction-grade — but their exclusion is what keeps the payer seats' exit options thin: the negotiators who might have softened the terms were never in the room.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — administrative heterogeneity — is corroborated as real, but the doctrine built on it has outlived its demonstrated efficacy: the record shows displacement stalling short of completion under maximal enforcement and reverting under relaxed enforcement, the signature of a mandate persisting past its function. Classification guards both mislabels. Calling this a rope whitewashes the unconsulted victims and the enforcement dependence; calling it a snare erases the genuine standardization gains every treasury and railway still draws on. The tangled_rope verdict holds the two facts together, and the R5 interview supplies the obsolescence signal: a contested founding problem attached to a world-rearranging arrangement is precisely the configuration in which the mandate question stays open instead of being settled by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading (exogenous_override) of the kernel legitimacy_of_imposed_practice; what would the sibling readings change structurally?',
    'Author the sibling stories (endogenous_climb, hybrid_scaffolding) and compare victim sets, epsilon, and computed types across the family.',
    'Under endogenous_climb, the same decree episodes classify with higher epsilon (non-consent becomes decisive harm) and the vindicated proposition flips toward falsified; under hybrid_scaffolding, enforcement-plus-messaging episodes split off as a separate constraint with lower suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings; disagreement located at whether internalization is causally necessary for displacement.').

omega_variable(
    enforcement_deficit_escape_route,
    'When decree regimes fail to displace practice, is the failure attributable to insufficient enforcement (premise intact) or to the premise itself (internalization necessary)?',
    'Dose-response comparison across enforcement intensities: if displacement plateaus below complete despite maximal enforcement, the enforcement-deficit explanation is exhausted.',
    'If the escape route closes, the reading''s foundational axiom loses holdable status and the vindicated proposition inverts; the arrangement''s classification shifts toward pure extraction riding a shrinking coordination claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_deficit_escape_route, empirical, 'Unfalsifiability move protecting the decree-sufficiency premise from counterexample.').

omega_variable(
    consultative_pathway_foregone,
    'Would the targeted populations have adopted the standardized practices voluntarily within a generation had consultation and phase-in been offered?',
    'Natural experiments where adoption was offered without sanction (market-driven calendar uptake, voluntary fashion diffusion) compared against coerced uptake curves.',
    'If voluntary adoption was available, the arrangement''s costs include a foregone cheaper pathway, raising the extraction assessment; if not, part of the coercive overhead was unavoidable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultative_pathway_foregone, empirical, 'Whether the coercive premium reflects necessity or the refusal to negotiate.').

omega_variable(
    case_selection_asymmetry,
    'Does the doctrine''s evidentiary base generalize — do showcase successes (rapid script and administrative-calendar adoption) and failures (the twelve-year revolutionary calendar, persistent rural dual-calendar use) sample the same underlying mechanism?',
    'Systematic coding of all decree-displacement episodes by practice type, enforcement dose, and displacement half-life.',
    'If successes and failures track practice-type differences rather than decree force, the doctrine reduces to a conditional claim and the unconditional reading loses warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(case_selection_asymmetry, conceptual, 'Whether the reading''s supporting cases and refuting cases are drawn from comparable mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 1793, 1941).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1793, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1793, 0.12).
narrative_ontology:measurement_basis(legi_tr_t1793, observed).
narrative_ontology:measurement(legi_tr_t1815, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1815, 0.16).
narrative_ontology:measurement_basis(legi_tr_t1815, observed).
narrative_ontology:measurement(legi_tr_t1848, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1848, 0.2).
narrative_ontology:measurement_basis(legi_tr_t1848, observed).
narrative_ontology:measurement(legi_tr_t1871, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1871, 0.24).
narrative_ontology:measurement_basis(legi_tr_t1871, observed).
narrative_ontology:measurement(legi_tr_t1905, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1905, 0.28).
narrative_ontology:measurement_basis(legi_tr_t1905, observed).
narrative_ontology:measurement(legi_tr_t1925, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1925, 0.32).
narrative_ontology:measurement_basis(legi_tr_t1925, observed).
narrative_ontology:measurement(legi_tr_t1941, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 1941, 0.35).
narrative_ontology:measurement_basis(legi_tr_t1941, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t1793, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1793, 0.32).
narrative_ontology:measurement_basis(legi_be_t1793, observed).
narrative_ontology:measurement(legi_be_t1815, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1815, 0.38).
narrative_ontology:measurement_basis(legi_be_t1815, observed).
narrative_ontology:measurement(legi_be_t1848, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1848, 0.42).
narrative_ontology:measurement_basis(legi_be_t1848, observed).
narrative_ontology:measurement(legi_be_t1871, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1871, 0.46).
narrative_ontology:measurement_basis(legi_be_t1871, observed).
narrative_ontology:measurement(legi_be_t1905, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1905, 0.5).
narrative_ontology:measurement_basis(legi_be_t1905, observed).
narrative_ontology:measurement(legi_be_t1925, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1925, 0.53).
narrative_ontology:measurement_basis(legi_be_t1925, observed).
narrative_ontology:measurement(legi_be_t1941, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 1941, 0.55).
narrative_ontology:measurement_basis(legi_be_t1941, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1793, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1793, 0.44).
narrative_ontology:measurement_basis(legi_su_t1793, observed).
narrative_ontology:measurement(legi_su_t1815, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1815, 0.51).
narrative_ontology:measurement_basis(legi_su_t1815, observed).
narrative_ontology:measurement(legi_su_t1848, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1848, 0.57).
narrative_ontology:measurement_basis(legi_su_t1848, observed).
narrative_ontology:measurement(legi_su_t1871, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1871, 0.62).
narrative_ontology:measurement_basis(legi_su_t1871, observed).
narrative_ontology:measurement(legi_su_t1905, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1905, 0.68).
narrative_ontology:measurement_basis(legi_su_t1905, observed).
narrative_ontology:measurement(legi_su_t1925, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1925, 0.73).
narrative_ontology:measurement_basis(legi_su_t1925, observed).
narrative_ontology:measurement(legi_su_t1941, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 1941, 0.78).
narrative_ontology:measurement_basis(legi_su_t1941, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, resource_allocation).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimacy of imposed practice' decomposes into three readings with distinct epsilon values over the same decree episodes. This file authors the exogenous_override reading (epsilon 0.55, reading-indexed: the legitimizing frame discounts the unconsulted costs its own structural delta concedes). The endogenous_climb reading shares the referent but authors higher epsilon (non-consent becomes decisive harm) and treats the vindicated proposition as falsified; the hybrid_scaffolding reading splits enforcement-plus-messaging episodes into a separate constraint with lower suppression. The endogenous reading functions as the upstream baseline (higher empirical confidence, cited against decree claims); this reading is downstream and contested, citing showcase successes as its evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
