% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant as Aesthetic Uniformity and Behavioral Conformity Apparatus (Behavioral-Control Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   A mature planned community operates under recorded covenants administered
 *   by an elected board and a contracted management company. Read through the
 *   behavioral-control lens, the covenant's operative work is enforcing
 *   aesthetic uniformity and behavioral conformity as a property-value
 *   strategy: architectural review reaches subjective judgments about color,
 *   landscaping, and 'harmony'; lifestyle rules cap rentals and restrict
 *   pets, vehicles, and occupancy; and expression rules bar or shrink yard
 *   signs, flags, and seasonal displays. Enforcement runs on violation
 *   letters, per-day fines, liens, and the standing possibility of
 *   foreclosure for unpaid balances. The arrangement is claimed here as a
 *   snare: the coordination story (protected values, community character) is
 *   the cover under which conformity is compelled, persistence depends on the
 *   fine-and-lien machinery, exits are contractually sealed, and the victims
 *   are nameable. KEY AGENTS (by structural relationship): - board_directors:
 *   agenda-setting seat (organized/constrained) — interprets vague standards,
 *   selects violations, sets fines; discretion spares aligned households -
 *   conformist_majority_homeowners: primary beneficiary
 *   (moderate/constrained) — enjoys uniformity assurance and hoped-for
 *   premium - board_aligned_homeowners: secondary beneficiary
 *   (moderate/constrained) — approvals resolve in their favor -
 *   nonconformist_homeowners: primary target (powerless/trapped) — fines,
 *   liens, compelled undoing of improvements -
 *   marginal_aesthetics_households: target with identity-staked expression
 *   (powerless/identity_locked) - political_speech_households: target with
 *   partial statutory shield (powerless/constrained) - prospective_renters:
 *   excluded voice (powerless/mobile) — barred by rental caps -
 *   management_company: administrative collector (moderate/arbitrage) —
 *   per-door and per-action fees - state_lawmakers_and_courts: analytical
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.46).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.65).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant as Aesthetic Uniformity and Behavioral Conformity Apparatus (Behavioral-Control Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '87f369d6-2591-4a13-9a8c-353c6150b40c').
narrative_ontology:cs_kernel_codification('87f369d6-2591-4a13-9a8c-353c6150b40c', fixed_text).
narrative_ontology:cs_authority_grounding('87f369d6-2591-4a13-9a8c-353c6150b40c', extraction).
narrative_ontology:cs_interpretation_layer_present('87f369d6-2591-4a13-9a8c-353c6150b40c').
narrative_ontology:cs_reading_relation('87f369d6-2591-4a13-9a8c-353c6150b40c', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('87f369d6-2591-4a13-9a8c-353c6150b40c', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('87f369d6-2591-4a13-9a8c-353c6150b40c', foundational, collective_aesthetic_authority_over_private_expression).
narrative_ontology:cs_axiom_status(collective_aesthetic_authority_over_private_expression, holdable).
narrative_ontology:cs_axiom_grounding('87f369d6-2591-4a13-9a8c-353c6150b40c', collective_aesthetic_authority_over_private_expression, conventional).
narrative_ontology:cs_axiom('87f369d6-2591-4a13-9a8c-353c6150b40c', foundational, uniformity_maximizes_property_values).
narrative_ontology:cs_axiom_status(uniformity_maximizes_property_values, holdable).
narrative_ontology:cs_axiom_grounding('87f369d6-2591-4a13-9a8c-353c6150b40c', uniformity_maximizes_property_values, empirically_contingent).
narrative_ontology:cs_reference_frame('87f369d6-2591-4a13-9a8c-353c6150b40c', aesthetic_conformity_charter).
narrative_ontology:cs_drift_state('87f369d6-2591-4a13-9a8c-353c6150b40c', contemporary_post_buildout_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87f369d6-2591-4a13-9a8c-353c6150b40c', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_households).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, political_speech_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, management_company).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__behavioral_control_reading, aesthetic_uniformity_value_premium).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__behavioral_control_reading, covenant_running_with_land_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected owners who interpret the recorded standards, staff architectural review, decide which violations to pursue, and set fine amounts. The standards they administer are deliberately open-textured ('harmony,' 'suitability,' 'consistency'), so interpretation is where the real decisions happen. Their own properties tend to exemplify the approved look, and discretionary enforcement means households friendly to the board rarely hear from the committee. They can sell and leave the community like any owner, but while they hold seats their homes, reputations, and social standing are tied to the rules they run.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_directors, agenda_setter,
    organized, biographical, constrained, local).

% Owners whose tastes already match the approved palette. They experience the rules as background assurance that no neighbor will paint chartreuse, park a commercial fleet at the curb, or let the lawn go to seed, and they hope the tidy streetscape supports resale prices. They pay the same dues and assessments as everyone else and almost never interact with the review process. Selling remains possible but carries transaction costs and means leaving a home they chose.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Owners whose renovation plans, vehicles, and display habits coincide with whatever the current board prefers. Their applications are approved quickly, borderline calls resolve in their favor, and complaints lodged against them stall in committee. The rules cost them little because the rules are written in their image; their main expenditure is the same dues everyone pays.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Owners whose choices fall outside the standard: an unapproved paint color, a pickup with commercial plates, a front yard converted to vegetables. They receive violation notices, accrue per-day fines, and face liens if balances grow, with foreclosure for unpaid assessments as the terminal step. Complying means paying to undo their own improvements; fighting means attorney fees against association counsel funded by everyone's dues. Selling means marketing the one visibly nonconforming house on a uniform street, and the covenant binds any buyer anyway.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, biographical, trapped, local).

% Households whose expression sits just past the line: an ornate mailbox, a rose garden replacing turf, a heritage flag flown year-round, seasonal decorations left up a week too long. Their applications are denied on aesthetic-discretion grounds and the notices recur. The garden, the collection, or the display represents years of personal investment and is how the household presents itself to the world; ending the notices by erasing it would mean dismantling something their daily life is organized around, so they keep applying, appealing, and absorbing small fines rather than walk away from it.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_households, payer,
    powerless, biographical, identity_locked, local).

% Owners who post candidate signs during election season, fly flags beyond the approved pole height and size, or stage religious displays. State statutes and the federal flag-display statute give them partial protection, and litigation periodically restores a removed sign or flag, but each episode begins with demand letters, hearing notices, and accrued fines before the legal shield arrives. They cannot opt out of the covenants while owning, though the statutory overlay gives them a lever most of their neighbors lack.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, political_speech_households, payer,
    powerless, biographical, constrained, local).

% Would-be tenants screened out by rental caps and leasing bans that a growing share of associations adopt. They are not members, attend nothing, and vote on nothing; their exclusion is decided entirely by sitting owners. Their alternative is renting somewhere else, which is precisely why the cap is cheap for owners to impose: its costs land on people who were never in the room.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, prospective_renters, excluded,
    powerless, immediate, mobile, regional).

% A contracted vendor that sends violation letters, photographs alleged infractions, schedules hearings, processes fine payments, and keeps the association's books, for a per-door management fee plus per-action charges on each enforcement event it processes. Its revenue scales with the volume of enforcement activity it administers, and its inspectors effectively set which violations enter the pipeline. It serves many associations at once and can decline renewal and take its book of business elsewhere at contract cycle's end.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, management_company, beneficiary,
    moderate, immediate, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__behavioral_control_reading, management_company, agenda_setter).

% Legislatures enacting limits on association fines, flag and sign protections, and foreclosure procedure, and courts adjudicating disputes between recorded covenants and statutes. They see patterns across hundreds of associations, move slowly, and can reshape what covenants may reach, but only statute by statute and case by case.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, state_lawmakers_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__behavioral_control_reading, management_company).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__behavioral_control_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes every lot's exterior presentation — paint, roofing, landscaping, fencing, signage, vehicles, decorations — into one continuously maintained visible standard, and channels all decisions about private-property appearance through a single review process instead of leaving them to each owner.
% TRANSFER_FUNCTION: Moves decision authority over private exteriors from individual owners to the board and its review committee; moves compliance spending (repainting, re-landscaping, removal of displays) and fine payments from nonconforming households into the association's operating fund, which pays the management company and association counsel; distributes an anticipated resale premium across all member lots.
% ABSENT_VOICES: Prospective tenants barred by rental caps hold no seat and would object to the shrinking rental supply; future buyers inherit rules adopted before their purchase and can vote only after the fact; former owners who sold rather than comply carry the counter-testimony out of the community entirely. None attends meetings: the first is structurally outside membership, the second arrives only after the rules are fixed, and the third is gone.
% DISAPPEARANCE_RATIONALE: Overnight repeal would dissolve the review committee, void the fine schedules, and open leasing; exteriors would diversify within a season as owners repaint, re-landscape, and post signs; the board would shrink to a minimal maintenance function. The resale premium attached to uniformity would face a direct test against a diversified street, and the management company's enforcement-volume revenue line would collapse to bookkeeping.
% FOUNDING_PROBLEM: During initial development and sales, a builder needed to promise early buyers that later neighbors could not degrade the streetscape the sales brochures depicted; the recorded covenant was that promise, made enforceable against every future purchaser before the neighborhood existed.
% FOUNDING_PROBLEM_CORROBORATION: Land-use histories and court opinions trace these covenants to developer sales campaigns that sold uniform character ahead of build-out; document surveys show rulebooks expanding for decades after construction ended, which no sales-phase rationale covers. Legislative findings on fine and foreclosure practices and attorneys-general reports corroborate from outside the membership. Attestation comes from land-use scholars, courts, and state legislatures — no corroboration originates within the benefiting seats alone.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.46 — moderate in aggregate because the burden concentrates on a minority of households while the majority experiences the rules as cheap assurance; the engine's per-seat computation is where the concentration shows. Suppression is 0.65 and unscaled by design: it is the raw structural fact of contract-bound ownership, escalating fines, lien and foreclosure exposure, and supermajority amendment thresholds, with statutory carve-outs for flags and signs leaving deliberate leaks. Theater_ratio is 0.26 — enforcement here is real (notices issue, fines post, houses repaint), but a growing share of activity is defensive paper: standards manuals, newsletters celebrating harmony, award programs, and documentation trails built for litigation rather than for upkeep. Accessibility_collapse is 0.58: understanding the covenant does not fully close alternatives (selling, litigating, amending, statutory preemption all exist) but each is costly enough that most households comply instead. Resistance is 0.55: flag-statute litigation, sign cases, state preemption bills, board-recall attempts, and fine-payment refusals are a real and recurring counter-pressure. The temporal series run on one shared grid (points 0, 6, 12, 18, 24, 30) with every tracked metric authored at every point; the trajectories are a ratchet, not a cycle — enforcement capacity built stepwise (photographic violation documentation, per-day fine schedules, post-recession foreclosure practice) rather than oscillating, so no intermittent-reinforcement dynamic is claimed. Claim and metrics are independent authored facts: the snare claim states the structure as this reading sees it; the metrics describe observed operation; any divergence the engine computes is the datum, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   From the conformist majority's seat the arrangement computes as light-touch mutual assurance among people who already agree; from the nonconformist and marginal-aesthetics seats the same recorded text computes as compulsion with no exit and a price on self-presentation; the board's seat sees legitimate self-government of a shared asset. Coalition potential for the powerless seats exists on paper — recall elections, class litigation, amendment campaigns — but collective-action costs (turnout apathy, fear of selective retaliation, confidential hearing outcomes, management-company control of the violation pipeline) keep the targets atomized, which is precisely what the enforcement machinery is positioned to exploit. The engine computes these divergent per-seat classifications from roles, exit options, and enforcement exposure; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (conformist majority, board-aligned owners) sit near the subsidized end of the directionality range: the rules subsidize their preferences at little personal cost. Declared victims (nonconformists, marginal-aesthetics households, political-speech households) sit near the target end, with trapped and identity-locked exit pushing the first two further toward full-target than the statutorily-shielded speech households, whose constrained-but-real legal lever moderates their position. The board holds no beneficiary or victim declaration, so its derived position falls back to its power atom's default; the directionality override moves the organized atom toward the beneficiary end (d = 0.2) because discretionary enforcement systematically spares aligned households, board members' own properties included — the derivation chain cannot see that self-exemption from beneficiary declarations alone. The management company derives a low position from its beneficiary role; its per-action fee structure, which scales revenue with enforcement volume, is why it is not placed at the extreme subsidized end despite collecting from every enforcement event.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — a builder's sales-phase promise that future neighbors could not degrade the advertised streetscape — expired at build-out, yet the arrangement persisted by re-founding itself around conformity preference and value rhetoric. Keeping this reading as its own constraint prevents the classic laundering error: the genuine maintenance-and-externality coordination that would soften classification lives in the sibling coordination_reading's constraint, not here, so this story's epsilon stays clean for the conformity apparatus alone. The dead founding-problem status paired with a world_rearranges disappearance verdict is the expected signature of a repurposed mandate and routes to the capture/zombie check rather than being smoothed away; the classification resists both directions of mislabeling — it refuses to read the surviving maintenance function as exoneration, and it refuses to deny that real coordination exists somewhere in the family.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This story instantiates the behavioral_control_reading of the hoa_covenant_scope kernel; which structural elements would change under the sibling readings?',
    'Compile and compare the sibling stories: the coordination_reading shrinks epsilon toward coordination cost and dissolves the speech-expression victim classes; the extraction_reading raises epsilon and relocates the receipt of gains toward the treasury-and-board axis. The disagreement is located in the covenant''s legitimate scope — whether subjective aesthetic judgment and expression regulation fall inside the covenant''s purpose.',
    'Classification is stable only within this reading. A different reading of the same recorded text is a different constraint with its own epsilon, victim set, and type; no averaging across readings is licensed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Kernel-membership and sibling-delta record for the behavioral-control reading of hoa_covenant_scope.').

omega_variable(
    price_premium_empirics,
    'Does enforced aesthetic uniformity actually command a measurable resale premium, or is the apparent premium an artifact of self-selection (buyers who prefer order sort into covenant communities regardless of enforcement)?',
    'Hedonic pricing studies controlling for self-selection and comparing otherwise-similar covenant and non-covenant subdivisions, plus natural experiments where enforcement lapsed or was enjoined.',
    'If no premium survives controls, the value-maximization justification collapses and the cover-story account of this reading strengthens; if a real premium exists, part of the measured burden is compensated by a genuine shared benefit and hybrid classification pressure appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_premium_empirics, empirical, 'Whether the property-value rationale is empirically real or rhetorical.').

omega_variable(
    internalized_conformity,
    'How much of nonconformist acquiescence is structural (fines, liens, foreclosure threat) versus internalized (households adopting the approved aesthetic as their own standard and self-policing)?',
    'Post-exit trajectory: households that sell and move to unregulated neighborhoods — if conformity habits, self-censorship about displays, and discomfort with visible difference persist after the enforcement mechanism is gone, the internalized share is material.',
    'An internalized component raises effective suppression above the structural measure and predicts persistence of conformity norms even where enforcement weakens or is preempted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_conformity, empirical, 'Structural versus internalized share of observed compliance.').

omega_variable(
    preference_authenticity,
    'Is the conformist majority''s preference for uniformity a genuine stable preference, or a produced one — norm cascade, fear of becoming a target, sunk compliance already spent repainting?',
    'Revealed-preference comparison: owner behavior during enforcement lapses (board turnover, moratoria, enjoined fine schedules) versus stated survey preference during normal operation.',
    'If the preference is produced, the beneficiary seats sit closer to symmetric than their declarations suggest, aggregate effective burden rises, and the majority-consent defense of the arrangement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preference_authenticity, preference, 'Authenticity of the majority''s conformity preference.').

omega_variable(
    speech_preemption_trajectory,
    'Will statutory preemption (flag acts, political-sign protections, fine caps) keep amputating the expression-enforcement limb, or will associations migrate scope into unregulated domains (rental caps, vehicle rules, short-term lease bans)?',
    'Track state legislative sessions and appellate outcomes over the coming decade; count new covenant amendments targeting domains not yet subject to preemption.',
    'Continued preemption cuts suppression and epsilon at the speech margin and pressures the computed type toward a hybrid; successful scope migration holds the profile steady while relocating the victim class from sign-flyers to landlords and vehicle owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_preemption_trajectory, empirical, 'Whether the expression-enforcement limb shrinks by statute or migrates to new domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t6, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(hoa__tr_t6, observed).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(hoa__tr_t12, observed).
narrative_ontology:measurement(hoa__tr_t18, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(hoa__tr_t18, observed).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(hoa__tr_t24, observed).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(hoa__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t6, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 6, 0.37).
narrative_ontology:measurement_basis(hoa__be_t6, observed).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement_basis(hoa__be_t12, observed).
narrative_ontology:measurement(hoa__be_t18, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(hoa__be_t18, observed).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(hoa__be_t24, observed).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(hoa__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t6, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement_basis(hoa__su_t6, observed).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(hoa__su_t12, observed).
narrative_ontology:measurement(hoa__su_t18, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(hoa__su_t18, observed).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement_basis(hoa__su_t24, observed).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(hoa__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__behavioral_control_reading, identity_coordination).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the HOA covenant' decomposes into three structurally distinct constraints sharing one recorded kernel (hoa_covenant_scope): coordination_reading (maintenance and externality resolution; low epsilon, no expression victims), behavioral_control_reading (this file; conformity enforcement with subjective-aesthetic and speech scope; moderate epsilon, minority victim class), and extraction_reading (fine proliferation and board power consolidation; high epsilon, gains captured at the treasury-board axis). The upstream coordination claim is routinely cited as legitimation for the downstream behavioral and extraction operations, so contamination propagates from the coordination story outward. Each member links the others via network.affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__behavioral_control_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
