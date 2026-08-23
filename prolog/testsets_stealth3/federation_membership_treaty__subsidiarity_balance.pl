% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Subsidiarity-Balanced Freedom of Movement Settlement
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   This story instantiates the subsidiarity_balance reading of the
 *   federation_membership_treaty kernel: the founding treaty is read as
 *   establishing a court-supervised proportionality settlement in which
 *   freedom of movement is a right and national restrictions are lawful only
 *   when they serve legitimate interests proportionately. The standing
 *   arrangement under contest — the epsilon referent — is that settlement as
 *   it actually operates: graduated conditions on residence and benefit
 *   access, case-by-case justification contests between states and movers,
 *   and an adjudicative layer that sets the balance's operating point. Per
 *   the epsilon-invariance discipline, the sibling readings
 *   (integration_primary, sovereignty_primary) are separate constraints with
 *   their own files, epsilon values, and victim sets; nothing here averages
 *   across them. The claim/metric gap is deliberate: the arrangement is
 *   CLAIMED as tangled_rope because it structurally combines a genuine
 *   coordination function (one adjudicable mobility law replacing bilateral
 *   patchworks) with asymmetric extraction (costs concentrate on the least
 *   mobile and least organized), while the metrics are authored descriptively
 *   from the arrangement's observable operation — the engine computes
 *   per-seat classifications from the structural data. KEY AGENTS (by
 *   structural relationship): - supranational_court: Agenda-setter
 *   (institutional/constrained) — administers proportionality review and sets
 *   the balance's operating point - mobile_workers: Net beneficiary with
 *   payer side (organized/mobile) — holds enforceable rights, bears graduated
 *   friction - economically_inactive_migrants: Primary target
 *   (powerless/trapped) — bears the sharpest conditions -
 *   member_state_governments: Dual-positioned payer/beneficiary
 *   (institutional/constrained) — cedes blanket control, retains justified
 *   discretion - national_welfare_bureaucracies: Beneficiary
 *   (institutional/constrained) — converts discretion into eligibility
 *   filters - cross_border_employers: Beneficiary (powerful/arbitrage) —
 *   draws on pooled continental labor - third_country_nationals: Excluded
 *   (powerless/trapped) — governed by parallel regimes outside the settlement
 *   - federalist_integration_lobby: Excluded (organized/constrained) —
 *   presses the integrationist pole with no operative seat -
 *   comparative_federalism_scholars: Analytical observer
 *   (analytical/analytical) — tracks performance across domains and decades
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.58).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.64).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Subsidiarity-Balanced Freedom of Movement Settlement").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '4e2fc709-f3dd-4cb0-9d74-5c31958b7a80').
narrative_ontology:cs_kernel_codification('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', fixed_text).
narrative_ontology:cs_authority_grounding('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', lineage).
narrative_ontology:cs_interpretation_layer_present('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80').
narrative_ontology:cs_reading_relation('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', foundational, legitimate_national_interests_constrain_mobility).
narrative_ontology:cs_axiom_status(legitimate_national_interests_constrain_mobility, holdable).
narrative_ontology:cs_axiom_grounding('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', legitimate_national_interests_constrain_mobility, instrumental).
narrative_ontology:cs_axiom('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', foundational, restriction_requires_proportional_justification).
narrative_ontology:cs_axiom_status(restriction_requires_proportional_justification, holdable).
narrative_ontology:cs_axiom_grounding('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', restriction_requires_proportional_justification, conventional).
narrative_ontology:cs_reference_frame('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', proportionality_graduated_mobility_settlement).
narrative_ontology:cs_drift_state('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', post_exit_referendum_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4e2fc709-f3dd-4cb0-9d74-5c31958b7a80', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, national_welfare_bureaucracies).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_state_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, mobile_workers).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, proportionality_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the proportionality standard: reviews member-state restrictions on movement, decides which justifications stand, and thereby sets the practical operating point between mobility and national protection. Its authority flows from the founding treaty texts and accumulated case law; it cannot relocate its function outside the treaty framework without dissolving the basis of its own jurisdiction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, supranational_court, agenda_setter,
    institutional, generational, constrained, continental).

% Hold enforceable rights to move, work, and reside across member states, backed by court-reviewed protection against arbitrary exclusion. They bear the graduated costs of exercising those rights: registration obligations, resource and insurance conditions, waiting periods for benefits, and the uncertainty of case-by-case justification contests. Organized representation gives them litigation channels most residents lack.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_workers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, mobile_workers, payer).

% Move without employment — students, retirees, caregivers, job-seekers — and meet the sharpest edge of the settlement: residence conditional on sufficient resources and sickness insurance, benefit access deferred or denied, removal procedures for those judged to become burdens. Their lack of economic footing removes the leverage that protects employed movers, and the conditions follow them wherever they go inside the federation.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, economically_inactive_migrants, payer,
    powerless, biographical, trapped, continental).

% Cede blanket control over who enters and stays, accept court review of every restrictive measure, and fund the administrative machinery of compliance. In exchange they retain a bounded power to protect public order, welfare solvency, and labor-market interests whenever they can articulate a proportional justification. Leaving the framework entirely remains formally available but has been demonstrated to carry severe economic and institutional cost.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_state_governments, beneficiary).

% Convert the retained discretion into operational eligibility filters: habitual-residence tests, resource thresholds, exportability rules for benefits. They gain a defensible administrative line between established residents and newcomers that survives court scrutiny when documented properly, and they bear the paperwork burden of defending that line case by case.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, national_welfare_bureaucracies, beneficiary,
    institutional, generational, constrained, national).

% Draw on a pooled continental labor market and locate operations where labor supply suits them. The graduated structure leaves their hiring channels open while imposing few direct conditions on them; their ability to shift investment across borders gives them leverage in disputes over restriction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, cross_border_employers, beneficiary,
    powerful, biographical, arbitrage, continental).

% Live and work inside the federation under parallel national permit regimes that the mobility settlement does not reach. They would press for inclusion in the enforceable-rights framework but hold no seat in the proportionality conversation; their exclusion defines the boundary of who the balance protects.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, third_country_nationals, excluded,
    powerless, biographical, trapped, global).

% Argue that movement is constitutive of the common market and that state discretion is the exception eating the rule. The settlement gives their position no operative home — restrictions are treated as permissible when justified — so they litigate at the margins and campaign for treaty-level change they cannot themselves enact.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federalist_integration_lobby, excluded,
    organized, biographical, constrained, continental).

% Track how the balance performs across domains and decades: which justifications succeed, where friction concentrates, how the settlement absorbs crises. They publish assessments that feed back into litigation strategy and reform proposals without holding any operative seat.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, comparative_federalism_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, court-administered standard (proportionality) under which member states may protect defined national interests while citizens retain enforceable mobility rights — replacing a patchwork of bilateral mobility agreements with a single adjudicable rule.
% TRANSFER_FUNCTION: Moves adjudicative authority over mobility restrictions from national capitals to the supranational court; moves mobility friction (residence conditions, registration, resource tests, waiting periods) onto mobile citizens, concentrated on the economically inactive; returns to states a bounded, justification-dependent discretion they would lose under full integration.
% ABSENT_VOICES: Third-country nationals resident in the federation are governed by parallel, harsher regimes and have no seat in the proportionality conversation; federalist maximalists would remove state discretion entirely; sovereignty hardliners would remove court oversight — all three stand outside the operative settlement, which is negotiated among states, the court, and organized mobile labor.
% DISAPPEARANCE_RATIONALE: If the proportionality-balanced regime vanished overnight, the federation would split toward one of the sibling settlements: either unrestricted mobility with presumptively invalid restrictions, or state-by-state consent regimes with bilateral exceptions. Labor supply chains, cross-border services, welfare portability rules, and residence patterns would all reorganize around whichever pole captured; the court's dockets and the states' immigration administrations would be rebuilt from scratch.
% FOUNDING_PROBLEM: Postwar federation-building faced a legitimacy dilemma: a single mobility area requires surrendering national control over labor markets and welfare access, yet national democracies derive legitimacy from protecting exactly those domains. The arrangement was built to let both persist — mobility as a right, national protection as a justified exception.
% FOUNDING_PROBLEM_CORROBORATION: National constitutional courts asserting counter-limit reservations against supranational mobility mandates, comparative federalism scholarship documenting the persistent legitimacy deficit, and intergovernmental conference records where governments repeatedly renegotiated safeguard clauses all attest the founding tension from outside the benefiting parties; the arrangement's own recurring crises — referendum campaigns fought openly over movement — corroborate that the problem never closed.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the settlement delivers real mobility value to employed movers and employers, but its graduated conditions convert the formal right into a contingent permission precisely for those without economic footing, and states pay a real autonomy price under court review. Suppression (0.64) reflects the standing enforcement machinery — registration systems, resource tests, removal procedures, and the court's monopoly on justification review — which actively forecloses both unrestricted-mobility claims and blanket-restriction claims; per the unscaled-suppression rule this is a raw structural property, not amplified by scope. Theater (0.36) is rising: proportionality review still filters real cases, but formulaic balancing language increasingly defers to state justifications in politically sensitive domains. Accessibility collapse (0.52): once the balance is understood, both poles collapse as options inside the framework, while exit (union withdrawal) remains formally open at demonstrated severe cost. Resistance (0.60) runs on two flanks simultaneously — integrationist litigation pressing the presumption against restrictions, sovereigntist politics pressing for consent-based control — characteristic of a contested hybrid rather than a settled coordination device. All three tracked metrics run on one shared time grid (points 0, 6, 12, 18, 24, 32; roughly settlement consolidation, pre-enlargement, eastern enlargement, austerity-era benefit contests, exit-referendum crisis peak, post-crisis partial retreat). Trajectories are monotonic-rising to t=24 with partial retreat by t=32 — not cyclical; the retreat reflects crisis-driven flexibilization, not structural reversal.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the court's seat the arrangement is a functioning adjudicative order it built and maintains — coordination all the way down. From the economically inactive migrant's seat the same structure is a lattice of conditions that makes the right conditional on resources they lack — enforced exclusion wearing the grammar of balance. From the member-state seat it is a managed loss of sovereignty that nonetheless preserves core protective capacity — a bargain half-kept. The engine derives these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (mobile_workers, cross_border_employers, national_welfare_bureaucracies) sit toward the subsidized end: the settlement lowers their costs or hands them usable discretion. Declared victims (economically_inactive_migrants, member_state_governments) sit toward the target end, with trapped exit amplifying the migrants' position and constrained-but-real exit damping the states'. member_state_governments are deliberately dual-positioned (payer primary, beneficiary secondary) because the settlement takes blanket control from them while returning justification-dependent discretion — their effective position is mixed, nearer the middle than a pure target. mobile_workers carry a payer secondary role: enforceable rights with graduated friction. The court's position derives from administration rather than collection — it sets the operating point and accumulates adjudicative authority, which is why it is authored as agenda_setter rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling a single mobility area with the democratic legitimacy of national welfare and labor protection — is still live, so no mandatrophy is declared: the arrangement continues to perform its original function, degraded at the edges. The classification guards against two symmetric mislabels. Reading migrant-side friction as pure extraction (snare) erases the genuine coordination function: one adjudicable mobility law replaced a bilateral patchwork and still solves a real collective problem. Reading the balance as pure coordination (rope) erases the asymmetric incidence: costs concentrate on the powerless and unorganized while discretion accrues to institutions. Tangled rope holds both facts. The rising theater_ratio marks the risk trajectory: if the founding problem ever closes (full political union or full disaggregation), the proportionality language would persist as ritual over a dead dispute — a piton endpoint the current drift data gestures toward but has not reached.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dominance,
    'Is the proportionality-balance reading the operative settlement of the federation membership treaty, or one of three live readings whose dominance shifts by policy domain and political period?',
    'Domain-disaggregated tracking of which default presumption governs actual cases: whether restricting states or moving citizens carry the burden of justification in welfare, labor, residence, and security domains.',
    'If sovereignty_primary becomes operative, the victim set expands to all mobile citizens and this story''s classification understates the burden; if integration_primary prevails, the victim set shifts to states and the balance reading overstates migrant-side costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Which reading of the membership treaty actually governs.').

omega_variable(
    domain_varying_incidence,
    'Does the burden of the graduated structure concentrate in identifiable policy domains (welfare access, residence security, labor market protection) such that beneficiary and bearer sets differ per domain?',
    'Case-level analysis of proportionality outcomes by domain: who loses, who wins, in which policy areas.',
    'High domain variance would justify decomposing this story into per-domain constraints with distinct epsilon values rather than one blended classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_varying_incidence, empirical, 'Whether burden incidence is domain-uniform or domain-concentrated.').

omega_variable(
    proportionality_review_functionality,
    'Is proportionality review performing real filtering of state justifications, or drifting toward ritualized deference in which formulaic balancing almost never overturns a state''s stated interest?',
    'Longitudinal outcome rates: share of state restriction justifications upheld, narrowed, or overturned by the court, controlling for case mix.',
    'Rising deference would push theater_ratio upward and signal decay toward maintenance of the balance language after the balance substance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_review_functionality, empirical, 'Functional versus theatrical proportionality adjudication.').

omega_variable(
    state_exit_option_weight,
    'How heavily should demonstrated union exit weigh as a real exit option for member-state governments — does the exit precedent make states mobile enough to damp their effective burden, or is exit so costly that states remain effectively bound?',
    'Comparative analysis of exit-cost estimates against observed state compliance behavior and bargaining outcomes.',
    'If exit is credible, the state seat sits nearer the subsidized end than victim-declaration derivation suggests; if not, states sit closer to bound targets of the court''s adjudicative authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_exit_option_weight, empirical, 'Credibility of the state exit channel.').

omega_variable(
    citizenship_boundary_contingency,
    'Is the exclusion of third-country nationals from the mobility settlement intrinsic to this reading''s structure, or a contingent feature that a broader reading of the same treaty text could dissolve?',
    'Doctrinal analysis of whether equal-treatment provisions can be extended to long-term resident third-country nationals without treaty revision.',
    'If contingent, part of the measured suppression is a boundary choice rather than a structural necessity, and the affected set could expand without changing the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(citizenship_boundary_contingency, conceptual, 'Whether the citizen/non-citizen boundary is structural or chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.14).
narrative_ontology:measurement(fede_tr_t6, federation_membership_treaty__subsidiarity_balance, theater_ratio, 6, 0.17).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__subsidiarity_balance, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fede_tr_t18, federation_membership_treaty__subsidiarity_balance, theater_ratio, 18, 0.27).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.33).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__subsidiarity_balance, theater_ratio, 32, 0.36).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t6, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(fede_be_t18, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 32, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(fede_su_t6, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(fede_su_t18, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 32, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% 'Free movement in the federation' is a colloquial label covering three structurally distinct settlements that differ in default presumption and therefore in epsilon, beneficiary sets, and victim sets. Decomposed per the epsilon-invariance principle: integration_primary (restrictions presumptively illegitimate), sovereignty_primary (movement conditional on state consent), and this subsidiarity_balance reading (proportionality-governed movement). The siblings are linked here as network neighbors; this reading structurally influences both by supplying the adjudicative middle ground they contest — court practice under the balance reading changes the legitimacy conditions under which each sibling's claims are argued.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
