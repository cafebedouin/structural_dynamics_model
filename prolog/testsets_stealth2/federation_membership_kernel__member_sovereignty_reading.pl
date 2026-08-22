% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member-State Sovereignty Bounds on Free Movement (Welfare-Capacity Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the member_sovereignty_reading of the
 *   federation_membership_kernel. The standing arrangement under contest —
 *   the fixed referent for every metric here — is the regime in which member
 *   states bound free movement by national welfare capacity and labor-market
 *   protection, retaining authority to exclude economically inactive Union
 *   citizens and to shield social-solidarity institutions. Epsilon is
 *   authored BY THIS READING'S OWN LIGHTS over that fixed referent (values
 *   are reading-indexed; the integration_reading would author a markedly
 *   higher epsilon over the identical arrangements, and the
 *   welfare_coordination_reading a differently structured one — those are
 *   separate files, linked via network.affects_constraints, not hedges folded
 *   into this one). The expected structural delta is realized in the
 *   declarations: constrained mobility places economically inactive migrants
 *   in the victim set, sending-state workers face restricted access under
 *   transitional controls, the activity condition selects flows toward the
 *   skilled (intensifying brain drain while trapping dependency at home), and
 *   receiving-state welfare systems gain protection at the price of reduced
 *   labor-market flexibility. KEY AGENTS (by structural relationship):
 *   member_state_governments — agenda setter (institutional/constrained),
 *   administers the exclusion instruments and captures the authority and
 *   electoral rents; receiving_state_welfare_contributors — primary
 *   beneficiary (organized/constrained); incumbent_domestic_workers —
 *   beneficiary (organized/constrained); economically_inactive_eu_migrants —
 *   primary target (powerless/constrained);
 *   sending_state_workers_under_controls — target (moderate/constrained);
 *   sending_state_governments — secondary target with partial benefit
 *   (institutional/constrained); economically_active_mobile_workers — partial
 *   beneficiary carrying status-verification friction (moderate/mobile);
 *   ecj_and_eu_institutions — analytical observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.42).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.55).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member-State Sovereignty Bounds on Free Movement (Welfare-Capacity Reading)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, 'd8b3a6d1-bc99-494f-8c70-a7f5f1df3102').
narrative_ontology:cs_kernel_codification('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', fixed_text).
narrative_ontology:cs_authority_grounding('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', lineage).
narrative_ontology:cs_interpretation_layer_present('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102').
narrative_ontology:cs_reading_relation('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', federation_membership_kernel__integration_reading, forecloses).
narrative_ontology:cs_reading_relation('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', foundational, welfare_solidarity_requires_bounded_membership).
narrative_ontology:cs_axiom_status(welfare_solidarity_requires_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', welfare_solidarity_requires_bounded_membership, empirically_contingent).
narrative_ontology:cs_axiom('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', foundational, member_states_retain_exclusion_authority).
narrative_ontology:cs_axiom_status(member_states_retain_exclusion_authority, holdable).
narrative_ontology:cs_axiom_grounding('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', member_states_retain_exclusion_authority, conventional).
narrative_ontology:cs_reference_frame('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', conferred_competences_member_state_mastery).
narrative_ontology:cs_drift_state('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', contemporary_post_brexit_enlargement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d8b3a6d1-bc99-494f-8c70-a7f5f1df3102', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_contributors).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, incumbent_domestic_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, economically_active_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_eu_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_workers_under_controls).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_active_mobile_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the residence and welfare-access rules: habitual residence tests, right-to-reside assessments, removal powers, and negotiated transitional controls on new members' workers. Justify each instrument as protecting contribution pools and service capacity. Collect the political returns: electoral credit for protection, discretion over scarce welfare slots, and retention of a prerogative no supranational body can fully audit. Leaving the arrangement would require treaty change they cannot initiate alone.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, member_state_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Pay contributions and taxes into national systems whose eligibility lines this regime draws. Gain from contributor pools kept coterminous with the national tax community and from predictable actuarial ratios. Cannot exit the national system without emigrating themselves, and bear the administrative cost of the screening apparatus as taxpayers.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_contributors, beneficiary,
    organized, biographical, constrained, national).

% Work in labor markets shielded from unrestricted inflow competition, especially in lower-wage segments and in sectors covered by transitional controls. Gain from slower labor-supply growth in their segments; pay in the form of higher prices where restricted supply tightens services they consume.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, incumbent_domestic_workers, beneficiary,
    organized, biographical, constrained, national).

% Hold Union citizenship and formal movement rights, yet find residence conditional on resources they do not have: job-seeker periods expire, comprehensive sickness cover is required, self-sufficiency thresholds bind. Face refusal of registration, lapse of lawful residence status, and return to their state of origin. Becoming economically active is the main door, and it is not theirs to open on demand.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_eu_migrants, payer,
    powerless, biographical, constrained, continental).

% During accession transition periods face labor-market access restrictions in receiving states that their own government's accession treaty accepted: work permits, quotas, sector bans. Move anyway through posted-work and informal channels, absorbing the friction; their wages and bargaining position carry the discount.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_workers_under_controls, payer,
    moderate, biographical, constrained, continental).

% Accepted transitional controls as the price of accession and litigate against their extensions. Carry the concentrated side of the bargain: economically inactive citizens who cannot emigrate remain on home welfare budgets while the economically active, disproportionately skilled, still leave. Receive remittance inflows and occasional bargaining leverage in return.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, sending_state_governments, beneficiary).

% Retain the core of their movement rights: worker status opens residence and coordinated social-security treatment. Bear status-verification friction, waiting periods for equal treatment, and the risk of reclassification to inactive during unemployment spells, which switches their access off.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_active_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__member_sovereignty_reading, economically_active_mobile_workers, payer).

% Adjudicate and monitor: the Court rules on whether national residence tests and removals respect treaty movement rights; the Commission opens infringement proceedings and audits transitional-control notifications. Neither pays into nor collects from the arrangement; both determine which national instruments survive review.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, ecj_and_eu_institutions, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, member_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains nationally bounded, contribution-financed welfare systems inside an open union: keeping the contributor pool coterminous with the national tax community stabilizes actuarial ratios, lets democracies match inflows to housing and service absorption capacity, and preserves labor-market standards against wage-arbitrage pressure.
% TRANSFER_FUNCTION: Moves welfare-access security and labor-market shelter from prospective mobile citizens (above all the economically inactive) to incumbent contributors and workers of receiving states; moves final decision authority over membership boundaries from supranational adjudication to national governments.
% ABSENT_VOICES: Economically inactive Union citizens who would reside cross-border have no seat in the Council working groups that design residence tests; sending-state publics absorb the concentrated dependency and selection costs but appear only through governments juggling the dossier against unrelated bargaining chips; future migrants are unrepresented by construction.
% DISAPPEARANCE_RATIONALE: Overnight removal of exclusion authority would force welfare-access rules to harmonize upward, collapse the residual function of internal-border machinery, reprice receiving-state contribution pools, and open relief valves for sending-region dependency — the entire federal bargain over who owns solidarity would renegotiate within years.
% FOUNDING_PROBLEM: How nationally bounded, contribution-financed welfare states — built on solidarity among stable membership communities — can survive inside a union that guarantees free movement; that is, reconciling open borders with closed solidarity pools.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: comparative welfare-state scholarship on the residence-contribution nexus documents the bounded-solidarity problem independently of any government; sending-state governments, themselves disadvantaged by parts of the arrangement, attest the cost asymmetry in Council records; Court case law repeatedly concedes the difficulty of coordinating national social-security systems. Receiving-state governments attest the problem loudest and are self-interested; the founding problem does not rest on their testimony alone.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).
:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 by this reading's lights: the reading acknowledges real costs — exclusion harms to inactive citizens, restricted access for controlled workers, concentrated dependency in sending regions — but judges them legitimate boundary-maintenance prices rather than pure extraction, hence well below what a rights-prioritizing reading would author over the same referent. Suppression is 0.55 as a RAW STRUCTURAL PROPERTY (unscaled by power or scope, per the framework rule): residence testing, removal powers, and transitional controls are real coercive machinery, though administratively framed rather than openly punitive. Theater_ratio 0.28: the welfare-protection function is substantially real, but a growing share of the arrangement's public justification — the welfare-tourism framing — outruns the fiscal evidence, which is why theater climbs through the middle of the interval before easing. Accessibility_collapse 0.45: alternatives persist (activation into work, return home, timing strategies), so understanding the rule does not close the option space the way a natural law would. Resistance 0.62: sustained opposition from expansive Court jurisprudence, Commission infringement practice, sending-state litigation, and mobile-citizen advocacy. The temporal series runs on ONE SHARED GRID (all three metrics at all eight points) and traces a wave rather than a monotonic drift: extractiveness and suppression peak around the eastern-enlargement control regimes and austerity-era right-to-reside tightening (points 10-15), then ease as the most restrictionist member exits and margin-of-exclusion jurisprudence stabilizes. Base properties are measured at interval end, on the post-peak plateau. Coalition note: the primary target class is dispersed across twenty-plus states, heterogeneous in need, and legally individuated by case-by-case residence assessment — the classic conditions under which powerless agents fail to convert numbers into coalition power, which is why their low power atom is structurally stable rather than an artifact of analysis.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is a stewardship function governments perform for their contributors; from the payer seats the same instruments operate as gated denial of rights formally held. The engine derives this divergence from the structural data; nothing in the authored claim adjudicates it. Two same-level contrasts deserve note. First, lateral differentiation among nominal equals: economically active and economically inactive Union citizens hold identical citizenship status but face opposite option spaces, because worker status — not power or wealth — is the variable that unlocks access; the constraint-specific factor separating them is administrative classification, not global standing. Second, governmental differentiation at identical institutional power: sending-state and receiving-state governments sit at the same power atom with the same constrained exit, yet experience opposite directionalities because the arrangement's costs and protections flow in opposite directions across the border. Identity-lock note: member-state governments carry an institutional identity as custodians of national solidarity; the arrangement has fused with that self-conception, so breaking the frame (for instance, a fiscal union with a common solidarity pool) would change the classification more than any marginal policy reform would. Suppression here is overwhelmingly structural — legal conditions, documentation regimes, removal powers — rather than internalized; no interpersonal-style internalization omega is warranted.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation and no overrides are needed. Member_state_governments, receiving_state_welfare_contributors, and incumbent_domestic_workers sit near the beneficiary end (low d): the arrangement subsidizes their pools, markets, and prerogatives. Economically_inactive_eu_migrants sit near the full-target end (high d): they bear the transfer and hold only constrained exits, since activation is not demandable and return is the designed fallback. Sending_state_workers_under_controls likewise sit high, with moderately better exit through posted-work channels. Sending_state_governments derive high-but-not-maximal d: the victim declaration dominates, but remittance inflows and retained bargaining leverage dampen it below the pure-target end. Economically_active_mobile_workers derive low-to-mid d: genuine beneficiaries whose secondary friction (status verification, reclassification risk) pulls them slightly toward target. Observers are directionality-neutral. On scope: the movement rights being bounded operate at continental scale while the welfare pools being protected are national, so verification of abuse claims runs across a scale mismatch — the engine's scope amplification lands hardest on the targets least able to litigate across that mismatch, which is exactly the inactive-migrant seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling open borders with closed solidarity pools — is live: aging populations, migration pressure, and welfare strain keep it operative, so mandatrophy is NOT resolved and the arrangement's mandate has not outlived its function. The classification earns its keep in both directions. Against a pure-rope mislabel: the excluded migrants and controlled workers are identifiable payers, and naming them prevents the coordination story from laundering the asymmetry. Against a pure-snare mislabel: the actuarial coordination function is genuine — contribution-financed systems demonstrably depend on bounded, stable contributor communities — so reading the whole arrangement as cover would erase the real problem it manages. The drift risk sits elsewhere: theater_ratio trends upward across the interval, meaning the rhetorical justification (welfare-tourism framing) is gradually substituting for the fiscal one; if the solidarity_boundedness omega resolves as design-contingency and portability mechanisms mature, the coordination leg could atrophy while the exclusion machinery persists on inertia and performance — the classic piton trajectory. Tracking theater and extractiveness on the shared grid is what makes that transition detectable early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the member_sovereignty_reading of the federation_membership_kernel; would the integration_reading or the welfare_coordination_reading assign a different victim set, beneficiary structure, and epsilon to the identical treaty arrangements?',
    'Generate the sibling stories over the identical referent (the standing free-movement and citizenship provisions) and compare authored epsilon, victim sets, and computed per-seat types; the divergence locates the disagreement structurally.',
    'If sibling epsilon diverges widely, classification is reading-indexed rather than topic-indexed, and cross-kernel comparisons must always pair referent-fixed, reading-varied files; treating any single reading''s verdict as the topic''s verdict would be a category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one reading of a contested kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    welfare_tourism_fiscal_incidence,
    'Does restricting economically inactive migrants'' welfare access track a real fiscal externality, or a politically useful perception of one?',
    'Longitudinal fiscal-incidence studies of intra-EU migrants (age-adjusted contributions versus consumption), plus natural experiments from transitional-control expiries (A8 in 2011, A2 in 2014) and right-to-reside tightenings.',
    'If the net fiscal effect of inactive cohorts is neutral or positive, the exclusion''s coordination justification thins and the arrangement slides toward snare-flavored extraction riding on a smaller genuine core; if genuinely negative, the coordination leg of the tangled rope strengthens and the reading''s own assessment is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_tourism_fiscal_incidence, empirical, 'Empirical basis of the welfare-burden justification for exclusion.').

omega_variable(
    solidarity_boundedness_design_contingency,
    'Is bounded membership a structural precondition of national welfare solidarity, or an artifact of residence-based entitlement design that portable or harmonized structures could dissolve?',
    'Comparative analysis of portable-entitlement mechanisms (posted-worker coordination, cross-border pension aggregation) and counterfactual modeling of residence-decoupled benefit designs.',
    'If design-contingent, the constraint is an artifact of current welfare architecture rather than a deep coordination necessity, and its mandatrophy risk rises as portability matures — the coordination function could atrophy while the exclusion machinery persists theatrically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(solidarity_boundedness_design_contingency, conceptual, 'Whether the coordination function is deep or contingent on welfare-system design choices.').

omega_variable(
    selective_mobility_brain_drain_direction,
    'Does conditioning residence on economic activity intensify selective brain drain (only the economically viable move) while trapping inactive dependents in sending regions, as the expected structural delta predicts?',
    'Pre/post comparison of the skill composition of intra-EU flows around transitional-control expiries and right-to-reside tightenings; sending-region dependency-ratio trajectories.',
    'Confirmation extends the victim set beyond excluded individuals to sending-state fiscal systems and confirms the delta''s brain-drain clause; disconfirmation would contract the victim set and weaken the sending-state harm claims that currently balance the receiving-state benefit claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_mobility_brain_drain_direction, empirical, 'Whether the regime''s selection effect matches the predicted brain-drain intensification.').

omega_variable(
    internal_border_control_normalization,
    'Are the pandemic-era and migration-crisis suspensions of internal free movement a temporary overlay, or a ratchet toward permanently hardened internal boundaries?',
    'Track reinstatement durations, legal justifications, and standing internal border-control notifications from 2020 forward; compare against pre-2020 baseline frequency.',
    'If a ratchet, the suppression series resumes climbing and the reading''s enforcement base consolidates beyond its historical envelope; if transient, suppression decays back toward administrative screening and the post-peak easing in the measurements continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_border_control_normalization, empirical, 'Trajectory of the enforcement layer after 2020 crisis suspensions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fedmemb_msr_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(fedmemb_msr_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(fedmemb_msr_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(fedmemb_msr_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.29).
narrative_ontology:measurement(fedmemb_msr_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(fedmemb_msr_tr_t25, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement(fedmemb_msr_tr_t30, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(fedmemb_msr_tr_t34, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 34, 0.28).

% Extraction over time
narrative_ontology:measurement(fedmemb_msr_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(fedmemb_msr_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(fedmemb_msr_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(fedmemb_msr_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.51).
narrative_ontology:measurement(fedmemb_msr_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(fedmemb_msr_be_t25, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(fedmemb_msr_be_t30, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(fedmemb_msr_be_t34, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 34, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fedmemb_msr_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fedmemb_msr_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(fedmemb_msr_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(fedmemb_msr_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(fedmemb_msr_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(fedmemb_msr_su_t25, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(fedmemb_msr_su_t30, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(fedmemb_msr_su_t34, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 34, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the federation_membership_kernel per the epsilon-invariance principle: the colloquial debate over 'free movement versus the welfare state' covers three structurally distinct claims, instantiated as three readings with distinct victim sets and distinct reading-indexed epsilon values over the SAME referent (the standing treaty arrangements governing intra-Union movement and welfare access). This story authors the member_sovereignty_reading: the bounded-exclusion arrangement as this reading's own lights assess it. The integration_reading authors the same referent as a rights-violating arrangement (higher epsilon, expanded victim set); the welfare_coordination_reading authors it as a coordination-design problem (different beneficiary structure entirely). Each file links the others via network.affects_constraints; cross-reading comparison must hold the referent fixed and vary only the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
