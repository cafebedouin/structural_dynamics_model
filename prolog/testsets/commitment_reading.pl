% ============================================================================
% CONSTRAINT STORY: commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commitment_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: commitment_reading
 *   human_readable: Irrevocable Self-Binding as Cooperation Guarantee (Commitment Reading)
 *   domain: cooperation_theory/institutional_economics/evolutionary_game_theory
 *
 * SUMMARY:
 *   This story instantiates the commitment reading of the credible-cooperator
 *   kernel: cooperation is made credible by an irreversible, self-binding act
 *   — a lease, a marriage, a vendor contract — that removes the option to
 *   defect by removing the option to exit. Legitimacy under this reading
 *   derives from the sunk, non-recoverable nature of the bond itself, not
 *   from any ongoing verification process. The defining structural feature is
 *   discontinuity: while the bond holds and interests remain aligned,
 *   extraction is low and the arrangement looks like clean coordination (a
 *   Rope). But because legitimacy is grounded in irreversibility rather than
 *   continuous checking, the moment the counterparty's interests diverge and
 *   exit is structurally unavailable to the bound party, the same mechanism
 *   becomes a trap — extraction spikes sharply rather than drifting upward
 *   gradually, which is the diagnostic difference from the audit reading's
 *   continuous-drain failure mode. This is deliberately NOT the audit reading
 *   (verification-based legitimacy), the signaling_market reading
 *   (costly-signal-based credibility), or the exit_option reading
 *   (credibility preserved through retained-but-costly exit) — those are
 *   separate constraints with separate ε values, linked here only through the
 *   shared kernel.
 *
 * KEY AGENTS:
 *   - asymmetrically_bound_party: primary target (moderate/trapped) — surrendered exit to make cooperation credible, now cannot recover it when needed
 *   - counterparty_with_symmetric_bind: primary beneficiary (moderate/constrained) — retains superior exit options, benefits from the other party's foreclosed exit
 *   - system_relying_on_credible_commitment: institutional beneficiary (institutional/analytical) — benefits from the general availability of binding instruments regardless of which party ends up trapped
 *   - contract_drafting_institutions: agenda-setter (institutional/analytical) — writes and enforces the terms that manufacture irrevocability, could write symmetric exit but often doesn't
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commitment_reading, 0.42).
domain_priors:suppression_score(commitment_reading, 0.71).
domain_priors:theater_ratio(commitment_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commitment_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commitment_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(commitment_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commitment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(commitment_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commitment_reading, tangled_rope).
narrative_ontology:human_readable(commitment_reading, "Irrevocable Self-Binding as Cooperation Guarantee (Commitment Reading)").
narrative_ontology:topic_domain(commitment_reading, "cooperation_theory/institutional_economics/evolutionary_game_theory").

domain_priors:requires_active_enforcement(commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commitment_reading, 'ff0e6cf6-6dde-4911-bb5b-8c6930331de5').
narrative_ontology:cs_kernel_codification('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', distributed).
narrative_ontology:cs_authority_grounding('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', practice).
narrative_ontology:cs_interpretation_layer_present('ff0e6cf6-6dde-4911-bb5b-8c6930331de5').
narrative_ontology:cs_reading_relation('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', credible_cooperator_kernel__audit_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', credible_cooperator_kernel__signaling_market_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', credible_cooperator_kernel__exit_option_reading, influences).
narrative_ontology:cs_axiom('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', foundational, irreversibility_constitutes_credibility).
narrative_ontology:cs_axiom_status(irreversibility_constitutes_credibility, holdable).
narrative_ontology:cs_axiom_grounding('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', irreversibility_constitutes_credibility, instrumental).
narrative_ontology:cs_axiom('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', secondary, sunk_cost_substitutes_for_verification).
narrative_ontology:cs_axiom_status(sunk_cost_substitutes_for_verification, holdable).
narrative_ontology:cs_axiom_grounding('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', sunk_cost_substitutes_for_verification, empirically_contingent).
narrative_ontology:cs_reference_frame('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', irrevocable_bond_as_credible_commitment_technology).
narrative_ontology:cs_drift_state('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', contemporary_contract_and_family_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff0e6cf6-6dde-4911-bb5b-8c6930331de5', '').
narrative_ontology:cs_kernel_id(commitment_reading, credible_cooperator_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commitment_reading, counterparty_with_symmetric_bind).
narrative_ontology:constraint_beneficiary(commitment_reading, system_relying_on_credible_commitment).
narrative_ontology:constraint_victim(commitment_reading, asymmetrically_bound_party).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entered a lease, marriage, or vendor contract whose value proposition was the irreversibility itself — the binding act removed their own option to defect, which is precisely what made their cooperation credible to the other side. When the counterparty's interests diverge (the landlord finds a better tenant, the spouse's affection cools, the platform changes terms), this party discovers that the sunk, non-recoverable nature of the bond that once generated trust now generates a one-way trap: they cannot exit at the moment exit is what they need, because exit was the thing they gave up to make the deal credible in the first place.
narrative_ontology:constraint_stakeholder(commitment_reading, asymmetrically_bound_party, payer,
    moderate, biographical, trapped, regional).

% Also bound by the same instrument in principle, but holds structural advantages — better information, alternative options developed during the relationship, or a formal escape clause the other side lacks (a break clause, an at-will termination right, a diversified supplier base). Benefits from the counterparty's foreclosed exit: it stabilizes the arrangement on terms favorable to them without requiring them to bear equivalent exposure.
narrative_ontology:constraint_stakeholder(commitment_reading, counterparty_with_symmetric_bind, beneficiary,
    moderate, biographical, constrained, regional).

% Markets, courts, and social institutions that depend on parties being able to make credible, verifiable-by-irreversibility promises — mortgage markets, marriage as a social institution, long-term vendor relationships that unlock investment neither party would make under a purely at-will arrangement. This system benefits from the existence of binding instruments in general, independent of which specific party ends up asymmetrically exposed in any one instance.
narrative_ontology:constraint_stakeholder(commitment_reading, system_relying_on_credible_commitment, beneficiary,
    institutional, generational, analytical, national).

% Legal systems, standard-form contract drafters, and enforcement bodies that write and uphold the terms making the bond irrevocable — courts that enforce lease terms, contract law that penalizes breach, cultural and religious institutions that make divorce costly. They administer the mechanism that manufactures the bond's non-recoverability and could, in principle, write in symmetric exit rights but frequently do not.
narrative_ontology:constraint_stakeholder(commitment_reading, contract_drafting_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Parties who would have preferred a verification-based, exit-preserving cooperative arrangement (continuous monitoring, reputational bonding, revocable trust) but were offered only the binding-instrument form as the available cooperative technology. They are not consulted on whether irreversibility is the right mechanism for their situation — the market or institution presents it as the only credible option.
narrative_ontology:constraint_stakeholder(commitment_reading, counterfactual_at_will_parties, excluded,
    powerless, biographical, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commitment_reading, counterparty_with_symmetric_bind).
narrative_ontology:fixing_cost_class(commitment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem in cooperation under incomplete information: absent a mechanism that removes the option to defect, rational actors cannot trust each other's promises to cooperate over time, so investment, trust, and joint value creation stall. An irreversible bond substitutes for costly ongoing verification by making defection structurally unavailable rather than merely undesirable.
% TRANSFER_FUNCTION: Moves flexibility and exit-value from the party whose exit option is extinguished to the party who retains superior exit options or informational advantage, and moves systemic trust-capacity from individual risk-bearers to the institutions (markets, marriage, long-term contracting norms) that depend on credible commitments being crediblly available at scale.
% ABSENT_VOICES: Parties who would have preferred a verification-based cooperative arrangement (continuous monitoring, staged trust-building, revocable commitment with reputational cost) are not offered that alternative as a live option — the binding-instrument form is presented as the only credible cooperative technology, foreclosing the exit_option_reading's preferred mechanism before the relationship even begins.
% DISAPPEARANCE_RATIONALE: If irrevocable self-binding disappeared as a cooperative mechanism overnight, long-term leases, marriages, and vendor contracts would need to be replaced by continuous-verification substitutes (bonds, escrow, reputation systems, staged commitments) — investments requiring multi-year horizons (mortgages, capital-intensive supply chains) would become harder to finance because the credibility that irreversibility once supplied would have to be manufactured some other, likely costlier, way.
% FOUNDING_PROBLEM: Cooperation between self-interested parties over time requires a mechanism to prevent defection when one party's interests diverge from the joint arrangement; before formal binding instruments, cooperation depended on repeated interaction, kinship, or violence to enforce promises, which excluded most economically valuable long-horizon cooperation between strangers.
% FOUNDING_PROBLEM_CORROBORATION: Institutional economists and game theorists outside the beneficiary set (e.g., analyses of hold-up problems in transaction cost economics) attest the founding problem — enabling otherwise-impossible cooperation — remains partially live for genuinely symmetric bonds, but note that in a substantial share of real instruments the binding mechanism has drifted toward asymmetric lock-in that serves the stronger party's exit-optionality rather than solving a live mutual credibility problem; family law scholars and tenant advocacy research corroborate this drift specifically in marriage and lease contexts.
narrative_ontology:disappearance_verdict(commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commitment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-07-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commitment_reading, 0.42, 'claude-sonnet-5', 'conditional_vs_unconditional_cooperation_2026_20260725_131209', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commitment_reading_tests).
:- end_tests(commitment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored low-to-moderate at the story-level scalar (0.42) because this reading's defining feature is discontinuity, not a steady extraction rate — the measurement series shows near-zero extraction (0.08-0.15) through most of the interval while the bond serves its genuine coordination function, then a sharp spike to 0.61 at t=20 when the counterparty's interests diverge and the trapped party discovers exit was never available, before settling to 0.42 as the arrangement either resolves through costly renegotiation or the bond's terms are partially unwound. Suppression is authored higher and rises more smoothly (0.55 to 0.75) because the suppressive apparatus — contract enforcement, social stigma against breaking a marriage, penalty clauses for early lease termination — is present and hardening throughout the interval even while extraction stays low; the suppression is what converts latent divergence into inescapable extraction once it occurs, and it is a raw structural property, unscaled by the actual extraction rate at any given moment. Theater ratio stays low throughout (0.1-0.2) because the coordination function this reading claims (solving the credible-commitment problem) is largely genuine when interests are aligned, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   The bound party and the institutional beneficiary seat compute this constraint very differently by structural necessity, not by choice of vantage: from the trapped party's position, the bond that once enabled a mutually valuable relationship becomes, upon divergence, an instrument with no coordination content left — pure extraction with no available exit. From the institutional seat (courts, marriage as a social form, mortgage markets), the same mechanism looks like a functioning credibility technology that occasionally produces hard cases, because the institution's welfare is aggregated across many instances, most of which never reach the divergence point. Both readings are structurally correct from their respective seats; the engine computing divergent types per seat is exactly the phenomenon this classification exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   The asymmetrically_bound_party is the structural target: their directionality is pushed toward the full-target end because they are trapped (no exit option) and because the beneficiary declaration names the counterparty as receiving the value of their foreclosed exit. The counterparty_with_symmetric_bind sits nearer the beneficiary end because their exit_options are only constrained (not trapped) — they retain some capacity to walk away or renegotiate that the bound party lacks, which is precisely the asymmetry this reading's expected structural delta calls out. The system_relying_on_credible_commitment is analytical/institutional and benefits diffusely from the mechanism's existence, independent of any single instance's outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling cooperation between parties who cannot otherwise trust each other over a long horizon) remains genuinely live for many instances of this mechanism — it is not dead, which is why the classification is tangled_rope rather than snare: there IS a real coordination function operating for the duration the bond holds. But the founding-problem interview surfaces a contested status: family law and tenant-advocacy corroboration outside the beneficiary set indicates that in a substantial share of real instances, the binding mechanism has drifted toward serving the stronger party's asymmetric exit-optionality rather than solving a live mutual problem. Tangled rope correctly captures this: genuine coordination function (required) coexisting with asymmetric extraction that requires active enforcement (contract law, social stigma, penalty clauses) to hold — exactly the hybrid this type exists to name, as opposed to snare (pure extraction, no coordination story) or rope (no victim, no enforcement requirement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_credibility_vs_engineered_lockin,
    'Is the irrevocability of any given binding instrument (this lease, this marriage, this vendor contract) genuinely necessary to solve a mutual credibility problem, or has it been engineered by the drafting party specifically to produce asymmetric exit-optionality favoring themselves?',
    'Compare instruments where both parties negotiated the binding terms from positions of comparable bargaining power against instruments imposed via standard-form contracts or social convention with no negotiation; asymmetric drafting power correlates with asymmetric exit terms would indicate engineered lock-in rather than mutual credibility solution.',
    'If genuinely mutual, the arrangement is closer to a rope in most instances with tangled_rope only in the divergence tail; if systematically engineered, the arrangement is closer to snare wearing a coordination-function cover story, and the beneficiary declaration should shift toward sole capture rather than shared coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_credibility_vs_engineered_lockin, empirical, 'Whether irrevocability solves a real mutual problem or manufactures one-sided lock-in.').

omega_variable(
    reading_boundary_ambiguity,
    'This story is authored as one reading (commitment_reading) among four siblings within the credible_cooperator_kernel (audit_reading, signaling_market_reading, exit_option_reading). Is the boundary between commitment_reading and exit_option_reading always clean in practice, or do real instruments blend retained-costly-exit with irrevocable-exit in ways that make classifying a specific instance under one reading versus the other partly a matter of interpretive choice rather than observable fact?',
    'Examine specific contract types (e.g., leases with break clauses vs. leases without) and determine whether the break-clause presence moves the instance decisively into exit_option_reading territory or merely modulates the severity of commitment_reading''s discontinuous extraction.',
    'If the boundary is genuinely fuzzy for many real instruments, the two readings function more as points on a spectrum than as discrete alternative constraints, which would argue for revisiting whether they should remain separate stories or be merged with a spectrum variable — though per the ε-invariance principle, if their ε profiles differ as described (discontinuous vs. continuous-cost-of-retained-exit) they remain structurally distinct constraints regardless of real-world blending.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether commitment_reading and exit_option_reading are cleanly separable in practice or exist on a blended spectrum.').

omega_variable(
    institutional_beneficiary_scope,
    'Does the system_relying_on_credible_commitment genuinely benefit from irreversibility specifically, or would it benefit equally from any sufficiently credible commitment technology (including the audit or signaling alternatives), making its beneficiary status here an artifact of this reading''s framing rather than a real preference for irrevocability over alternatives?',
    'Compare institutional outcomes (mortgage market depth, marriage rates and stability, long-term contract formation) across jurisdictions or eras with differing legal defaults on binding-instrument revocability, holding other credibility technologies constant.',
    'If the institution is indifferent between commitment mechanisms, its beneficiary status is weaker and more diffuse than authored, which would shift gain_flow away from any institutional capture reading and toward the counterparty_with_symmetric_bind as sole capturer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_beneficiary_scope, empirical, 'Whether the institutional beneficiary specifically needs irrevocability or merely needs some credible-commitment technology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commitment_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commitment_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comm_tr_t4, commitment_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(comm_tr_t8, commitment_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(comm_tr_t12, commitment_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(comm_tr_t16, commitment_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(comm_tr_t20, commitment_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(comm_tr_t24, commitment_reading, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commitment_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(comm_be_t4, commitment_reading, base_extractiveness, 4, 0.09).
narrative_ontology:measurement(comm_be_t8, commitment_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(comm_be_t12, commitment_reading, base_extractiveness, 12, 0.15).
narrative_ontology:measurement(comm_be_t16, commitment_reading, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(comm_be_t20, commitment_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comm_be_t24, commitment_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commitment_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t4, commitment_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(comm_su_t8, commitment_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(comm_su_t12, commitment_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(comm_su_t16, commitment_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(comm_su_t20, commitment_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(comm_su_t24, commitment_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commitment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(commitment_reading, 0.12).
narrative_ontology:affects_constraint(commitment_reading, audit_reading).
narrative_ontology:affects_constraint(commitment_reading, signaling_market_reading).
narrative_ontology:affects_constraint(commitment_reading, exit_option_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling readings of the credible_cooperator_kernel, each a separate constraint with its own ε profile: commitment_reading (this file — discontinuous ε, low-then-catastrophic), audit_reading (continuous verification-driven drain), signaling_market_reading (cost borne upfront as a costly signal, ε concentrated at formation), exit_option_reading (continuous cost of maintaining a retained-but-costly exit option rather than a discontinuous trap). All four are linked bidirectionally via affects_constraints because a real institution may drift between these mechanisms over its lifecycle (e.g., a relationship that begins under commitment_reading dynamics may be renegotiated into exit_option_reading dynamics via a divorce settlement or lease break clause added after formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
