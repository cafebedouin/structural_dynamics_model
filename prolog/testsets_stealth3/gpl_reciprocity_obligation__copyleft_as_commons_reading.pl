% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Mandatory Reciprocity as Commons-Preservation Mechanism
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   A reciprocal software license obliges anyone who distributes a derivative
 *   of pooled commons code to release their modifications under the same
 *   terms. This story instantiates the commons reading of that obligation:
 *   the license is an institutional technology whose function is preventing
 *   enclosure of a shared resource, its principal beneficiary is the commons
 *   as a maintained institution, and its principal bearers of cost are firms
 *   seeking to exit with proprietary derivatives. The epsilon referent
 *   throughout is the standing reciprocity arrangement as it operates — the
 *   enforced obligation on integrators — assessed by this reading's own
 *   lights; it is not the permissive or proprietary arrangements this reading
 *   declines to endorse. KEY AGENTS (by structural relationship): -
 *   the_free_software_commons: Primary beneficiary (organized/trapped) —
 *   collects returned improvements, cannot exit its own constitutive rule. -
 *   proprietary_exit_maximizer_firms: Primary target (powerful/constrained) —
 *   bears the forfeited proprietary option. - contributing_developers:
 *   Dual-positioned (moderate/identity_locked) — net beneficiaries who pay in
 *   surrendered proprietary rights. - fsf_license_stewards and
 *   software_freedom_conservancy_enforcers: Agenda-setters
 *   (institutional/arbitrage, organized/constrained) — author the terms and
 *   enforce compliance respectively, with sharply different exit profiles. -
 *   dual_license_commercial_vendors: Arbitraging beneficiary
 *   (powerful/arbitrage) — complies formally while selling relief from the
 *   terms. - proprietary_stack_vendors: Excluded party (institutional/mobile)
 *   — shapes the environment without holding a seat.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.52).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Mandatory Reciprocity as Commons-Preservation Mechanism").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9').
narrative_ontology:cs_kernel_codification('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', fixed_text).
narrative_ontology:cs_authority_grounding('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', lineage).
narrative_ontology:cs_interpretation_layer_present('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9').
narrative_ontology:cs_reading_relation('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', foundational, commons_survival_requires_mandatory_reciprocity).
narrative_ontology:cs_axiom_status(commons_survival_requires_mandatory_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', commons_survival_requires_mandatory_reciprocity, empirically_contingent).
narrative_ontology:cs_axiom('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', secondary, individual_exit_rights_yield_to_commons_continuity).
narrative_ontology:cs_axiom_status(individual_exit_rights_yield_to_commons_continuity, holdable).
narrative_ontology:cs_axiom_grounding('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', individual_exit_rights_yield_to_commons_continuity, deontological).
narrative_ontology:cs_reference_frame('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', reciprocal_commons_pooling).
narrative_ontology:cs_drift_state('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', contemporary_server_side_ai_ingestion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('38de4ab3-2a94-4a08-88f9-0d1bb64e9cf9', '2026-07-01T00:00:00Z').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_free_software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, contributing_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_exit_maximizer_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_license_commercial_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, contributing_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_license_commercial_vendors).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds copyright on the license text itself, publishes successive versions and the authoritative interpretation FAQ, defines what counts as compliant distribution, and campaigns for adoption across projects. Bears the reputational and coordination cost of defending contested interpretations but can amend the terms going forward, so adverse interpretive outcomes never trap them in someone else's rules.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, fsf_license_stewards, agenda_setter,
    institutional, generational, arbitrage, global).

% Runs compliance programs for member projects: negotiates settlements with violators, documents infringements, and occasionally litigates under copyright law. Funded by donations, so enforcement capacity is finite and selectively deployed; aggressive action against prominent firms carries fundraising and public-relations risk, which narrows their practical room to maneuver.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, software_freedom_conservancy_enforcers, agenda_setter,
    organized, biographical, constrained, national).

% The pooled corpus of reciprocally licensed source code together with the contributor community that maintains it. Grows only through returned improvements; its integrity depends on the reciprocity rule continuing to bind integrators. It has no center that could relocate or reconstitute itself elsewhere if the rule failed — its continued existence as a commons is constituted by the rule it benefits from.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_free_software_commons, beneficiary,
    organized, civilizational, trapped, global).

% Write and contribute code under reciprocal terms. They receive guaranteed access to everyone else's improvements and a durable shared asset; they pay by surrendering proprietary rights in their own contributions and by accepting that downstream obligations may constrain products they later ship. Leaving the ecosystem would cost them professional standing and a community identity substantially built around commitments to shared code.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, contributing_developers, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, contributing_developers, payer).

% Firms whose preferred business model integrates commons code into closed products. Each integration forces a choice: publish derivative sources on the same terms, engineer around the component, procure a substitute, or ship in violation and risk infringement action. Avoiding GPL code entirely is possible but carries re-engineering cost, dependency-chain friction, and lost access to mature components; many such firms respond by maintaining strict GPL-procurement bans instead.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_exit_maximizer_firms, payer,
    powerful, biographical, constrained, global).

% Publish a community edition of their product under the reciprocal license while selling proprietary licenses for the same codebase. They harvest external contributor labor into the free edition and monetize the desire of customers to escape its terms, complying formally with the very license whose constraints they sell relief from.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_license_commercial_vendors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_commons_reading, dual_license_commercial_vendors, payer).

% Firms running fully proprietary stacks that never incorporate reciprocal code. They hold no seat in license governance, yet their procurement policies, standards-body lobbying, and promotion of permissive alternatives shape the environment the reciprocity rule operates in. They would argue the rule functions as a market barrier dressed as ethics, and they encounter it only as a procurement obstacle.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_stack_vendors, excluded,
    institutional, biographical, mobile, global).

% Study license regimes as institutional technology, tracing enclosure and reciprocity dynamics across permissive and reciprocal ecosystems, publishing analyses that both defenders and critics of the arrangement cite in support of opposed conclusions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, ip_policy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the assurance problem of a shared code pool: absent a binding reciprocity rule, any integrator can appropriate improvements and close them, and rational contributors anticipating defection under-invest in the pool. Making unilateral appropriation a copyright violation converts mutual contribution into the stable strategy.
% TRANSFER_FUNCTION: Moves derivative-work improvements and redistribution rights from firms that build on the commons back into the commons; equivalently, it transfers the opportunity cost of proprietary closure from the pool onto integrators, and transfers legal risk from contributors (whose code retains guaranteed free status) to violators.
% ABSENT_VOICES: Proprietary-stack vendors and corporate buyers of closed derivatives are outside license governance entirely; end users receiving reciprocal-derived code embedded in violating shipped firmware are owed sources they never obtain and have no seat anywhere. They surface only as litigation counterparties, procurement memos, and complaint threads.
% DISAPPEARANCE_RATIONALE: If the reciprocity rule vanished overnight, the pool's assurance mechanism would fail: improvements would become immediately appropriable, integration-seeking firms would shift toward take-and-close behavior, contribution incentives would degrade, and the specific commons institution would dissolve into permissive pools sustained by different mechanisms (employer subsidy, foundation patronage) or into captured proprietary codebases. Arrangements demonstrably depend on the rule continuing to bind.
% FOUNDING_PROBLEM: The 1980s privatization wave: printer firmware shipped without source, proprietary Unix fragmentation, and a legal default in which code shared in cooperation was routinely appropriated and closed. The arrangement was built to answer one question: how can software remain shareable under a copyright regime tilted toward private appropriation?
% FOUNDING_PROBLEM_CORROBORATION: Enclosure pressure is attested from outside the benefiting parties: legal-academic literature documenting appropriation dynamics in shared-code regimes, the historical record of pre-reciprocity capture (proprietary Unix fragmentation, the AT&T-driven disruption of the Berkeley CSRG), and — decisively — the compliance expenditures of the firms the rule binds, whose own procurement policies and settlement payments attest that the enclosure barrier is real enough to spend money avoiding.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored medium (0.55 at interval end) because the reciprocity burden is real and concentrated — integrators genuinely lose the proprietary-closure option — but bounded: entrants retain full use, modification, and redistribution rights under identical terms, and entry remains voluntary, so the burden is a price of admission rather than confiscation. Suppression (0.52) reflects legal coercion that is substantial where triggered (violation is copyright infringement) but scoped to those who chose to incorporate the code; permissive alternatives persist robustly, so alternatives are narrowed, not collapsed (accessibility_collapse 0.45). Resistance (0.55) is real and visible: procurement bans on reciprocal dependencies, clean-room reengineering, dual-license negotiation leverage, and contested enforcement actions. Theater is low (0.20): the enforcement and stewardship activity is substantively functional, with only a modest campaign-and-symbolism component. The measurement series traces enforcement-capacity maturation (suppression_requirement rising from 0.25 to 0.65 by the enforcement-professionalization peak, then easing as capacity strained and server-side use began escaping the distribution trigger), a corresponding extractiveness arc peaking mid-interval before leakage and substitution pulled it down slightly, and slowly accumulating theater as stewardship activity became partly self-referential. The trajectory is non-monotonic but not oscillatory: no intermittent-reinforcement cycle is claimed, and all three metric series share one time grid. Suppression here is predominantly structural (legal liability, dependency lock-in) with a minor internalized component (community norms that stigmatize closure attempts); the scalar does not distinguish them, which the omegas partially cover.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute materially differently. From the agenda-setter seats the arrangement is a functioning protection regime they administer: low personal extraction, genuine coordination achievement. From the payer seat the identical structure operates as a compulsory levy on business-model freedom backed by copyright enforcement. Contributing developers experience both faces at once — protected contributor and conscripted non-proprietor — and their computed position should sit between the commons and the integrator poles. The engine derives this divergence from the authored directionalities and exits; nothing in the claimed_type adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The commons is the structural beneficiary: it collects every returned improvement and bears no comparable cost, placing it at the beneficiary end of d. Proprietary exit-maximizer firms are the targets: they pay the transfer in forfeited closure, and their constrained (not trapped, not mobile) exit keeps their effective extraction high but short of the full-target pole — they can leave, expensively. Contributing developers net out as beneficiaries (guaranteed access to others' work exceeds their surrendered rights for most), but their identity_locked exit raises their effective exposure above what a mobile beneficiary would show, since they cannot credibly threaten departure. Dual-license vendors derive toward the beneficiary/subsidy end through their arbitrage-grade exit: they can monetize either side of the terms at will. Deliberately, no directionality_overrides are authored: overrides key on power atoms, and this story's two powerful seats (exit-maximizer firms and dual-license vendors) require corrections in opposite directions, so a power-atom override would corrupt one while fixing the other; the needed differentiation is carried by exit_options (constrained versus arbitrage) instead, which the derivation chain already reads.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — enclosure of cooperatively produced code under appropriation-defaulting copyright — is still live (server-side use, locked-down shipping, and machine-learning ingestion are current enclosure vectors), so the mandate has not outlived its function and no mandatrophy resolution is declared. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure rope erases the real asymmetric burden borne by integration-seeking firms; reading it as pure snare erases the genuine assurance function that keeps the pool stocked at all. The R5 mismatch check comes up clean: founding_problem_status is live and disappearance_verdict is world_rearranges, so no dead-mandate-plus-rearranging-world flag fires, and the absence of any seat capturing gains diffusely enough to constitute capture is reflected by gain_flow pointing at the commons seat rather than at any administrator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of kernel gpl_reciprocity_obligation (copyleft_as_commons_reading). What structurally changes under the sibling readings — copyleft_as_freedom_reading and copyleft_as_restriction_reading — and where exactly is the disagreement located?',
    'Cross-file comparison of the sibling stories'' beneficiary/victim declarations, epsilon referents, and computed per-seat classifications; the disagreement is located in which seat counts as the beneficiary (commons institution versus end users versus unrestricted integrators) and therefore in what the reciprocity burden is extracted FOR.',
    'Under the freedom reading the beneficiary set shifts to end users and epsilon is authored near zero from that seat (the burden is the point, not a cost); under the restriction reading the victim set widens to commercial integrators generally and epsilon rises. Classification of the same license text could range from rope-flavored to snare-flavored depending on the instantiated reading; only the sibling files carry those verdicts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one kernel, three readings, three distinct constraints; this story''s metrics are valid only within the commons reading.').

omega_variable(
    reciprocity_necessity_vs_preference,
    'Is mandatory reciprocity a genuine structural necessity for sustaining the code commons, or an institutional preference that rival coordination technologies (permissive licensing plus employer subsidy, foundation patronage, consortium funding) could satisfy equally?',
    'Comparative longitudinal analysis of permissive versus reciprocal ecosystems: contribution rates, corporate appropriation events, and pool durability matched on project scale and age, treating license-choice shocks as natural experiments.',
    'If rival mechanisms sustain equivalent pooling, the coordination half of the tangled-rope structure weakens and the measured extraction reads increasingly as rent-setting by stewards rather than the price of the commons; if reciprocal terms uniquely resist appropriation, the coordination claim strengthens and the extraction is confirmed as functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_necessity_vs_preference, empirical, 'Whether the commons genuinely depends on the reciprocity obligation or merely prefers it.').

omega_variable(
    saas_leakage_effectiveness_gap,
    'Does the observed compliance record reflect effective enclosure prevention, or is an increasing share of commons-derived value flowing out through server-side deployment that never triggers the distribution-based reciprocity condition?',
    'Audit-scale estimation of production deployments running modified commons code as unredistributed network services, contrasted with distributed-binary compliance volume, across the interval''s latter third.',
    'Substantial untriggered leakage would accelerate the practice-drift assessment, depress the effective protection the commons receives, push theater_ratio upward as formal compliance becomes a shrinking fraction of actual use, and date a transition toward inertial maintenance earlier than the scalar metrics alone indicate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_leakage_effectiveness_gap, empirical, 'Whether the reciprocity rule still binds the dominant deployment mode, or protects a shrinking slice of usage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_commons_reading_tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t0, observed).
narrative_ontology:measurement(gpl_commons_reading_tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t5, observed).
narrative_ontology:measurement(gpl_commons_reading_tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t10, observed).
narrative_ontology:measurement(gpl_commons_reading_tr_t16, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t16, observed).
narrative_ontology:measurement(gpl_commons_reading_tr_t22, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 22, 0.15).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t22, observed).
narrative_ontology:measurement(gpl_commons_reading_tr_t27, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 27, 0.18).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t27, observed).
narrative_ontology:measurement(gpl_commons_reading_tr_t32, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement_basis(gpl_commons_reading_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(gpl_commons_reading_be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t0, observed).
narrative_ontology:measurement(gpl_commons_reading_be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t5, observed).
narrative_ontology:measurement(gpl_commons_reading_be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t10, observed).
narrative_ontology:measurement(gpl_commons_reading_be_t16, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t16, observed).
narrative_ontology:measurement(gpl_commons_reading_be_t22, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 22, 0.62).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t22, observed).
narrative_ontology:measurement(gpl_commons_reading_be_t27, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 27, 0.58).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t27, observed).
narrative_ontology:measurement(gpl_commons_reading_be_t32, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement_basis(gpl_commons_reading_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_commons_reading_su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t0, observed).
narrative_ontology:measurement(gpl_commons_reading_su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t5, observed).
narrative_ontology:measurement(gpl_commons_reading_su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t10, observed).
narrative_ontology:measurement(gpl_commons_reading_su_t16, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t16, observed).
narrative_ontology:measurement(gpl_commons_reading_su_t22, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 22, 0.65).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t22, observed).
narrative_ontology:measurement(gpl_commons_reading_su_t27, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 27, 0.58).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t27, observed).
narrative_ontology:measurement(gpl_commons_reading_su_t32, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement_basis(gpl_commons_reading_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'GPL copyleft': the single natural-language concept covers three structurally distinct claims instantiated from one kernel (gpl_reciprocity_obligation). This file authors the commons reading (beneficiary: the commons as institution; victim: exit-maximizing integrators; medium epsilon over the standing arrangement). The freedom reading shifts the beneficiary set toward end users and authors epsilon from a rights-preservation seat; the restriction reading shifts the victim set toward all commercial integrators and authors epsilon from a business-liberty seat. The stories are linked pairwise via affects_constraints so contamination and legitimacy-flow analysis can traverse the family; the upstream freedom reading historically supplies the rhetorical legitimacy the commons reading builds on, which is documented in each file's kernel_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
