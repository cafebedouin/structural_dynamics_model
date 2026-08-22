% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Federation Selective Solidarity: Tiered Free Movement by Contribution Status
 *   domain: political_economy/federalism/migration_policy/welfare_state
 *
 * SUMMARY:
 *   Federation member states have adopted a tiered free-movement and
 *   welfare-access regime indexed to contribution history and economic
 *   activity status. Workers with documented employment and fiscal
 *   contribution gain unrestricted intra-federation movement and welfare
 *   parity with citizens; economically inactive people (between jobs,
 *   retirees, students, family members, asylum applicants) face escalating
 *   restrictions: conditional welfare, proof-of-funds requirements, mandatory
 *   return-home provisions if welfare dependence accrues, and longer waiting
 *   periods before access. The regime is framed as protecting fiscal
 *   sustainability and preventing welfare-seeking migration; critics frame it
 *   as converting citizenship membership into a market transaction gated by
 *   prior economic performance. This constraint instantiates the SELECTIVE
 *   SOLIDARITY reading of the federation-membership-obligations kernel: free
 *   movement is preserved in principle but welfare access is decoupled from
 *   citizenship and reattached to contribution status. This reading coexists
 *   with the integration-primary reading (which treats free movement and
 *   welfare as indivisible citizenship entitlements) and the
 *   member-sovereignty-primary reading (which treats both as conditional on
 *   member state welfare-system protection). The constraint is CLAIMED as
 *   tangled_rope (real coordination of labor mobility with fiscal limits,
 *   plus asymmetric extraction from inactive migrants); the authored metrics
 *   (extractiveness 0.68, suppression 0.52, theater 0.41) describe a regime
 *   that actively enforces contribution-based gatekeeping. The claim-metric
 *   divergence is intentional; the engine measures whether the real operation
 *   aligns with the coordination framing or runs as pure extraction.
 *
 * KEY AGENTS:
 *   - employed_mobile_workers (beneficiary; moderate power; mobile exit): gain unrestricted movement and welfare parity
 *   - economically_inactive_migrants (victim; powerless; trapped exit): face conditional welfare, proof-of-funds, return-home risk
 *   - high_contribution_states (agenda_setter; institutional power; arbitrage exit): set and enforce the tiering rules unilaterally
 *   - low_contribution_history_workers (victim; powerless; identity-locked exit): face escalating scrutiny and longer waiting periods despite employment
 *   - low_gdp_member_states (excluded; powerful but constrained): lose workers to migration and have their citizens' contribution records discounted
 *   - asylum_and_family_reunification_applicants (victim; powerless; trapped exit): welfare access is entirely derivative of sponsoring worker's contribution
 *   - integration_advocate_coalition (excluded; powerful but constrained): argue tiering violates federation constitutive principles; not in the default frame
 *   - federation_legislative_body (observer; institutional power; constrained exit): formal authority to modify rules but constrained by high-contribution state defection threat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.52).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Federation Selective Solidarity: Tiered Free Movement by Contribution Status").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, 'e067f48c-f0f1-41ab-882d-db82cf118061').
narrative_ontology:cs_kernel_codification('e067f48c-f0f1-41ab-882d-db82cf118061', formalized).
narrative_ontology:cs_authority_grounding('e067f48c-f0f1-41ab-882d-db82cf118061', extraction).
narrative_ontology:cs_interpretation_layer_present('e067f48c-f0f1-41ab-882d-db82cf118061').
narrative_ontology:cs_reading_relation('e067f48c-f0f1-41ab-882d-db82cf118061', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('e067f48c-f0f1-41ab-882d-db82cf118061', federation_membership_obligations__member_sovereignty_primary, influences).
narrative_ontology:cs_axiom('e067f48c-f0f1-41ab-882d-db82cf118061', foundational, welfare_access_contingent_on_contribution_history).
narrative_ontology:cs_axiom_status(welfare_access_contingent_on_contribution_history, holdable).
narrative_ontology:cs_axiom_grounding('e067f48c-f0f1-41ab-882d-db82cf118061', welfare_access_contingent_on_contribution_history, instrumental).
narrative_ontology:cs_axiom('e067f48c-f0f1-41ab-882d-db82cf118061', foundational, fiscal_sustainability_permits_selective_solidarity).
narrative_ontology:cs_axiom_status(fiscal_sustainability_permits_selective_solidarity, holdable).
narrative_ontology:cs_axiom_grounding('e067f48c-f0f1-41ab-882d-db82cf118061', fiscal_sustainability_permits_selective_solidarity, empirically_contingent).
narrative_ontology:cs_reference_frame('e067f48c-f0f1-41ab-882d-db82cf118061', fiscal_sustainability_framework).
narrative_ontology:cs_drift_state('e067f48c-f0f1-41ab-882d-db82cf118061', contemporary_welfare_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e067f48c-f0f1-41ab-882d-db82cf118061', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, high_contribution_states).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, fiscal_administration).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, low_contribution_history_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, asylum_and_family_reunification_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, low_gdp_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers with continuous employment history and documented contribution records gain unrestricted intra-federation movement, access to member state welfare on parity with citizens, and rapid family reunification approval. Their mobility is the justified core of the arrangement; they collect full rights.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    moderate, biographical, mobile, global).

% Workers between jobs, career changers, retirees, students, and family members of mobile workers face tiered restrictions: limited duration of residence, conditional welfare access (must demonstrate financial self-sufficiency or prior contribution), mandatory health insurance, and return-home requirements if welfare dependence accrues. Legal presence is contingent, not assured. Movement between member states is permitted but welfare access is not guaranteed to follow.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_migrants, payer,
    powerless, immediate, trapped, local).

% Wealthy member states with strong fiscal capacity and high per-capita contribution rates set and enforce the tiering rules through selective welfare eligibility, contribution history documentation requirements, and bilateral labor agreements. They frame the arrangement as protecting welfare sustainability against foreseeable fiscal stress; they administer the enforcement machinery (contribution tracking, eligibility verification, deportation authority).
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, high_contribution_states, agenda_setter,
    institutional, generational, arbitrage, regional).

% Workers from lower-income member states, or those with interrupted employment histories, face escalating scrutiny: longer residency requirements before welfare eligibility (5–10 years vs. 0 for citizens), higher proof-of-contribution thresholds, and social benefit restrictions even after the waiting period. Career interruption (illness, caregiving, retraining) is treated as contribution gap. Structural economic inequality maps directly onto tiered mobility and welfare access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, low_contribution_history_workers, payer,
    powerless, biographical, identity_locked, regional).

% Lower-income member states lose workers to migration (brain drain, labor drain) yet their citizens face contribution-history stigma in high-income destinations. They are excluded from setting tiering rules (those are set by high-contribution states unilaterally or through coalitions) but bear the externality of losing productive workers and having their citizens' contribution records systematically discounted. They retain nominal sovereignty but face structural pressure to accept asymmetric terms.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, low_gdp_member_states, payer,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, low_gdp_member_states, excluded).

% Non-worker migrants (family members, asylum seekers, humanitarian applicants) are entirely dependent on the sponsoring worker's contribution status. A spouse's welfare eligibility is derivative of the worker's contribution record; an asylum seeker has no independent claim. Contribution-based tiering extends to humanitarian migration, converting a membership claim into a transaction validated by prior fiscal performance. Their residence and welfare access are proxies for their sponsor's labor history.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, asylum_and_family_reunification_applicants, payer,
    powerless, immediate, trapped, local).

% The administrative doctrine that welfare is a fiscal liability that must be managed through eligibility restrictions. The doctrine treats free movement (a citizenship claim) as conditional on economic contribution (a labor-market claim). Tiering is administratively simpler than universal eligibility, reducing welfare fraud risk and enabling fine-grained cost control — the administrative good that the constraint vindicates.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, fiscal_administration, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(federation_membership_obligations__selective_solidarity, fiscal_administration).

% Civil-rights organizations, labor unions, and some member state governments argue that tiering violates federation constitutive principles: free movement and welfare access are membership rights, not labor-market transactions; contribution history is an instrumentality that converts citizenship into a commodity. They are excluded from the default frame (the tiering rules are authored in fiscal/administrative language) but constitute the primary resistance.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, integration_advocate_coalition, excluded,
    powerful, generational, constrained, regional).

% Inter-member supranational authority that ratifies and can modify the tiering rules. In practice constrained by high-contribution state pressure (which can exit or defund the federation if tiering is dismantled); formal authority exceeds actual discretion. Observes contests between other seats.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federation_legislative_body, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_obligations__selective_solidarity, high_contribution_states).
narrative_ontology:fixing_cost_class(federation_membership_obligations__selective_solidarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents uncontrolled welfare-seeking migration by limiting welfare access to those who have demonstrated capacity for economic self-sufficiency or prior fiscal contribution; solves coordination of open labor mobility with closed welfare systems by tiering rights accordingly. Enables high-income states to set welfare eligibility rules defensively while maintaining nominally open borders for mobile workers.
% TRANSFER_FUNCTION: Moves welfare access rights selectively from economically inactive people and low-contribution-history workers to employed workers and citizens. Transfers administrative burden of contribution-history tracking to individual migrants and lower-income member states. Transfers control of welfare access from federation-level principle to member state discretion, indexed by prior fiscal performance.
% ABSENT_VOICES: Workers in precarious employment (gig, seasonal, informal) have no auditable contribution history and are treated as economically inactive despite working; they are excluded from welfare debate because they are administratively invisible. Non-worker migrants (family members, students, humanitarian applicants) are excluded from the mobility/welfare framing entirely; their situation is treated as dependent, not as an independent membership question.
% DISAPPEARANCE_RATIONALE: If the tiering enforcement vanished, welfare access would become universal within the federation regardless of contribution history; employers would face wage pressure (workers no longer gatekept by contribution requirements); low-income workers would have portable welfare claims; labor mobility would decouple from economic status. The distribution of welfare access and the gatekeeping power would shift from member states to federation-level principle.
% FOUNDING_PROBLEM: Large-scale free movement combined with national welfare systems creates fiscal exposure: member states cannot control in-migration and thus cannot reliably constrain welfare-claim volume; a state's fiscal capacity to provide welfare is no longer bounded by its own population. Tiering was built to allow mobility while preserving member state fiscal autonomy.
% FOUNDING_PROBLEM_CORROBORATION: High-contribution states attest the founding problem is live and acute, citing empirical data on welfare-claim growth correlating with migration. Integration advocates (civil-rights organizations, labor unions, some member states) attest the founding problem is either fabricated (migration's net fiscal effect is positive) or solved (labor-market demand manages selection without welfare gatekeeping). The contest is over whether the founding problem statement itself is accurate or is a cover story for contribution-based discrimination.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint decouples welfare access from citizenship membership and reattaches it to prior fiscal contribution — a transaction rather than an entitlement. The constraint operates asymmetrically: employed workers gain; economically inactive people and low-contribution-history workers lose. Suppression is moderate (0.52) because the regime uses administrative gatekeeping (contribution tracking, eligibility verification, residency waiting periods) rather than direct coercion; the suppression is structural (economic dependency on employment, documentation requirements, welfare conditionality) rather than violent. However, the regime actively enforces return-home provisions and eligibility denials, which constitutes active enforcement machinery. Theater is moderate-low (0.41): the fiscal-sustainability framing is partly genuine (welfare systems are fiscally constrained) but increasingly defensive (the net fiscal effect of low-contribution workers is empirically contested). The measurement series tracks extraction and suppression rising over the interval as the tiering rules are refined and made more granular — contribution-history thresholds tighten, waiting periods lengthen, and administrative burden on workers increases. Suppression requirement (the active force needed to maintain the constraint) is lower than base extractiveness because many workers voluntarily (or strategically, given trapped alternatives) internalize the tiering logic — 'I should wait until I am employed before moving' becomes self-policing rather than externally enforced.
 *
 * PERSPECTIVAL GAP:
 *   The high-contribution states' seat (agenda_setter) experiences the constraint as coordinating legitimate security against fiscal overload; the constraint is rationally necessary. The economically_inactive_migrants' seat experiences it as arbitrary gatekeeping; the same rule structure appears as pure extraction. The employed_mobile_workers' seat experiences it as beneficial (they gain full rights); low-contribution-history workers' seat experiences it as discriminatory (they work but are treated as risks). The federation_legislative_body observes all these gaps but is structurally constrained by the threat of high-contribution state defection if tiering is dismantled — formal authority diverges from practical discretion. The engine computes per-seat directionality from these structural asymmetries; the agenda_setter seat's d is near 0 (full beneficiary), while the victim seats' d approaches 1.0 (full target). The constraint's computed type may differ by seat: from the high-contribution state view, tangled_rope (coordination + asymmetric extraction); from the economically_inactive migrants' view, snare (pure extraction, no meaningful coordination benefit).
 *
 * DIRECTIONALITY LOGIC:
 *   High-contribution states are structural beneficiaries: they retain effective control over welfare access despite nominally open borders, they shift administrative burden to workers and lower-income states, and they collect the fiscal discipline dividend (lower welfare claims per capita). Their directionality is near 0.1 (full beneficiary). Employed mobile workers gain full movement and welfare parity; their directionality is near 0.2 (beneficiary with some cost). Economically inactive migrants are the clear targets: they lose welfare access, face proof-of-funds burden, and carry return-home risk; their directionality is near 0.95 (full target). Low-contribution-history workers are targets despite being employed: their directionality is near 0.85 (substantial target) because employment alone does not secure access — contribution history and economic-status stigma persist. Low-gdp member states are payers in an inter-state sense: they lose workers and have their citizens' contribution records systematically discounted; their directionality is near 0.7 (asymmetric target). The integration_advocate_coalition is excluded rather than directed; their seat does not appear in directionality calculations (it is observer-like) but constitutes high resistance (0.71), which indexes the strength of contestation. Federation_legislative_body's directionality is analytic (the role is observer); it computes no extraction because it neither collects nor bears the constraint's costs directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (free movement + national welfare = fiscal exposure) is contested. High-contribution states attest it is live and acute; integration advocates attest it is either fabricated (the empirical finding is that migration's net fiscal effect is positive, not negative) or already solved (competitive labor markets select for employed workers without gatekeeping). The constraint persists despite this contest because high-contribution states have structural power: they control welfare eligibility rules unilaterally, they can threaten defection from the federation, and they have administrative capacity to implement tiering. The contest is not over whether the founding problem exists but over whether it is *characterized correctly* — is it a fiscal problem (requiring tiering) or a discrimination problem (requiring integration)? The mismatch vector is: founding_problem_status = contested (parties dispute the facts) + disappearance_verdict = world_rearranges (the constraint structures welfare access; removing it would cause reorganization). This combination suggests the constraint is not a false summit (which would require status=dead + verdict=world_rearranges, indicating a zombie rule). Instead, the constraint is a tangled_rope with unresolved founding-problem contestation: the coordination story (protecting fiscal sustainability) coexists with the extraction story (gatekeeping welfare access by prior contribution status) and neither has resolved decisively. Mandatrophy is not present because the founding problem is still contested; if the problem were demonstrably dead (empirical consensus that migration's net effect is positive), the persistence of tiering would indicate mandatrophy (the rule persists despite its founding justification evaporating). Current state: the contest persists, the constraint persists, the founding problem status remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Is the founding problem (free movement + national welfare = fiscal overload) empirically real, or is it a constructed threat narrative?',
    'Systematic analysis of migration flows, welfare-claim rates, and net fiscal impact of mobile workers across member states. Comparison with counterfactual: what would welfare claims be under full open access (no tiering)?',
    'If the founding problem is empirically confirmed as acute, tiering is justified as coordination. If the founding problem is disproven or overstated, tiering becomes pure extraction using fiscal narrative as cover; the constraint reclassifies from tangled_rope to snare (or piton if the threat becomes widely discredited but tiering persists theatrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether the founding problem is real or a constructed threat narrative.').

omega_variable(
    contribution_history_as_proxy_for_sustainability,
    'Does contribution history (prior employment and fiscal payment) actually predict welfare-claim restraint, or is it a proxy that embeds existing inequality into tiering rules?',
    'Cohort analysis: compare welfare-claim trajectories of economically inactive migrants who do eventually gain access (after tiering waiting period expires) vs. those who are never active. If claim rates are similar (high) regardless of contribution history, the premise fails; contribution history is proxy, not predictor.',
    'If contribution history predicts restraint, it is a defensible selection mechanism (coordination). If it does not predict restraint, it is a discrimination mechanism (tiering becomes a tool for wealth-based welfare gatekeeping, reclassifying as snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contribution_history_as_proxy_for_sustainability, empirical, 'Whether contribution history predicts welfare-claim restraint or is a proxy for wealth-based gatekeeping.').

omega_variable(
    federation_commitment_to_solidarity_vs_closure,
    'Is the federation constitutively committed to welfare-access universalism (solidarity principle), or is selective gatekeeping a legitimate reading of federation authority?',
    'Treaty interpretation, jurisprudence from federation supreme court, and member state negotiation outcomes. Does federation founding text support universal welfare claims for members, or does it preserve member state discretion?',
    'If founded on solidarity (universal principle), selective_solidarity forecloses integration_primary by attempting to preserve gatekeeping within a solidarity framework — a contradictory reading that can only persist if the founding text is genuinely ambiguous. If founded on sovereignty (member state discretion), selective_solidarity is a defensible intermediate position between integration_primary and member_sovereignty_primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federation_commitment_to_solidarity_vs_closure, conceptual, 'Whether the federation is constitutively committed to welfare universalism or selective gatekeeping.').

omega_variable(
    identity_lock_mechanism_in_low_contribution_workers,
    'Is the suppression of low-contribution-history workers structural (economic dependency, documentation barriers, waiting periods) or internalized (they have accepted the tiering logic as legitimate)?',
    'Post-access trajectory: if workers gain full access after waiting period and then show high welfare-claim rates, suppression was purely structural; if they show restraint even after access is secured, suppression is partly internalized. Also: surveys of workers'' perception of tiering legitimacy vs. coercion.',
    'If suppression is purely structural, removing tiering rules would immediately change behavior (workers would move earlier). If suppression is internalized, removing rules would have delayed effects as workers unlearn the tiering logic. Internalized suppression makes the constraint more extractive (higher effective χ) because it persists even when formal barriers are removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_low_contribution_workers, empirical, 'Whether suppression of low-contribution workers is structural or internalized.').

omega_variable(
    selective_solidarity_vs_member_sovereignty_boundary,
    'Is selective_solidarity structurally distinct from member_sovereignty_primary, or does selective_solidarity collapse into member_sovereignty once the fiscal justification is accepted?',
    'Test case: if high-contribution states adopt selective_solidarity tiering, do they resist member_sovereignty arguments (which would allow more aggressive closure)? Or does accepting tiering as legitimate open the door to further restrictions justified on fiscal grounds (member_sovereignty_primary reading)?',
    'If selective_solidarity is a stable intermediate reading, it can coexist with integration_primary as a contested position. If it collapses into member_sovereignty, it is a stepping stone to more aggressive closure and becomes a rhetorical fig leaf rather than a distinct principle. The constraint''s long-term classification (piton vs. tangled_rope) depends on whether it settles as a stable compromise or drifts toward member_sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_solidarity_vs_member_sovereignty_boundary, conceptual, 'Whether selective_solidarity is a stable reading or collapses into member_sovereignty_primary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.31).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.35).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.38).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.4).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__selective_solidarity, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__selective_solidarity, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__selective_solidarity, suppression_requirement, 25, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% The federation_membership_obligations kernel has three structurally distinct readings: integration_primary (welfare access is a citizenship entitlement, mobility trumps fiscal closure), member_sovereignty_primary (welfare systems retain closure authority, mobility is conditional), and selective_solidarity (this story — tiering by contribution history as a hybrid attempt to preserve both). The three readings share a common kernel (the standing arrangement: open movement + national welfare systems) but instantiate different constraint structures with different beneficiaries, victims, and mechanisms. Each reading is a separate constraint story with its own ε, its own stakeholders, and its own computed type. The three are linked via network.affects_constraints so that analysis of one reading (e.g., observing that tiering is increasingly extractive) can propagate to hypothesis about the others (e.g., whether member_sovereignty is the attractor state for selective_solidarity drift).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
