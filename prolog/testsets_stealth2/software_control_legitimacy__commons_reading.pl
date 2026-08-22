% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Commons Governance (Commons Reading)
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   Since the late 1990s, control over shared digital infrastructure has been
 *   exercised through stewardship bodies — foundations and consortia that
 *   hold licenses, trademarks, and contribution agreements, and convene
 *   negotiation among corporate sponsors, volunteer maintainers, and
 *   affiliated institutions. The arrangement's premise is that control
 *   questions are governance questions: no party holds absolute claim over
 *   jointly produced and jointly relied-upon code, and outcomes are set by
 *   negotiated collective management rather than by either unilateral
 *   enclosure or unconditional release. This story fixes epsilon to the
 *   standing arrangement it is about — the operational commons-governance
 *   regime as it actually runs, in its representative foundation-stewarded
 *   mixed form — assessed by this reading's own lights: the
 *   negotiated-management core is legitimate, and the measured extraction
 *   reflects what that reading itself concedes, namely capture trends,
 *   minority exclusion at the governance table, and skewed conversion of
 *   contributor labor into sponsor value. Degenerate instantiations (wholly
 *   captured faux-open governance) would be separate stories under the
 *   epsilon-invariance rule, linked through the network. KEY AGENTS (by
 *   structural relationship): - foundation_steward_bodies: Agenda-setting
 *   administrator (institutional/constrained) — runs licenses, trademarks,
 *   governance; authority exists only inside the regime -
 *   corporate_infrastructure_contributors: Dual-positioned sponsor
 *   (powerful/arbitrage) — pays engineer labor, collects amplified product
 *   value - independent_volunteer_maintainers: Labor-bearing participant
 *   (moderate/identity_locked) — supplies maintenance, carries the load,
 *   fused with project identity - proprietary_enclosure_vendors:
 *   Denied-appropriation payer (powerful/arbitrage) — forgoes enclosure
 *   rents, adapts via complements and dual licensing -
 *   free_software_absolutist_advocates: Subordinated-program payer
 *   (organized/identity_locked) — accepts negotiated outcomes as second-best
 *   - unseated_downstream_users: Excluded bearer (powerless/trapped) — runs
 *   the infrastructure, holds no seat - commons_governance_researchers:
 *   Analytical observer (institutional/analytical) — studies capture,
 *   participation, and legitimacy from outside every seat
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.42).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.38).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Negotiated Commons Governance (Commons Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "economic/political/technological").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'c3451bb4-4bb3-4cfe-8fec-c9b45c350f81').
narrative_ontology:cs_kernel_codification('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', distributed).
narrative_ontology:cs_authority_grounding('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', distributed).
narrative_ontology:cs_reading_relation('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_axiom('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', foundational, no_absolute_control_claim_over_shared_infrastructure).
narrative_ontology:cs_axiom_status(no_absolute_control_claim_over_shared_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', no_absolute_control_claim_over_shared_infrastructure, conventional).
narrative_ontology:cs_axiom('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', secondary, affected_parties_hold_governance_voice_claims).
narrative_ontology:cs_axiom_status(affected_parties_hold_governance_voice_claims, holdable).
narrative_ontology:cs_axiom_grounding('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', affected_parties_hold_governance_voice_claims, deontological).
narrative_ontology:cs_reference_frame('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', polycentric_negotiated_stewardship).
narrative_ontology:cs_drift_state('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', contemporary_platformization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3451bb4-4bb3-4cfe-8fec-c9b45c350f81', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, foundation_steward_bodies).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, corporate_infrastructure_contributors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, commons_stakeholder_communities).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, proprietary_enclosure_vendors).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, free_software_absolutist_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, unseated_downstream_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, independent_volunteer_maintainers).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, corporate_infrastructure_contributors).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, independent_volunteer_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the licenses, trademarks, contribution agreements, and governance processes through which shared digital infrastructure is managed. Collect membership dues and trademark-licensing revenue, and convene the negotiations that allocate decision weight among sponsors, volunteers, and affiliates. Their authority exists only inside the regime they steward; dissolving or transferring stewardship would mean surrendering the institution itself, so exit is constrained even where dissatisfaction arises.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, foundation_steward_bodies, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, foundation_steward_bodies, beneficiary).

% Fund paid engineering labor into shared projects and receive in return a pool of infrastructure they did not have to build alone, plus influence over technical direction proportional to their contribution weight. They pay in kind and occasionally chafe at governance limits on appropriation of the commons they help maintain, but they retain strong outside options: forking, proprietary complements, dual licensing, or withdrawing staff while keeping expertise.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, corporate_infrastructure_contributors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, corporate_infrastructure_contributors, payer).

% Supply unpaid review, maintenance, and support labor that keeps the shared stack running, receiving reputation, working tools, and community standing in return. They carry a disproportionate share of long-tail maintenance while decision weight concentrates among funded sponsors. Their professional and community identity is fused with their project roles; walking away means abandoning work and relationships that constitute who they are, so exit is nominally possible but practically costly.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, independent_volunteer_maintainers, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, independent_volunteer_maintainers, beneficiary).

% Prefer exclusive control over components they rely on or helped create; the regime denies them full enclosure rights over shared infrastructure regardless of investment. They adapt by selling proprietary complements, negotiating dual-license exceptions, or funding friendly forks, so their losses are forgone rent streams and constrained strategy rather than existential harm. They lobby and litigate against expansions of collective control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, proprietary_enclosure_vendors, payer,
    powerful, biographical, arbitrage, global).

% Campaign for uncompromised user control over computing and treat negotiated licensing compromises as betrayals of first principles. The regime subordinates their program to bargaining outcomes in which their claims routinely yield. Their identity is fused with the cause itself, so exit would mean abandoning the moral project; they remain as internal critics whose objections shape discourse without commanding votes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, free_software_absolutist_advocates, payer,
    organized, generational, identity_locked, global).

% Run businesses, services, and systems on top of the collectively managed infrastructure but hold no governance seat anywhere in the regime. They absorb roadmap reversals, security-posture changes, and dependency churn decided over their heads, and their switching costs away from entrenched infrastructure components are high enough that exit is theoretical rather than real.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, unseated_downstream_users, excluded,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, unseated_downstream_users, payer).

% Study participation, capture, and legitimacy in software stewardship bodies: maintainer-affiliation surveys, decision-trail analysis, and comparative studies of foundation governance. They publish findings that the regime's own participants cite selectively, and they see the full structure from no seat inside it.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_governance_researchers, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared digital infrastructure — languages, runtimes, cryptographic libraries, web servers, package ecosystems — suffers classic public-goods failure if each firm builds and controls its own copy: duplicated effort, incompatible forks, fragmented security response. The regime solves this once, centrally: common repositories, pooled maintenance, compatible licensing, and a standing table where rival firms and volunteers negotiate technical direction.
% TRANSFER_FUNCTION: Moves labor, decision authority, and appropriable value. Unpaid and sponsored engineering labor flows into commonly held assets; decision authority concentrates in steward bodies and large sponsors; enclosure rents that would have accrued to exclusive owners are forgone to the commons; and a portion of contributor-generated value accrues to sponsoring firms in excess of their input share.
% ABSENT_VOICES: Unseated downstream users and non-participating small developers would object to decisions taken without them; they stand outside every governance body, represented only by proxy through sponsor-funded seats. Absolutist advocates on both wings hold loud voices but no veto, and their systematic out-voting is precisely the cost the regime imposes on them.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, firms would retreat to private stacks and hostile forks within quarters, interoperability would decay, duplicated security response would fragment, and both absolutist camps would lose the negotiating counterparty their positions are defined against. The entire modern software supply chain is organized around the assumption of collectively managed shared infrastructure.
% FOUNDING_PROBLEM: From the 1980s onward, software culture split between proprietary enclosure, which legally locked code away, and free-software absolutism, which demanded unconditional user control; between them, shared infrastructure was chronically under-provisioned and legally hazardous to reuse. The founding problem was constructing durable collective management of shared code that neither camp would destroy and that firms could join without surrendering survival.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by commons-governance scholarship in the Ostrom lineage documenting both the persistent under-provisioning problem and the recurring capture risk, by standards-body and litigation records showing continued enclosure/free conflicts, and by competition-authority dockets examining concentration of decision weight in stewardship bodies. No seat inside the regime disputes that the underlying coordination problem persists; the disputable question is who bears its costs.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the regime delivers real collective goods while systematically converting contributor labor into sponsor value and excluding identifiable classes from the table — the reading itself holds that extraction varies with the quality of commons rules, and the representative modern instantiation sits mid-range. Suppression is moderate-low (0.38): exits genuinely exist (forking, proprietary complements, withdrawal), but ecosystem gravity, license webs, and identity fusion make them costly, and the constraint's persistence depends on actively maintained enforcement machinery — license compliance, trademark policing, contribution-agreement regimes — not spontaneous consent. Theater is low-moderate (0.25) and rising: most governance activity is functional, but consultative performance ('community feedback' that does not bind) grows as openness becomes a reputational asset, which is the Goodhart-drift signature the temporal series tracks. Accessibility collapse is moderate (0.45): understanding the regime does not close alternatives — private development remains legal and practiced — but at infrastructure level the network effects make opting out progressively less viable. Resistance is elevated (0.55) because both wings actively fight the regime: enclosure interests litigate and lobby against collective-control expansions, absolutist advocates delegitimize each compromise, and volunteers organize over load and voice. Enforcement is declared active because the tangled-rope structure requires it: without steward enforcement of licenses, trademarks, and governance rules, the negotiated equilibrium unravels. The temporal series runs on ONE shared grid (1998, 2003, 2008, 2013, 2019, 2026) with all three metrics authored at every point; the suppression_requirement series is authored deliberately because the story specifically tracks enforcement-capacity build-out (the maturation of CLA regimes, compliance programs, and conduct enforcement), not mere extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify very differently, and the engine computes that divergence from the structural data. From the steward seat the arrangement is hard-won neutrality that keeps rivals cooperating; from the corporate sponsor seat it is a favorable bargain — influence and pooled assets for in-kind payment, with arbitrage-grade exit damping any felt extraction. From the volunteer maintainer seat the same structure operates as labor drain with diminishing voice, amplified by identity lock; from the absolutist advocate seat it is institutionalized betrayal of principle; from the unseated downstream user seat it is decisions made over their heads by parties who never consulted them. One constraint, five experienced realities — the per-seat computation is the measurement, and the authored claimed_type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality follows the beneficiary/victim declarations and exit structure. Foundation steward bodies derive near-beneficiary directionality (d low): they collect authority and dues and bear only enforcement costs. Corporate infrastructure contributors sit near symmetric (d ~0.5): genuine beneficiary of pooled assets and influence, genuine payer of constrained appropriation and governance friction, with arbitrage exit pulling their effective extraction down. Independent volunteer maintainers derive target-side directionality (d high) amplified by identity_locked exit — trapped-or-locked targets sit nearer the full-target end than mobile ones. Proprietary enclosure vendors derive target-side directionality despite their power and arbitrage options: they are declared victims of denied appropriation, and their exit options soften but do not invert that relationship. Free-software absolutist advocates are identity-locked payers whose effective extraction is amplified by lock. Unseated downstream users combine powerless power, trapped exit, and victim declaration — maximal effective extraction among the seats, borne by the least able to answer. Global spatial scope scales effective extraction modestly upward for all seats (verification is harder at planetary scale); suppression is left unscaled as a raw structural property. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already differentiate every seat, including the two same-power, same-exit pairs (sponsors vs enclosure vendors; both identity-locked payers), because their role declarations diverge.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: governing shared infrastructure amid enclosure and absolutism remains unsolved, so no obsolescence declaration is warranted and the mandate has not outlived its function. The tangled-rope claim does double preventive work. Against the absolutist mislabeling — reading the whole regime as pure extraction because identifiable classes pay — the declared coordination function and beneficiary set preserve the genuine collective-management core that a snare verdict would erase. Against the industry-friendly mislabeling — reading the regime as pure coordination because everyone nominally participates — the declared victim set, active-enforcement flag, and rising theater trajectory preserve the measurable asymmetries (labor-value skew, minority exclusion, capture trend) that a rope verdict would whitewash. Mandatrophy resolution here is not a transition to a successor arrangement but a standing obligation to keep the extraction component visible while the coordination component persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the software_control_legitimacy kernel — the commons_reading. How would the classification change if the same governance terrain were instantiated under a sibling reading?',
    'Author the sibling stories (software_control_legitimacy__freedom_imperative_reading, __pragmatic_openness_reading, __property_rights_reading), each with its own epsilon referenced to its own standing arrangement, then compare per-seat classifications across the family.',
    'Under property_rights_reading, creators and enclosure-seeking vendors move to beneficiary seats and commons obligations become the extraction surface; under freedom_imperative_reading, proprietary vendors and compromising stewards become the primary targets and epsilon rises sharply; under pragmatic_openness_reading the constraint demotes to an optional methodology with negligible standing extraction. The disagreement is located in the foundational axiom: whether any party holds an absolute control claim over shared code.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story instantiates the commons_reading of the software_control_legitimacy kernel; siblings are separate constraints.').

omega_variable(
    representative_instantiation_class,
    'Is the representative instantiation for epsilon the mainstream foundation-stewarded mixed regime (authored here), or should the family decompose further into healthy-stewardship and captured-fauxpen sub-constraints?',
    'Cluster real stewardship bodies on capture indicators (decision-weight concentration, CLA direction, seat composition); if the clusters separate cleanly with materially different epsilon, split into two linked stories per the epsilon-invariance rule and re-reference each.',
    'If healthy and captured instantiations are one population, the authored mid-range epsilon stands; if they separate, epsilon for this story should drop toward rope levels for the healthy cluster while the captured cluster warrants its own substantially more extractive story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representative_instantiation_class, conceptual, 'Whether one mid-range epsilon honestly represents the commons-regime population or masks a bimodal decomposition.').

omega_variable(
    governance_capture_concentration,
    'How much of actual decision weight in major stewardship bodies is effectively controlled by the small number of firms contributing paid developer labor, versus distributed across independent contributors?',
    'Longitudinal maintainer-affiliation analysis, RFC outcome-by-sponsor studies, and contribution-agreement holder concentration data across major foundations.',
    'High sponsor concentration raises effective extraction on volunteer maintainers and unseated downstream users and pushes payer-seat classifications toward snare; distributed decision weight supports the tangled_rope-to-rope band at most seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_capture_concentration, empirical, 'Empirical degree of sponsor capture of commons governance decision weight.').

omega_variable(
    absolutist_cost_baseline,
    'Are the costs the regime imposes on both absolutist wings genuine extraction (suppression of legitimate claims), or the ordinary price any binding governance charges some affected class — a cost no feasible alternative regime avoids?',
    'Baseline comparison across feasible governance designs: enumerate regimes that grant the absolutist wings their full claims and measure what each costs the other seats; if every feasible design imposes comparable costs somewhere, the absolutist burden is irreducible coordination cost.',
    'If the burden is irreducible, epsilon falls toward the rope band and the victim declarations re-read as priced-out preferences; if exclusion is discretionary — seats could be widened or vetoes granted at low cost and are not — the extraction is real and the tangled_rope verdict hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_cost_baseline, conceptual, 'Whether the minority-exclusion component of measured extraction is structural necessity or discretionary design choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__commons_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(soft_tr_t2003, software_control_legitimacy__commons_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__commons_reading, theater_ratio, 2008, 0.17).
narrative_ontology:measurement(soft_tr_t2013, software_control_legitimacy__commons_reading, theater_ratio, 2013, 0.2).
narrative_ontology:measurement(soft_tr_t2019, software_control_legitimacy__commons_reading, theater_ratio, 2019, 0.23).
narrative_ontology:measurement(soft_tr_t2026, software_control_legitimacy__commons_reading, theater_ratio, 2026, 0.25).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__commons_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(soft_be_t2003, software_control_legitimacy__commons_reading, base_extractiveness, 2003, 0.31).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__commons_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(soft_be_t2013, software_control_legitimacy__commons_reading, base_extractiveness, 2013, 0.39).
narrative_ontology:measurement(soft_be_t2019, software_control_legitimacy__commons_reading, base_extractiveness, 2019, 0.41).
narrative_ontology:measurement(soft_be_t2026, software_control_legitimacy__commons_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_control_legitimacy__commons_reading, suppression_requirement, 1998, 0.22).
narrative_ontology:measurement(soft_su_t2003, software_control_legitimacy__commons_reading, suppression_requirement, 2003, 0.25).
narrative_ontology:measurement(soft_su_t2008, software_control_legitimacy__commons_reading, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(soft_su_t2013, software_control_legitimacy__commons_reading, suppression_requirement, 2013, 0.33).
narrative_ontology:measurement(soft_su_t2019, software_control_legitimacy__commons_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(soft_su_t2026, software_control_legitimacy__commons_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'software control' conflates four structurally distinct claims about legitimacy, decomposed per the epsilon-invariance principle into one kernel (software_control_legitimacy) with four readings, each a separate story with its own epsilon, beneficiaries, victims, and type. This story instantiates commons_reading, whose epsilon is referenced to the standing commons-governance arrangement as the reading's own lights assess it. The sibling readings re-reference epsilon to their own endorsed or contested arrangements: property_rights_reading locates extraction in commons obligations imposed on creators; freedom_imperative_reading locates it in proprietary enclosure; pragmatic_openness_reading treats the whole question as instrumentally contingent. Upstream/downstream structure runs through this story's governance machinery: whichever reading dominates legitimacy discourse reshapes the operating environment of the others, which is why the family edges are declared bidirectionally through each file's own affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
