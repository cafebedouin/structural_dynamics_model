% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__user_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__user_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__user_centric_reading
 *   human_readable: Fair Use as Affirmative User Right (User-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   A US fair-use regime read through the user-centric lens: unauthorized use
 *   of copyrighted works is treated as an affirmative entitlement of users,
 *   held against owner exclusivity, with the four statutory factors weighed
 *   so as to preserve public access and ongoing cultural production. Creator
 *   compensation is subordinated: rights holders bear uncompensated
 *   displacement wherever a use serves the public-access purpose. This file
 *   instantiates ONE reading of the shared four-factor kernel (see
 *   kernel_context); the sibling readings are separate constraints with their
 *   own epsilon, victim sets, and classifications. The claim/metric gap is
 *   deliberate: the reading CLAIMS a balanced coordination arrangement
 *   (tangled_rope: genuine coordination function, identifiable paying
 *   minority, active judicial enforcement) while the authored metrics
 *   describe the arrangement as the reading itself assesses it, with low
 *   extraction, a hardening enforcement burden, and growing performative
 *   factor-recitation. The engine measures the divergence; nothing here
 *   reconciles claim to metrics. KEY AGENTS (by structural relationship): -
 *   federal_courts: agenda_setter (institutional/constrained) - administers
 *   factor-weighing, sets operative meaning - congress_ip_committees:
 *   agenda_setter (institutional/mobile) - statutory author, retains rewrite
 *   power - public_access_users: primary beneficiary (moderate/constrained) -
 *   receives access at zero license price - educational_institutions:
 *   beneficiary (institutional/constrained) - libraries_and_archives:
 *   beneficiary (institutional/constrained) - criticism_parody_authors:
 *   beneficiary (moderate/identity_locked) - practice fused with unauthorized
 *   quotation - platform_intermediaries: secondary beneficiary
 *   (institutional/arbitrage) - captures the monetized share -
 *   commercial_rights_holders: primary target (institutional/arbitrage) -
 *   bears uncompensated displacement - working_creators: target
 *   (moderate/constrained) - thin-margin licensing losses, dual-positioned -
 *   foreign_moral_rights_creators: excluded (moderate/trapped) - governed
 *   without a seat - legal_academy: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__user_centric_reading, 0.3).
domain_priors:suppression_score(fair_use_four_factor_test__user_centric_reading, 0.6).
domain_priors:theater_ratio(fair_use_four_factor_test__user_centric_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fair_use_four_factor_test__user_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__user_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__user_centric_reading, "Fair Use as Affirmative User Right (User-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__user_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__user_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__user_centric_reading, '11ca2174-04da-41e0-be13-94e6adad43d5').
narrative_ontology:cs_kernel_codification('11ca2174-04da-41e0-be13-94e6adad43d5', fixed_text).
narrative_ontology:cs_authority_grounding('11ca2174-04da-41e0-be13-94e6adad43d5', lineage).
narrative_ontology:cs_interpretation_layer_present('11ca2174-04da-41e0-be13-94e6adad43d5').
narrative_ontology:cs_reading_relation('11ca2174-04da-41e0-be13-94e6adad43d5', fair_use_four_factor_test__creator_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('11ca2174-04da-41e0-be13-94e6adad43d5', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('11ca2174-04da-41e0-be13-94e6adad43d5', foundational, user_entitlement_primacy).
narrative_ontology:cs_axiom_status(user_entitlement_primacy, holdable).
narrative_ontology:cs_axiom_grounding('11ca2174-04da-41e0-be13-94e6adad43d5', user_entitlement_primacy, deontological).
narrative_ontology:cs_axiom('11ca2174-04da-41e0-be13-94e6adad43d5', secondary, market_harm_subordinate_to_public_access).
narrative_ontology:cs_axiom_status(market_harm_subordinate_to_public_access, holdable).
narrative_ontology:cs_axiom_grounding('11ca2174-04da-41e0-be13-94e6adad43d5', market_harm_subordinate_to_public_access, instrumental).
narrative_ontology:cs_reference_frame('11ca2174-04da-41e0-be13-94e6adad43d5', user_right_public_access_balancing).
narrative_ontology:cs_drift_state('11ca2174-04da-41e0-be13-94e6adad43d5', contemporary_ai_training_litigation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('11ca2174-04da-41e0-be13-94e6adad43d5', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__user_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, public_access_users).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, libraries_and_archives).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, criticism_parody_authors).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, platform_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, commercial_rights_holders).
narrative_ontology:constraint_victim(fair_use_four_factor_test__user_centric_reading, working_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__user_centric_reading, working_creators).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, first_amendment_expression_values).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__user_centric_reading, cumulative_culture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which unauthorized uses of copyrighted works are permitted, weighing the four statutory factors case by case and setting precedents that bind later decisions. Nothing material flows to courts from the doctrine's operation; their stake is institutional, in the legitimacy of principled adjudication. They are bound by the statute and by stare decisis; stepping outside the framework would forfeit the basis of their own authority.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Wrote the four-factor statute in 1976 and retain power to rewrite or repeal it, but have declined to materially amend it despite decades of lobbying from both sides. Gains and losses from the doctrine's daily operation bypass them directly; their exposure is political, pressed by constituent industries on one side and public-interest constituencies on the other.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, congress_ip_committees, agenda_setter,
    institutional, generational, mobile, national).

% Readers, researchers, journalists, and hobbyists who quote, copy, and build on published works without paying license fees. Access to the cultural record reaches them at zero marginal price wherever their uses fall inside the doctrine's protective perimeter. They cannot opt out of the copyright system covering everything they might want to read; their practical alternative, negotiating licenses work by work, is usually unavailable or uneconomic at their scale.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, public_access_users, beneficiary,
    moderate, generational, constrained, national).

% Universities and schools that reproduce excerpts for teaching, course packs, and digital learning platforms. They receive instructional flexibility that per-use licensing would price out of existence. They face periodic licensing demands and lawsuits; their alternative is negotiating blanket licenses, which they purchase where offered but which never cover the full range of classroom practice.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, educational_institutions, beneficiary,
    institutional, generational, constrained, national).

% Preserve, digitize, and lend the published record, relying on the doctrine to copy for preservation, open access to orphan works, and support computational research. Their collections would be legally unusable at scale under strict per-copy permission. Their exit, refusing to preserve or lend, would contradict their statutory mission.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, libraries_and_archives, beneficiary,
    institutional, generational, constrained, national).

% Critics, satirists, reviewers, and essayists whose work consists of quoting, excerpting, and recontextualizing existing works. Their output is impossible without borrowing from the works they address; no licensing negotiation substitutes for the immediacy of quotation. Their livelihood rides on the borrowing remaining lawful, and their professional identity is fused with the practice of unauthorized reference.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, criticism_parody_authors, beneficiary,
    moderate, biographical, identity_locked, national).

% Search engines, hosting services, and AI developers that index, snippet, host, and train on massive corpora of copyrighted works. Monetized value flows to them directly: advertising around indexed content, subscription products built on ingested texts. They fund the leading fair-use litigation and could, at scale, buy licenses or relocate processing offshore; their participation is chosen, not compelled.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, platform_intermediaries, beneficiary,
    institutional, generational, arbitrage, global).

% Publishers, studios, labels, and estates that own large catalogs and sell licenses. Every unauthorized use the doctrine shelters is revenue they did not collect and control they did not exercise. They respond with litigation, lobbying for narrower readings, digital locks, and contract terms that route around the doctrine; their catalogs and capital give them many partial exits, though none restores full pre-doctrine control.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, commercial_rights_holders, payer,
    institutional, generational, arbitrage, global).

% Individual authors, illustrators, photographers, and musicians dependent on licensing fees and reprint income. When their works are quoted, reposted, or absorbed into training corpora without payment, the loss lands on thin margins. They themselves read and borrow from the surrounding culture, so they sit on both sides of the exchange, but they lack the legal budgets to litigate and the catalog leverage to force licensing deals; their recourse is trade associations and small-claims pressure.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, working_creators, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__user_centric_reading, working_creators, beneficiary).

% Authors publishing under European and other moral-rights regimes whose works circulate in the US market, where the doctrine applies to their works without their consent and without a seat for them in the adjudication that decides. They would contest unauthorized modification and unattributed commercial reuse; their objections register only dimly, through treaties and publisher intermediaries.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, foreign_moral_rights_creators, excluded,
    moderate, biographical, trapped, continental).

% Scholars who map the doctrine's evolution, publish the treatises judges cite, and supply the competing interpretive frames now in dispute. They collect nothing from the doctrine's operation and their stake is analytic; they observe from outside enforcement and benefit from no particular resolution.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__user_centric_reading, legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__user_centric_reading, platform_intermediaries).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__user_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the clearance problem for socially valuable secondary uses: criticism, scholarship, teaching, preservation, and indexing require immediate access to existing works, and work-by-work licensing either refuses these uses outright or prices them out of existence. The four-factor weighing coordinates owners and users on which unauthorized uses proceed without permission.
% TRANSFER_FUNCTION: Moves uncompensated use value from rights holders to users: access, quotation, reproduction, and training ingestion flow to readers, institutions, and platforms at zero license price, while forgone licensing revenue and surrendered control flow away from publishers, studios, and working creators.
% ABSENT_VOICES: Foreign moral-rights creators whose works circulate under the doctrine without consent or a seat in adjudication; future creators whose not-yet-existing works will be absorbed without payment; licensing intermediaries denied a market wherever the doctrine shelters the use. They stand outside US courtrooms and congressional hearings, registering objections only through treaties and publisher proxies.
% DISAPPEARANCE_RATIONALE: Repealing the doctrine overnight would force every quotation, course pack, library scan, search index, and training corpus into per-use licensing: educational access would collapse or prices would soar, platform search and hosting would become mass infringement, and cultural production that builds on prior work would chill while licensing markets ballooned. Both the beneficiaries and the payers reorganize around its absence; nothing about the current configuration survives independently of it.
% FOUNDING_PROBLEM: Exclusive publication rights, applied literally, criminalize the quotation, criticism, teaching, and archiving that free expression and cumulative culture require; the doctrine was built to reconcile owner exclusivity with the public's need to use the cultural record.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 1976 House Report states the balance purpose; successive Supreme Court opinions reaffirm it; and rights holders themselves, the paying seat, attest the problem is live in their own litigation briefs and licensing proposals, which concede the doctrine's balancing function while contesting its breadth. No party denies the founding problem; the parties dispute its resolution.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__user_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__user_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__user_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__user_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__user_centric_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__user_centric_reading_tests).
:- end_tests(fair_use_four_factor_test__user_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.30, low, per this reading's own assessment of its arrangement: unauthorized public-facing use proceeds by right, and the costs imposed on rights holders are weighed and accepted rather than concealed. Suppression is authored at 0.60 as a raw structural property, unscaled by power or scope (only extractiveness is scaled downstream by directionality and scope): the arrangement persists only through sustained judicial defense against narrowing pressure from litigation, lobbying, digital locks, and contract override, and that enforcement machinery has visibly hardened over the interval. Theater rises to 0.46 as factor-recitation increasingly rationalizes outcomes reached on other grounds, a documented pattern in the case law. Accessibility collapse sits at 0.42: a licensing alternative formally persists but fails in practice for criticism, preservation, and indexing, where owners refuse or pricing excludes. Resistance is high at 0.70; few doctrines attract this volume of counter-mobilization. The three measurement series share one grid ({0, 135, 153, 174, 180, 185}): extractiveness declines as the user-right consolidation proceeds from Folsom's thin equity through Campbell and Google Books, then ticks up as AI-era market-substitution rulings push back; theater and enforcement requirement rise monotonically. Claimed type is stated independently of these metrics: the structure, meaning genuine coordination function plus identifiable paying minority plus active enforcement, is what this reading believes true of its own arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical statutory text. From the federal bench the doctrine is a stable equilibrium of weighed factors; from the rights-holder seat the same text operates as uncompensated dispossession met with organized resistance; from the public-user seat it is a birthright perpetually under siege; from the platform seat it is load-bearing infrastructure worth litigating for. Power and exit differentiate same-level actors: platform intermediaries and commercial rights holders both hold institutional power at global scope, but the platforms' arbitrage exit (licenses, offshore processing, technical measures) places them near the beneficiary pole while the rights holders' identical formal power buys only partial exits from the losing pole. Working creators are dual-positioned, paying through lost licensing income while benefiting as consumers of the surrounding culture, which the engine weighs through their secondary role. The engine derives these per-seat classifications from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: public_access_users, educational_institutions, libraries_and_archives, criticism_parody_authors, and platform_intermediaries all receive the arrangement's flows and derive low d, with the platforms' arbitrage-grade exit pinning them nearest the subsidy end despite their scale. Victim declarations drive high d: commercial_rights_holders and working_creators bear the uncompensated transfer, with the working creators' constrained exit sitting them deeper toward the full-target end than the rights holders' arbitrage options. Agenda-setter seats (courts, Congress) derive mid-range d, collecting and paying nothing material. Foreign moral-rights creators carry high d with no seat at all, which is precisely the absent-voices finding. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct ordering, and a power-atom-keyed override would misfire here by striking both institutional beneficiaries and institutional payers at once.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, reconciling owner exclusivity with expression and cumulative culture, remains live in the AI-licensing era, so the genealogy interview returns live crossed with world_rearranges: no zombie mismatch, no mandatrophy declaration. The classification discipline cuts both ways here. Reading the arrangement's low authored epsilon as pure coordination would erase the identified paying seats and the active enforcement the doctrine requires; reading the rights holders' intense resistance as proof of pure extraction would erase the genuine clearance-coordination function that licensing markets demonstrably fail to supply. The tangled-rope structure holds both facts: coordinated users, paying rights holders, and judicial enforcement keeping the balance from collapsing in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the user_centric_reading of the fair_use_four_factor_test kernel; how would the sibling readings re-author the same referent?',
    'Comparative classification across the three reading-stories sharing kernel_id fair_use_four_factor_test: creator_centric_reading authors high epsilon with users positioned as violators; transformative_use_reading collapses four-factor balancing into transformativeness dominance.',
    'The cross-reading epsilon spread over the shared statutory referent is the measurement of the kernel contest; convergence across readings would indicate the three labels name one constraint, not three.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading-indexed values over a shared kernel; this file carries only the user-centric instantiation.').

omega_variable(
    entitlement_bearer_disagreement,
    'Where is the kernel disagreement located: is the protected entitlement held by users (an affirmative right) or by owners (property from which exceptions are carved)?',
    'Doctrinal analysis of how each reading characterizes the default rule: the user-centric reading treats unauthorized public-facing use as presumptively licensed by right; the creator-centric reading treats it as presumptively trespass requiring justification.',
    'Flipping the default flips the victim set and reverses epsilon polarity across readings; the foreclosure relation to creator_centric_reading follows from this axis, since no single framework can hold both defaults for the same act-space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entitlement_bearer_disagreement, conceptual, 'The structural element readings differ on is the bearer of the entitlement, not the four factors themselves.').

omega_variable(
    incentive_shrinkage_empirics,
    'Does broad user-side access measurably reduce creator incentives enough to shrink long-run cultural production?',
    'Natural experiments around landmark expansions (Campbell, Google Books), royalty-panel studies, and controlled licensing-market data comparing output before and after shelter widened.',
    'Confirmed incentive collapse would force this reading''s arrangement to justify itself transitionally rather than as steady state; a null result stabilizes the low authored epsilon as durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_shrinkage_empirics, empirical, 'Whether the reading''s reduced-creator-compensation tradeoff is empirically self-defeating.').

omega_variable(
    platform_capture_of_user_right,
    'Does the user-centric reading in practice serve diffuse public access, or concentrated platform interests riding under the public-access banner?',
    'Trace litigation sponsorship, monetization flows, and settlement patterns: who funds fair-use defenses and who books the resulting revenue.',
    'If platforms capture the gains, the reading''s effective beneficiary structure shifts toward concentrated capture, the public-access justification weakens, and the arrangement drifts toward enforced extraction wearing a coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_of_user_right, empirical, 'Whether the declared public beneficiary set masks a concentrated capturer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__user_centric_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__user_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fair_tr_t135, fair_use_four_factor_test__user_centric_reading, theater_ratio, 135, 0.22).
narrative_ontology:measurement(fair_tr_t153, fair_use_four_factor_test__user_centric_reading, theater_ratio, 153, 0.3).
narrative_ontology:measurement(fair_tr_t174, fair_use_four_factor_test__user_centric_reading, theater_ratio, 174, 0.38).
narrative_ontology:measurement(fair_tr_t180, fair_use_four_factor_test__user_centric_reading, theater_ratio, 180, 0.42).
narrative_ontology:measurement(fair_tr_t185, fair_use_four_factor_test__user_centric_reading, theater_ratio, 185, 0.46).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fair_be_t135, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 135, 0.48).
narrative_ontology:measurement(fair_be_t153, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 153, 0.4).
narrative_ontology:measurement(fair_be_t174, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 174, 0.32).
narrative_ontology:measurement(fair_be_t180, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 180, 0.28).
narrative_ontology:measurement(fair_be_t185, fair_use_four_factor_test__user_centric_reading, base_extractiveness, 185, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(fair_su_t135, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 135, 0.3).
narrative_ontology:measurement(fair_su_t153, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 153, 0.38).
narrative_ontology:measurement(fair_su_t174, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 174, 0.5).
narrative_ontology:measurement(fair_su_t180, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 180, 0.56).
narrative_ontology:measurement(fair_su_t185, fair_use_four_factor_test__user_centric_reading, suppression_requirement, 185, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__user_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__user_centric_reading, transformative_use_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into three structurally distinct constraints, one per reading of the shared four-factor kernel. This story authors epsilon for the user-centric instantiation (low, users as rights-holders, victim set = rights holders); creator_centric_reading authors high epsilon over the same statutory text with the victim set inverted to users; transformative_use_reading replaces factor-balancing with a single transformativeness axis. All three cite the same canon (Folsom v. Marsh, 17 U.S.C. sec. 107, Campbell v. Acuff-Rose) as warrant; the statutory text anchors the family and each reading is linked to its siblings via affects_constraints per the epsilon-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
