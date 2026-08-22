% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Proprietary Software Regime under the Freedom Imperative Reading
 *   domain: economic/political/technological ethics
 *
 * SUMMARY:
 *   The standing arrangement under contest is the global proprietary-software
 *   regime: license agreements drafted solely by publishers, technical
 *   restriction measures backed by anti-circumvention statute, and a
 *   decades-long drift from sold copies toward rented subscriptions and
 *   remotely controlled services. This file is ONE READING of the
 *   software_source_status kernel - the freedom-imperative reading, which
 *   holds the four user freedoms (to run, study, modify, and redistribute) as
 *   inalienable ethical requirements and classifies proprietary distribution
 *   as categorically unjust. Per the kernel-referent rule, epsilon is
 *   authored for the standing arrangement AS THIS READING ASSESSES IT - not
 *   for the free-software commons the reading endorses, which would drive
 *   epsilon toward zero for every advocacy reading and destroy the
 *   measurement. The claim and the metrics are independent authored facts:
 *   claimed_type states this reading's categorical structural judgment; the
 *   metric values state what the arrangement's operation looks like from this
 *   seat. The three sibling readings are separate constraint files linked
 *   through the network block; nothing about them is averaged into this one.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Agenda-setting publisher ([institutional]/[arbitrage]) - drafts the license terms, collects the revenue stream, can pivot distribution models at will
 *   - proprietary_software_users: Primary target ([powerless]/[constrained]) - bears restricted access, price escalation, and lock-in
 *   - enterprise_software_licensees: Organized target with partial offset ([powerful]/[constrained]) - pays heavily, receives support and service commitments in return
 *   - interoperability_developers: Secondary target ([organized]/[constrained]) - barred by license and statute from bridging closed systems
 *   - patent_assertion_entities: Pure collector ([institutional]/[arbitrage]) - collects licensing payments without publishing software
 *   - open_core_hybrid_publishers: Boundary-straddling collector ([powerful]/[arbitrage]) - monetizes openness while withholding rights on the paid layer
 *   - free_software_foundations: Enforcement-and-analysis seat ([organized]/[analytical]) - litigates license violations and documents the arrangement against a fixed standard
 *   - independent_proprietary_developers: Excluded voice ([moderate]/[constrained]) - livelihood depends on the practice this reading pre-classifies as unjust
 *   - technology_legislators: Rule-writing observer ([institutional]/[analytical]) - enacted the anti-circumvention statute that gives license terms force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.78).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.8).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Proprietary Software Regime under the Freedom Imperative Reading").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "economic/political/technological ethics").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'fd4edf5d-7fc9-42a1-bb37-44609252370a').
narrative_ontology:cs_kernel_codification('fd4edf5d-7fc9-42a1-bb37-44609252370a', distributed).
narrative_ontology:cs_authority_grounding('fd4edf5d-7fc9-42a1-bb37-44609252370a', distributed).
narrative_ontology:cs_reading_relation('fd4edf5d-7fc9-42a1-bb37-44609252370a', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('fd4edf5d-7fc9-42a1-bb37-44609252370a', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd4edf5d-7fc9-42a1-bb37-44609252370a', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('fd4edf5d-7fc9-42a1-bb37-44609252370a', foundational, software_freedom_is_ethical_prerequisite).
narrative_ontology:cs_axiom_status(software_freedom_is_ethical_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('fd4edf5d-7fc9-42a1-bb37-44609252370a', software_freedom_is_ethical_prerequisite, deontological).
narrative_ontology:cs_axiom('fd4edf5d-7fc9-42a1-bb37-44609252370a', foundational, proprietary_distribution_categorically_unjust).
narrative_ontology:cs_axiom_status(proprietary_distribution_categorically_unjust, holdable).
narrative_ontology:cs_axiom_grounding('fd4edf5d-7fc9-42a1-bb37-44609252370a', proprietary_distribution_categorically_unjust, deontological).
narrative_ontology:cs_axiom('fd4edf5d-7fc9-42a1-bb37-44609252370a', secondary, user_source_access_inalienable).
narrative_ontology:cs_axiom_status(user_source_access_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('fd4edf5d-7fc9-42a1-bb37-44609252370a', user_source_access_inalienable, deontological).
narrative_ontology:cs_reference_frame('fd4edf5d-7fc9-42a1-bb37-44609252370a', four_freedoms_software_commons).
narrative_ontology:cs_drift_state('fd4edf5d-7fc9-42a1-bb37-44609252370a', contemporary_proprietary_dominance, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('fd4edf5d-7fc9-42a1-bb37-44609252370a', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, patent_assertion_entities).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, enterprise_software_licensees).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, interoperability_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, enterprise_software_licensees).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, open_core_hybrid_publishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publish operating systems, office suites, databases, and cloud services under license terms they alone draft. Revenue arrives as per-seat fees, subscriptions, and marketplace commissions; source code and build pipelines stay internal. They fund industry groups that press for stronger enforcement, and they can reprice, relicense, or discontinue products on their own schedule. Leaving the model is available to them - several have shifted parts of their portfolios to open distribution when it suited them.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Acquire software patent portfolios and collect licensing fees and settlement payments from companies whose products touch the patents. They publish no software and operate no services; their income consists entirely of payments secured by litigation risk. Their exposure to any particular licensing regime is portfolio-shaped and rebalanceable.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, patent_assertion_entities, beneficiary,
    institutional, biographical, arbitrage, global).

% Run software whose inner workings they cannot inspect, whose behavior they cannot alter, and whose redistribution the license forbids. When a vendor raises prices, discontinues a product, or removes a feature, their recourse is to accept the change or attempt a migration whose cost grows with every year of accumulated documents, macros, and integrations built on the vendor's formats.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, constrained, global).

% Sign multi-year agreements covering thousands of seats, paying substantial sums while receiving support contracts, service-level commitments, audit defense, and integration guarantees. Procurement teams negotiate hard, but the negotiation happens inside the vendor's product universe; replacing an accounting stack or office suite mid-operation is a multi-year program with failure modes executives fear.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, enterprise_software_licensees, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, enterprise_software_licensees, beneficiary).

% Build tools that connect, migrate, or extend closed systems - document converters, protocol implementations, accessibility layers. License terms and anti-circumvention statutes bar them from inspecting the formats they must bridge, and cease-and-desist letters arrive when their work threatens a vendor's channel. Some work proceeds under research exemptions; the rest waits for permission that may never come.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, interoperability_developers, payer,
    organized, biographical, constrained, global).

% Publish and defend free licenses, litigate against license violations, run compliance campaigns, and maintain the canonical definitions of user freedom. They accept no proprietary code into their projects and measure the software world against a fixed ethical standard. Their leverage is reputational and legal rather than market-based.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_foundations, observer,
    organized, generational, analytical, global).

% Release a usable core under permissive or copyleft licenses while reserving advanced features, management tooling, or hosted convenience under commercial terms. They take in community contributions and goodwill from the open layer and convert large customers to paid tiers through the closed layer. When community forks threaten the paid tier, they have changed licenses to re-close features.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, open_core_hybrid_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Solo developers and small studios who sell licenses to their own programs as their entire income - shareware authors, niche tool vendors, independent game developers. Inside this reading's frame their practice is classified as wrongdoing before they speak; they would testify that license sales are how an individual programmer eats, and that the alternative funding models on offer have repeatedly failed to feed anyone.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, independent_proprietary_developers, excluded,
    moderate, biographical, constrained, global).

% Draft and amend the statutes that give license terms force - anti-circumvention provisions, contract-enforcement doctrine, patent-term extensions. They hear testimony from vendors, rightsholder groups, libraries, and disability advocates, and periodically revisit exceptions for security research and interoperability. Their national jurisdictions fragment what would otherwise be a uniform worldwide regime.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, technology_legislators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, technology_legislators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Funds and organizes commercial software production: license and subscription revenue finances full-time development, professional support, security response, compatibility testing, and standardization around widely deployed products. Stated without evaluation of whether the funding justifies the restrictions that accompany it.
% TRANSFER_FUNCTION: Moves subscription and license payments from users and organizations to vendors and patent holders, and moves control over software behavior - source access, modification, redistribution - from those who run the software to those who publish it.
% ABSENT_VOICES: Independent developers who sell proprietary software as their livelihood, and ordinary users who rank convenience above source access, are pre-classified inside this reading's frame - the first as perpetrators of injustice, the second as victims who fail to know their own interest. Neither is engaged as a dissenting voice; the property-rights and utilitarian-hybrid readings exist as separate files precisely because their holders are not seated in this one.
% DISAPPEARANCE_RATIONALE: If proprietary licensing and its enforcement machinery vanished overnight, the funded-development model behind most commercial software would collapse faster than volunteer and public alternatives could replace it: enterprises would lose supported toolchains mid-operation, vendors would lose their revenue base, and the software economy would reorganize around whatever mix of commons, patronage, and service models could scale - a multi-year upheaval touching every sector that runs on software.
% FOUNDING_PROBLEM: How to fund software development as a commercial activity once general-purpose computers made copying effectively free - early commercial software houses needed a way to charge for programs that buyers could otherwise duplicate at zero marginal cost.
% FOUNDING_PROBLEM_CORROBORATION: Open-source sustainability research outside the vendor set attests the funding problem is unsolved: the 2019 Harvard Business School / Linux Foundation census found most widely used open-source infrastructure is maintained by unpaid volunteers, and recurring maintainer-burnout and funding-crisis episodes (the Heartbleed and log4shell episodes) were documented from the commons side, not by license sellers. The freedom movement itself concedes the problem is live - its objection is to the licensing solution, not to the existence of the funding problem.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time mapping: t=0 corresponds to 1984 (workstation-era proprietary dominance, the GNU project newly announced), t=8 to 1992, t=16 to 2000 (anti-circumvention statute in force, restriction hardware arriving), t=24 to 2008 (version-3 copyleft, phone-app gatekeeping), t=32 to 2016 (subscription conversion wave), t=40 to 2024. Extractiveness rises from 0.55 to 0.78 because the reading sees each regime shift as deepening the take: perpetual licenses became rentals, shipped binaries became remotely controlled services, and the user's practical ownership shrank at every step. Suppression_requirement is authored as a series because this story specifically traces enforcement-capacity buildup - from contract-only enforcement, through the 1998 anti-circumvention statute (t~14) and restriction-hardware maturation, to app-store gatekeeping and service delivery that removes the artifact a user could modify at all; a static scalar would miss the ratchet. Theater_ratio rises from 0.15 to 0.38 as justification shifted from arguable claims (development funding, support) toward performative ones (security-through-secrecy marketing, open-washing of closed cores, compliance branding). Accessibility_collapse sits at 0.50, deliberately mid-range: the reading's own argument depends on visible working alternatives existing, so exits do not vanish - they are made expensive by network effects, format gravity, and procurement inertia. Resistance is high (0.70): a forty-year movement with litigation arms, campaigns, and institutional memory contests the arrangement continuously. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine, through directionality and scope. All three series share one time grid, with every metric authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is the engine's computation, not this file's verdict. From the vendor seat the arrangement is a business model the vendor designed and can redesign - the lowest-directionality experience, with arbitrage-grade exit. From the end-user seat it is dependency without visibility - the highest-directionality experience, with exits that decay yearly. Enterprise licensees sit between: heavy payment, real offsets, negotiating power that never escapes the vendor's product universe. Interoperability developers experience the arrangement as a wall erected specifically against their work. The second gap is cross-reading: the same structural data, read under the property-rights lights, empties the victim set; read under the pragmatic lights, the payment becomes methodology cost. Which seat's experience defines the arrangement is exactly what the kernel contest is about. Coalition note: end users are individually powerless but historically coalition-capable - procurement leverage, consumer campaigns, and public-sector migration mandates are the reading's own strategy presupposition, so the powerless atom understates achievable counter-power.
 *
 * DIRECTIONALITY LOGIC:
 *   Vendors and patent collectors are declared beneficiaries: license and settlement payments flow to them, and their exit options are the strongest in the story, placing them near the beneficiary end of directionality. End users, enterprise licensees, and interoperability developers are declared victims: they carry the payments and the lost control, with constrained exits placing them near the target end. Enterprise licensees carry a secondary beneficiary role - support, service levels, audit defense - which moderates but does not reverse their position. Free-software foundations and legislators occupy analytical seats: they shape and judge the arrangement without collecting from it. Independent proprietary developers are authored as excluded rather than as a party: the reading's frame assigns them a moral category before they speak, which is precisely the consensus-provenance weakness the absent-voices answer records.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - funding software development once copying became free - is live, corroborated from outside the benefiting parties by open-source sustainability research; the arrangement is therefore not mandatrophy-resolved, and it is emphatically not inertial: it is actively maintained, profitable, and expanding, which rules the degraded/inertial type out. The classification work this reading performs cuts both ways. Against laundering: the genuine coordination function (funded development, support, standardization) must not be allowed to recode the arrangement as mere coordination cost - the categorical-illegitimacy premise exists to block exactly that move, and the rising theater series documents the justification drifting toward performance. Against over-extension: the universal-victim-set omega guards the categorical claim's weakest flank, the certified-firmware and medical-device cases where the freedom violation may be real but harmless; if a substantial benign class exists, the reading's own consistency requires narrowing the victim set rather than defending the slogan. The disappearance verdict (world_rearranges) with a live founding problem is the coherent pair: the arrangement does load-bearing work, and the dispute is over whether the work justifies the taking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates the freedom_imperative_reading of the software_source_status kernel; which of the four readings correctly characterizes the standing arrangement, and what would each sibling change structurally?',
    'Compile the three sibling stories over the identical standing arrangement and compare computed types, victim sets, and reading-indexed epsilon; divergence localizes the disagreement to victim-set membership (property-rights vs freedom), the valuation of restriction (pragmatic vs freedom), or the categorical-vs-contextual axis (hybrid vs freedom).',
    'If the property-rights reading computes as non-extractive over the same arrangement, the dispute lives in the axiom of inalienable user source-access rather than in measured structure; if the pragmatic reading converges on a coordination-dominant type while this reading computes pure extraction, the delta is precisely the categorical-illegitimacy premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the software-source-status kernel governs classification.').

omega_variable(
    universal_victim_set_edge_cases,
    'Does every proprietary deployment violate the freedoms in a way that constitutes injustice - including safety-certified avionics firmware, medical-device builds, and embedded controllers where modification rights interact with certification regimes?',
    'Survey proprietary deployments where no plausible freedom-harm materializes (certified flight-control firmware, pacemaker builds) and test whether the reading''s own harm account extends to them or admits a benign class.',
    'A substantial benign class would narrow the victim set below ''all proprietary software'', lowering effective extraction and pulling the computed type toward a hybrid coordination/extraction shape; a negligible class preserves the categorical claim and the wide victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_victim_set_edge_cases, empirical, 'Scope of the victim set across edge-case proprietary deployments.').

omega_variable(
    coercion_vs_market_preference,
    'How much of the standing arrangement''s persistence rests on legal-technical enforcement (EULA enforceability, anti-circumvention statute, DRM, app-store gatekeeping) versus voluntary market preference for supported products?',
    'Natural experiments from periods and jurisdictions with weak enforcement - pre-statute shrinkwrap skepticism, courts declining to enforce end-user restrictions: did proprietary share fall where enforcement lapsed?',
    'If persistence survives enforcement removal, the suppression measure is overstated and the arrangement reads closer to a contested coordination norm than to enforced extraction; if proprietary share tracks enforcement intensity, the pure-extraction characterization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_market_preference, empirical, 'Split of the arrangement''s persistence between coercion and market choice.').

omega_variable(
    saas_object_drift,
    'Has service-delivered software (no binary shipped, no source ever published) become a structurally different arrangement from licensed binaries, such that this story''s single epsilon conflates two regimes?',
    'Decomposition test: author separate stories for shipped-binary licensing and service-only delivery; if their epsilon, victim sets, and enforcement structures diverge beyond measurement noise, split the family.',
    'If they diverge, this story''s epsilon is a weighted blend and computed transitions date incorrectly; splitting yields one story where circumvention law bites (binaries) and one with still higher effective extraction (service delivery removes even the theoretical exit of patching a copy you hold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(saas_object_drift, conceptual, 'Whether service-delivered software is the same arrangement as binary licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t8, software_source_status__freedom_imperative_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(soft_tr_t8, observed).
narrative_ontology:measurement(soft_tr_t16, software_source_status__freedom_imperative_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(soft_tr_t16, observed).
narrative_ontology:measurement(soft_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(soft_tr_t24, observed).
narrative_ontology:measurement(soft_tr_t32, software_source_status__freedom_imperative_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement_basis(soft_tr_t32, observed).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t8, software_source_status__freedom_imperative_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement_basis(soft_be_t8, observed).
narrative_ontology:measurement(soft_be_t16, software_source_status__freedom_imperative_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(soft_be_t16, observed).
narrative_ontology:measurement(soft_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement_basis(soft_be_t24, observed).
narrative_ontology:measurement(soft_be_t32, software_source_status__freedom_imperative_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement_basis(soft_be_t32, observed).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t8, software_source_status__freedom_imperative_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(soft_su_t8, observed).
narrative_ontology:measurement(soft_su_t16, software_source_status__freedom_imperative_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(soft_su_t16, observed).
narrative_ontology:measurement(soft_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(soft_su_t24, observed).
narrative_ontology:measurement(soft_su_t32, software_source_status__freedom_imperative_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement_basis(soft_su_t32, observed).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the open-source-versus-proprietary debate' covers four structurally distinct claims sharing one referent - the standing proprietary-software arrangement - but differing in the lights under which it is read. This file instantiates the freedom-imperative reading (deontological: user freedoms as inalienable requirements, hence the widest victim set and the highest reading-indexed epsilon). Sibling files instantiate the pragmatic-development reading (freedom valued as development methodology), the property-rights reading (restriction as legitimate creator entitlement), and the utilitarian-hybrid reading (context-dependent welfare maximization). Each sibling carries its own epsilon, victim set, and claimed type; the family is linked so cross-reading comparison can localize where the disagreement lives - in victim-set membership, in the valuation of restriction, or on the categorical/contextual axis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
