% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open Source as Superior Development Methodology — Pragmatic Reading of the Source-Status Kernel
 *   domain: economic/technological/political
 *
 * SUMMARY:
 *   This story instantiates the pragmatic_development_reading of the
 *   software_source_status kernel: the claim that open source is a superior
 *   development methodology, with software freedom valuable as an input to
 *   quality rather than as an end in itself, and proprietary software
 *   therefore not inherently illegitimate. The standing arrangement under
 *   contest — the epsilon referent — is the industry-wide normative complex
 *   that treats open development as the methodologically superior default:
 *   the licensing conventions, contribution norms, hiring signals, conference
 *   economies, and corporate contribution strategies that reproduce that
 *   treatment. The referent is fixed across readings; what varies by reading
 *   is the evaluative stance. A freedom-imperative story of the same kernel
 *   would assess the same world from the premise that proprietary
 *   distribution is itself the injustice; a property-rights story would start
 *   from creator entitlement; a utilitarian-hybrid story would refuse a
 *   single ranking. Those are separate constraints with separate files,
 *   linked through network.affects_constraints. Structurally, the arrangement
 *   has a genuine coordination core — distributed peer review, shared
 *   non-differentiating infrastructure, bug detection pooled across a
 *   population no employer could assemble — and an asymmetric transfer
 *   layered through it: unpaid contributor and maintainer labor flowing
 *   toward commercial users who return little, normalized by
 *   permissive-licensing conventions that the methodology framing made
 *   respectable. Enforcement is social rather than legal: portfolio
 *   expectations in hiring, funder preferences, and reputational sanction
 *   inside engineering culture. KEY AGENTS (by structural relationship): -
 *   large_technology_firms: primary beneficiary and agenda-shaper
 *   (institutional/arbitrage) — collects the largest share of uncompensated
 *   value - open_source_advocacy_institutions: agenda_setter
 *   (institutional/constrained) — defines, interprets, and reproduces the
 *   norm - unpaid_critical_maintainers: primary target
 *   (moderate/identity_locked) — absorbs uncompensated obligation and
 *   incident blame - volunteer_hobbyist_contributors: secondary target with
 *   incidental returns (powerless/mobile) - career_minded_contributors:
 *   beneficiary (moderate/mobile) — converts contribution into reputation and
 *   mobility - open_core_startups: beneficiary (powerful/arbitrage) —
 *   monetizes the credibility the norm confers, with relicensing exits -
 *   commercial_downstream_users: payer with substantial offsetting gains
 *   (organized/constrained) - free_software_movement_activists: excluded
 *   voice (organized/constrained) — objectors absent from the rooms where the
 *   norm is applied - software_supply_chain_researchers: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.58).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.34).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open Source as Superior Development Methodology — Pragmatic Reading of the Source-Status Kernel").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "economic/technological/political").

domain_priors:requires_active_enforcement(software_source_status__pragmatic_development_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, large_technology_firms).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_core_startups).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, career_minded_contributors).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_advocacy_institutions).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, unpaid_critical_maintainers).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, volunteer_hobbyist_contributors).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, commercial_downstream_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, volunteer_hobbyist_contributors).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, commercial_downstream_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate global engineering organizations built on thousands of open-source components. Run developer-relations programs, fund a few flagship projects on their critical path, publish guidance that shapes how the industry talks about development models, and keep revenue-generating crown-jewel services closed. Return little where the component is not strategic. When license terms tighten they choose among funding maintainers, moving to forks, or pressing for rollback — all doors stay open to them.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, large_technology_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, large_technology_firms, agenda_setter).

% Foundations, license-approval bodies, and conference organizations that define what counts as open source, approve licenses, host shared infrastructure, and run the venues where the methodology story is retold. Funded by corporate sponsorship and donations. Their standing tracks the prominence of the idea they administer; they police the definitional boundary against both proprietary encroachment and ethical reframing, and they now preside over sustainability debates their framing helped create.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_advocacy_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Release a usable open core while reserving enterprise features under commercial terms, converting the credibility and distribution that open availability confers into pipeline, and drawing on contributor labor to cut development cost. When growth demands pricing power they have repeatedly relicensed the core toward stricter terms, trading community trust for revenue — an option always held in reserve.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_core_startups, beneficiary,
    powerful, generational, arbitrage, continental).

% Engineers who contribute to visible projects chiefly for portfolio, network, and hiring signal. They trade unpaid hours for reputation and mobility, an exchange that favors them early in their careers. They can stop contributing at any time without material loss, and most redirect to paid work as seniority rises.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, career_minded_contributors, beneficiary,
    moderate, biographical, mobile, global).

% Individuals or two-person teams maintaining infrastructure everything else depends on — package managers, compression libraries, certificate tooling — on nights and weekends beside day jobs. They absorb triage, security response, and public blame when things break, with little or no pay. Leaving means abandoning years of work, a reputation fused with the project, and users left exposed, so most continue well past sustainability. Several major supply-chain incidents began exactly at this seat.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, unpaid_critical_maintainers, payer,
    moderate, biographical, identity_locked, global).

% People who patch bugs, write documentation, and answer forum questions for enjoyment, learning, or scratching a personal itch. Their aggregate hours are enormous and almost entirely uncompensated. They owe nothing to anyone and can stop whenever they like; most drift out of any given project within a few years.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, volunteer_hobbyist_contributors, payer,
    powerless, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, volunteer_hobbyist_contributors, beneficiary).

% Companies of every size that ship products containing open-source components. They save vast licensing and development cost — the model's entire appeal from their side — while inheriting the operational risk of under-maintained dependencies: emergency patching, audit findings, supply-chain compromises. Switching a deeply embedded dependency is expensive, and most contribute back a small fraction of what they take.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, commercial_downstream_users, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, commercial_downstream_users, beneficiary).

% Advocates who hold that software freedom is an ethical demand, not a productivity technique. They were present when the open-source label was coined and explicitly lost the framing contest: corporate and media adoption followed the methodology pitch, not the rights pitch. They now sit outside the rooms where firms set licensing and contribution strategy, publishing critiques and enforcing copyleft licenses through legal defense funds.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, free_software_movement_activists, excluded,
    organized, generational, constrained, global).

% Academic and industry analysts who measure contribution asymmetries, dependency networks, and maintenance funding gaps — census-style valuations, SBOM analyses, postmortems of supply-chain compromises. They see the whole structure and operate nowhere inside it; their publications are the main external check on the methodology narrative.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_supply_chain_researchers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, large_technology_firms).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools software development across organizational boundaries: otherwise-unaffiliated engineers review one another's code, non-differentiating infrastructure is built once instead of per firm, and bug detection is distributed across a population no single employer could assemble. Standing within the pool substitutes for wages on the margin.
% TRANSFER_FUNCTION: Moves unpaid engineering labor and maintenance effort from individual contributors and maintainers toward every commercial user of the resulting code, including firms that return little or nothing; moves reputation and hiring capital toward visible contributors; moves the operational risk of under-maintained dependencies onto downstream users; and, through permissive-license normalization, moves appropriation rights from author communities toward whoever holds deployment scale.
% ABSENT_VOICES: Ethical-freedom advocates are structurally outside corporate licensing and contribution-strategy decisions; unpaid maintainers are rarely seated when the firms depending on them set roadmap and security policy; end users of infrastructure libraries are represented by no one — nobody speaks for the systems that quietly rest on a two-person project.
% DISAPPEARANCE_RATIONALE: If the norm complex vanished overnight — if open development stopped being treated as the superior methodology and openness reverted to a purely ethical niche — hiring signals, conference economies, and corporate contribution strategies would reorganize within quarters; firms would pay for infrastructure they currently harvest or rebuild it in-house; many volunteer-maintained projects would lose their animating justification and decay; and licensing choices would migrate to whichever framing replaced the methodology story.
% FOUNDING_PROBLEM: In the late 1990s, collaborative software production lacked a business-legible justification: the existing movement framed code-sharing as an ethical demand, which corporations read as hostility to property. The pragmatic reading was assembled to solve that framing problem — to show that distributed peer review outperforms centralized development ('given enough eyeballs, all bugs are shallow') and to rebrand openness as a quality-and-velocity strategy firms could adopt without moral conversion.
% FOUNDING_PROBLEM_CORROBORATION: The legibility half of the founding problem is corroborated as solved by outcomes no beneficiary can self-report: mass Fortune 500 adoption, government open-source mandates, and the industry-wide licensing shift. Whether the superiority claim remains live is disputed from outside the beneficiary set — census studies and supply-chain postmortems (log4j, xz) document the sustainability gap, maintainer surveys record burnout at scale, and the free-software wing argues the framing victory itself manufactured the under-compensation now visible. Independent research literature, not movement self-description, is the operative attestation.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type, tangled_rope, states the structural read: a real coordination function (the commons demonstrably produces critical infrastructure) joined to asymmetric transfer (volunteer and maintainer labor subsidizing commercial appropriation) held together by continuous social enforcement. The metrics describe operation as observed. Extractiveness 0.58: the transfer is large and uphill — census-scale studies place ecosystem value consumed by firms orders of magnitude above maintainer funding — but participation is consented, returns to contributors are real if uneven, and nothing is collected coercively. Suppression 0.34: enforcement runs on hiring gatekeeping, funder preference, and reputational pressure rather than law; alternatives (proprietary, copyleft, paid models, dual licensing) remain live, so the coercive floor is low. Theater 0.28: open-washing — token repos opened for press cycles, contribution counts gamed — is a real and growing share of activity, but the review-and-maintenance core performs. Accessibility collapse 0.38: understanding the arrangement does not collapse alternatives; a firm can develop closed, a project can go copyleft, a maintainer can charge. Resistance 0.45: the free-software wing rejects the framing outright, relicensing defections punctuate it, and the sustainability movement contests it from inside. The temporal series run on one shared grid (t=0,4,9,13,18,22,27) so every metric is authored at every point; the scalars correspond to the interval-end state. The series are arc-shaped rather than cyclical: extractiveness and theater climb through the cloud-appropriation era, peak near the log4j disclosure window, and ease modestly as sustainability funding arrives — while suppression requirement rises through the portfolio-hiring hardening of the 2010s and declines afterward as blame-shifting post-log4j and xz weakened the norm's grip on volunteering. Coalition note: the powerless hobbyist seat is poorly positioned for coalition (replaceable, mobile, unorganized), but maintainers sit on scarce, irreplaceable assets and hold latent collective leverage that surfaces episodically in slowdown threats and strike letters.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently and the divergence is the finding. From the large-firm seat the arrangement presents as a commons that works: quality software at zero license cost, goodwill, and a talent pipeline — a coordination win the firms helped build. From the unpaid-maintainer seat the same structure presents as an uncompensated obligation backed by guilt: the world runs on their nights and weekends, and the methodology story that celebrated their contribution supplied none of the compensation. From the advocacy-institution seat the norm is a life's legitimacy project, so drift threatening the framing threatens identity, not just revenue. Same-level lateral divergence is sharpest between two contributor seats at identical nominal standing: career-minded contributors convert hours into reputation and mobility (net gainers), while hobbyists donate comparable hours for enjoyment and learning and collect little positional return — same activity, opposite structural direction. Downstream users experience both faces: enormous savings and inherited operational risk. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Beneficiaries (firms, startups, careerists, advocacy institutions) derive low d — subsidized by the arrangement. Targets derive high d: maintainers sit near the full-target end because their victim status combines with identity_locked exit, the amplifying case; hobbyists are pulled slightly back from the extreme by mobile exit — they can and do walk away. The one deliberate structural nuance is commercial_downstream_users, who appear in the victims array (they bear supply-chain risk, emergency-patching burden, and switching costs) while being among the largest gainers; because they are declared in BOTH arrays, the structural derivation lands them near symmetric without a directionality override — authoring an override keyed on their power atom would have collided with the free-software activists sharing that atom, so the dual declaration is the correct instrument here. Advocacy institutions are beneficiaries whose gain is mediated through administration: their d is low but their interest is in the norm's persistence rather than in surplus collection, which the agenda_setter role records separately from the beneficiary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making collaborative development legible and acceptable to business — was substantially solved within a decade of the framing battle; the arrangement then accreted second-life functions (industrial procurement logic, talent signaling, security-posture performance) that keep it live regardless. Hence founding_problem_status is contested rather than dead: the legibility problem is closed, the superiority claim is still litigated, and the sustainability gap the arrangement itself produced is a new live problem it did not exist to solve. The mismatch consumer reads status-times-verdict: contested-status with a world_rearranges verdict raises no zombie flag yet. The classification prevents two symmetrical errors. Reading the arrangement as pure coordination (a rope) erases the maintainer seat — the burnout, the xz-shaped fragility, the uncompensated liability — and launders the transfer as community spirit. Reading it as pure extraction (a snare) erases the demonstrable fact that the commons produces and sustains infrastructure no consortium has replicated, and that many participants are net gainers. Tangled rope holds both truths in one structure: coordination function plus asymmetric extraction plus active enforcement. Watch item: theater_ratio trending upward from open-washing is the visible Goodhart surface — if performative openness continues substituting for maintained openness, the arrangement drifts toward the degraded end without anyone deciding anything.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint instantiates the pragmatic_development_reading of the software_source_status kernel — what structurally changes if the standing arrangement were instantiated under a sibling reading instead?',
    'Comparative read of the sibling stories: software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading, software_source_status__utilitarian_hybrid_reading. Each relocates the epsilon referent: the freedom reading makes proprietary distribution itself the violation (expanding the victim set to all proprietary-software users and raising assessed extraction of the existing arrangement); the property-rights reading recodes copyleft enforcement as the harm and license violators as wrongdoers; the utilitarian hybrid fragments assessment by context, denying any single stable value.',
    'The tangled_rope verdict authored here is reading-indexed. Under the freedom-imperative instantiation the same arrangement computes with a materially higher victim count and higher extraction; under the property-rights instantiation the extraction direction partially inverts. Cross-reading comparison is valid only through the linked family, not within this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer-frame delta: which kernel reading is instantiated and how siblings would restructure the referent.').

omega_variable(
    superiority_domain_scope,
    'Is open development actually methodologically superior across classes of software, or superior mainly for infrastructure, tools, and standards-adjacent code while neutral or negative for UI-heavy, safety-critical, and design-coherent products?',
    'Controlled comparisons of quality, defect density, and velocity outcomes conditioned on development model and software category, holding team size and domain constant; replication across cohorts.',
    'If superiority is domain-limited, the universal-norm version of the arrangement applies where it does not pay, and part of the measured extraction is misallocated application rather than coordination cost — strengthening the asymmetric-component read. If robustly superior, more of the transfer is the price of the quality itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superiority_domain_scope, empirical, 'Whether the foundational superiority claim generalizes across software categories.').

omega_variable(
    reciprocity_flow_share,
    'What fraction of commercially exploited open-source value receives commensurate reciprocal investment from the exploiting firms?',
    'Contribution-versus-value-flow audits at ecosystem scale (census-style dependency valuation matched against sponsor and maintainer funding data), stratified by firm size.',
    'A very low reciprocity share establishes the transfer function as uncompensated subsidy flowing uphill; a high share would recast the arrangement as a functioning exchange and pull the classification toward the coordination pole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_flow_share, empirical, 'Magnitude of the unpaid-labor subsidy embedded in the arrangement.').

omega_variable(
    maintainer_identity_lock_depth,
    'How much of critical-maintainer persistence is voluntary attachment versus identity fusion with the project, such that exit is psychologically unavailable even when materially possible?',
    'Post-abandonment trajectory studies: compare health and handover outcomes of projects whose maintainers exited cleanly versus those who attempted exit and returned; interview-based measurement of guilt and self-concept coupling.',
    'If identity lock is deep, the arrangement holds its most essential payers through self-concept rather than incentive, and effective suppression is understated by any purely structural measure; breaking the frame would trigger abandonment waves rather than negotiated handovers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_identity_lock_depth, empirical, 'Depth of the identity mechanism binding unpaid maintainers to unsustainable obligations.').

omega_variable(
    selection_vs_installation,
    'Does the pragmatic framing dominate because open development genuinely outcompetes closed development (selection), or because it was deliberately engineered into business legibility in 1997-1998 and subsequently locked in through hiring norms and procurement habits (installation)?',
    'Natural experiments where the norm''s enforcement slackened (post-incident blame cycles, relicensing shocks): observe whether open development retains share on merit signals alone or requires continuous norm maintenance.',
    'If installation dominates, the arrangement''s persistence owes more to its enforcement machinery than to its payoff, raising the inertial component of the classification; if selection dominates, the norm is earning its place and the extraction is riding on a genuinely fit structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selection_vs_installation, conceptual, 'Fitness versus path-dependence accounts of the norm''s dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t4, software_source_status__pragmatic_development_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(soft_tr_t9, software_source_status__pragmatic_development_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement(soft_tr_t13, software_source_status__pragmatic_development_reading, theater_ratio, 13, 0.26).
narrative_ontology:measurement(soft_tr_t18, software_source_status__pragmatic_development_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(soft_tr_t22, software_source_status__pragmatic_development_reading, theater_ratio, 22, 0.29).
narrative_ontology:measurement(soft_tr_t27, software_source_status__pragmatic_development_reading, theater_ratio, 27, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(soft_be_t4, software_source_status__pragmatic_development_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(soft_be_t9, software_source_status__pragmatic_development_reading, base_extractiveness, 9, 0.53).
narrative_ontology:measurement(soft_be_t13, software_source_status__pragmatic_development_reading, base_extractiveness, 13, 0.58).
narrative_ontology:measurement(soft_be_t18, software_source_status__pragmatic_development_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(soft_be_t22, software_source_status__pragmatic_development_reading, base_extractiveness, 22, 0.6).
narrative_ontology:measurement(soft_be_t27, software_source_status__pragmatic_development_reading, base_extractiveness, 27, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__pragmatic_development_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(soft_su_t4, software_source_status__pragmatic_development_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(soft_su_t9, software_source_status__pragmatic_development_reading, suppression_requirement, 9, 0.38).
narrative_ontology:measurement(soft_su_t13, software_source_status__pragmatic_development_reading, suppression_requirement, 13, 0.43).
narrative_ontology:measurement(soft_su_t18, software_source_status__pragmatic_development_reading, suppression_requirement, 18, 0.46).
narrative_ontology:measurement(soft_su_t22, software_source_status__pragmatic_development_reading, suppression_requirement, 22, 0.39).
narrative_ontology:measurement(soft_su_t27, software_source_status__pragmatic_development_reading, suppression_requirement, 27, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'open source'. The label conflates four structurally distinct claims with different epsilon referents, victim sets, and empirical statuses: property-rights (historically upstream — the legal substrate all others presuppose), freedom-imperative (upstream of this file — the movement this reading split from in the 1997-98 reframing), pragmatic-development (this file), and utilitarian-hybrid (downstream synthesizer that moderates this file's universality claim using its empirical outputs). Each member links the others via network.affects_constraints; contamination propagates along the family because the readings cite one another — methodology claims are recruited as evidence in welfare arguments, and rights arguments are recruited against methodology framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
