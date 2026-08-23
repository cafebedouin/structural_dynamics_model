% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__property_rights_reading, []).

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
 *   constraint_id: software_control_legitimacy__property_rights_reading
 *   human_readable: Software Control as Creator Property Right (Property-Rights Reading)
 *   domain: economic/technological/political
 *
 * SUMMARY:
 *   The standing arrangement under contest is the proprietary-control regime:
 *   copyright asserted over source and binaries, end-user license agreements,
 *   activation servers and DRM, subscription conversion of formerly owned
 *   software, and treaty-aligned international enforcement. This story
 *   instantiates the property-rights reading of the
 *   software_control_legitimacy kernel, in which such restriction is the
 *   rightful exercise of creator authority over their work. The epsilon
 *   referent is that standing regime assessed by this reading's own lights:
 *   the reading credits restriction as mostly legitimate investment
 *   protection while conceding extractive excess where controls outrun
 *   recovery — kill-switch deprecation of functioning products, punitive
 *   technical measures aimed at paying customers, rental conversion of
 *   paid-off licenses. Sibling readings (freedom-imperative,
 *   pragmatic-openness, commons) are separate constraint stories with their
 *   own epsilon and victim sets, linked through network.affects_constraints;
 *   nothing about them is averaged into this file. Claim and metrics are
 *   independent authored facts: the tangled_rope claim comes from structural
 *   analysis, the metric values from descriptive judgment.
 *
 * KEY AGENTS:
 *   - - proprietary_software_vendors: agenda setter and primary beneficiary (institutional/arbitrage) — drafts and enforces the terms, receives the transfers
 *   - - software_capital_investors: secondary beneficiary (powerful/mobile) — collects returns without operating the machinery
 *   - - end_users_of_proprietary_software: primary target (powerless/constrained) — bears restriction and fee burdens under unilateral terms
 *   - - foss_developers_and_advocates: organized target with copyleft leverage (organized/identity_locked) — fenced out of proprietary code, chilled in reverse engineering, identity-fused to the freedom project
 *   - - interoperability_and_repair_developers: target (moderate/constrained) — blocked from the access their products require
 *   - - security_researchers: chilled auditor (moderate/constrained) — investigates at legal risk, absent from term-setting
 *   - - competition_and_standards_regulators: analytical observer (institutional/analytical) — reviews the structure and can reshape enforcement conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, 0.52).
domain_priors:suppression_score(software_control_legitimacy__property_rights_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__property_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__property_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__property_rights_reading, "Software Control as Creator Property Right (Property-Rights Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__property_rights_reading, "economic/technological/political").

domain_priors:requires_active_enforcement(software_control_legitimacy__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__property_rights_reading, 'bb6c1db2-8273-4aca-ac28-0a9bd682d380').
narrative_ontology:cs_kernel_codification('bb6c1db2-8273-4aca-ac28-0a9bd682d380', distributed).
narrative_ontology:cs_authority_grounding('bb6c1db2-8273-4aca-ac28-0a9bd682d380', lineage).
narrative_ontology:cs_interpretation_layer_present('bb6c1db2-8273-4aca-ac28-0a9bd682d380').
narrative_ontology:cs_reading_relation('bb6c1db2-8273-4aca-ac28-0a9bd682d380', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('bb6c1db2-8273-4aca-ac28-0a9bd682d380', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb6c1db2-8273-4aca-ac28-0a9bd682d380', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('bb6c1db2-8273-4aca-ac28-0a9bd682d380', foundational, creator_restrictive_authority_is_legitimate).
narrative_ontology:cs_axiom_status(creator_restrictive_authority_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('bb6c1db2-8273-4aca-ac28-0a9bd682d380', creator_restrictive_authority_is_legitimate, deontological).
narrative_ontology:cs_axiom('bb6c1db2-8273-4aca-ac28-0a9bd682d380', secondary, exclusive_control_promotes_progress).
narrative_ontology:cs_axiom_status(exclusive_control_promotes_progress, holdable).
narrative_ontology:cs_axiom_grounding('bb6c1db2-8273-4aca-ac28-0a9bd682d380', exclusive_control_promotes_progress, empirically_contingent).
narrative_ontology:cs_reference_frame('bb6c1db2-8273-4aca-ac28-0a9bd682d380', creator_exclusive_control_norm).
narrative_ontology:cs_drift_state('bb6c1db2-8273-4aca-ac28-0a9bd682d380', contemporary_open_source_mainstream_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('bb6c1db2-8273-4aca-ac28-0a9bd682d380', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__property_rights_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__property_rights_reading, software_capital_investors).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, end_users_of_proprietary_software).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, foss_developers_and_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, interoperability_and_repair_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__property_rights_reading, security_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes software under restrictive licenses: end-user license agreements, activation and DRM systems, subscription terms. Drafts the terms unilaterally, enforces them through technical measures and litigation, and lobbies internationally for stronger enforcement. Collects license fees, subscriptions, and maintenance revenue directly; retains sole authority over modification, redistribution, and interoperability of its products. Can restructure offerings (for example converting perpetual purchases into rentals) faster than regulators respond.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, proprietary_software_vendors, beneficiary).

% Holds equity and debt in software vendors. Returns depend on the pricing power that enforceable exclusivity protects. Diversified portfolios allow rotation out of segments whose protections erode; exposure is financial rather than operational, so the day-to-day enforcement burden lands elsewhere.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, software_capital_investors, beneficiary,
    powerful, biographical, mobile, global).

% Runs proprietary software at work and home under click-through terms they did not negotiate. May not modify, redistribute, or inspect what they run; pays recurring fees; faces switching costs in file formats, workflows, and skills that bound exit even where open alternatives nominally exist.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, end_users_of_proprietary_software, payer,
    powerless, biographical, constrained, global).

% Develops and maintains free and open-source software and campaigns for user control of computing. Legally barred from incorporating proprietary code into their work; reverse engineering for interoperability is chilled by anti-circumvention law. Their identity is bound up with the freedom commitment — abandoning the stance would mean leaving the community and project that constitute their working lives. At the same time, copyleft licenses borrow the regime's own exclusivity machinery (copyright enforcement) to keep derivative works open.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, foss_developers_and_advocates, payer,
    organized, generational, identity_locked, global).

% Builds compatible, repairable, or integrative products around proprietary platforms. Anti-circumvention provisions and license restrictions bar the reverse engineering their work requires. Right-to-repair disputes concentrate in particular jurisdictions, where legislation is the main lever available to them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, interoperability_and_repair_developers, payer,
    moderate, biographical, constrained, regional).

% Audits closed-source binaries for vulnerabilities. Anti-circumvention law and vendor disclosure policies constrain how they investigate and publish; several operate under legal uncertainty or personal risk. They have no seat in the license-term conversations that determine what they may examine.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, security_researchers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__property_rights_reading, security_researchers, excluded).

% Competition authorities, legislators, and standards bodies reviewing tying, lock-in, repairability, and interoperability. They see the structure through hearings, market studies, and filings, and can change enforcement conditions through mandate or remedy, though they neither run nor fund software themselves.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__property_rights_reading, competition_and_standards_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__property_rights_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(software_control_legitimacy__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the investment-appropriation problem in software production: enforceable exclusivity converts freely duplicable artifacts into recoverable assets, letting firms fund multi-year engineering efforts, coordinate customer expectations about permissible use, and sustain specialization in testing, documentation, and support.
% TRANSFER_FUNCTION: Moves money (license fees, subscriptions, maintenance payments) and control rights over use, modification, and distribution from end users and secondary developers toward vendors and their investors; moves decision authority over deployed computing from the people running it to the entities owning it.
% ABSENT_VOICES: End users never negotiated the terms they click through; security researchers and repair technicians are structurally outside license-term design; future developers who might build on today's code have no seat at all. All of them sit outside a take-it-or-leave-it process in which the drafting party is also the enforcement party.
% DISAPPEARANCE_RATIONALE: License-funded business models collapse immediately; a period of unrestricted copying follows; then procurement, update channels, and support structures reorganize around whatever replaces exclusivity as the funding mechanism — service contracts, patronage, or commons production. Enterprise software estates, app-store economics, and vendor security-response obligations would all need new foundations.
% FOUNDING_PROBLEM: When software decoupled from hardware sales (IBM's 1969 unbundling; the microcomputer boom of the mid-1970s), producers faced the appropriation problem: copies cost nothing to duplicate, so how could development investment ever be recovered? Exclusive control over use and redistribution was constructed as the answer.
% FOUNDING_PROBLEM_CORROBORATION: The historical reality of the founding problem is corroborated from outside the beneficiary set: industry economic histories document the pre-unbundling give-away norm and the funding crisis that followed; contemporaneous trade press recorded it; even FOSS advocates attest the original funding problem was real. Corroboration of its continued liveness comes almost exclusively from vendors themselves, while service-model and commons-production evidence from outside the beneficiary set supports the shifted-necessity reading — hence 'contested'.
narrative_ontology:disappearance_verdict(software_control_legitimacy__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__property_rights_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.52 is moderate by the reading's own accounting: restriction is credited as legitimate investment protection, with conceded excess where controls outrun recovery. Suppression 0.62 is authored as the raw structural property it is — unscaled by power or scope — reflecting the enforcement stack (anti-circumvention statutes, activation infrastructure, audit programs, treaty backing) required to hold restriction in place against real demand for modification and sharing; it exceeds theater because persistence depends on active defense, not participant preference. Theater 0.30: unread click-throughs, anti-piracy notices, and compliance audits that double as revenue harvesting grow steadily, but funding and support functions remain real. Accessibility_collapse 0.45: alternatives do not vanish — the open ecosystem is legal and visible — but network effects, format lock-in, and procurement inertia collapse alternatives for many concrete deployments. Resistance 0.60: a forty-year counter-current of copyleft licensing, public-interest litigation, right-to-repair legislation, and open-source adoption in critical infrastructure meets the regime continuously. All three temporal series share one grid (0,8,16,24,32,40): base_extractiveness climbs with rental conversion and telemetry-assisted compliance, suppression_requirement tracks the enforcement ratchet that followed anti-circumvention legislation mid-interval, theater_ratio rises as compliance activity grows faster than the functions it cites.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes different types per seat from the same structure. From the vendor seat the regime is commerce it built: exclusivity is what makes multi-year engineering fundable, and enforcement is contract administration. From the end-user and interoperability-developer seats the same clauses read as enclosure: paying for the privilege of not controlling one's own machines. The FOSS seat diverges further — the regime both fences it out and arms it, since copyleft runs on the exclusivity this reading sanctifies, so its experienced constraint differs from the end user's even at comparable nominal restriction. Investors, positioned upstream and mobile, see ordinary asset protection. None of these views is adjudicated by the authored claim; they fall out of the power, exit, and directionality data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: vendors (agenda_setter plus collector) sit near the beneficiary pole; investors, mobile and financially exposed, sit slightly higher but still subsidized. Victim declarations drive high d: end users (constrained, no negotiating seat) near full target; interoperability and repair developers similar, with marginally better exit through jurisdictional variation in repair law. One override is declared: the derivation from victim-plus-identity_locked would place foss_developers_and_advocates at essentially full target, but copyleft parasitism means the same exclusivity instrument they suffer under also enforces their licenses — a genuine secondary benefit the structural derivation cannot see. The override sets d to 0.82: firmly target-side, damped below the ceiling. Regulators contribute little directional weight as observers. Suppression stays unscaled; only extractiveness is scaled by directionality and scope, and the regime's global scope modestly amplifies effective chi on target seats because verifying compliance at planetary scale is harder.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions of error. Reading the regime as a snare would erase the genuine coordination achievement — exclusive recoverability funded decades of large-scale engineering, and reform proposals that ignore the funding problem will mispredict vendor behavior. Accepting the reading's self-description (restriction as pure legitimate right, hence rope-like) would hide the asymmetric burden: those who bear the restriction never negotiated it, and enforcement intensity grows with the value enclosed rather than with investment protected. The genealogy interview keeps the mandate question open: the founding problem was real and broadly corroborated, but its continued necessity is disputed by demonstrated service-funded and commons-funded production — hence 'contested'. The rising theater_ratio is the early-warning line: if enforcement becomes predominantly ritual while funding migrates to rental terms needing no copyright argument at all, the arrangement drifts toward inertial persistence without any party deciding it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the software_control_legitimacy kernel governs a given evaluation, and how would sibling readings change this constraint''s structure?',
    'Consumers join evaluations on reading_id before comparing classifications across the family; each sibling file carries its own epsilon, victim sets, and axioms.',
    'Under freedom_imperative_reading the vendors become illegitimate appropriators and epsilon rises sharply; under pragmatic_openness_reading restriction prices as a methodology choice near the coordination floor; under commons_reading the beneficiary structure reorganizes around shared-governance seats and the victim set thins.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'This story is one committer reading of a contested kernel; classification is not invariant across readings.').

omega_variable(
    incentive_axiom_empirical_status,
    'Does exclusive control actually promote software progress, as the reading''s empirically contingent axiom claims?',
    'Comparative output and quality studies across open and proprietary development; natural experiments such as the 1969 unbundling and subsequent open-sourcing events.',
    'If the incentive claim fails empirically, the reading loses its instrumental leg; the residual deontological core then justifies restriction less, and computed classification trends toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_axiom_empirical_status, empirical, 'Empirical contingency of the reading''s incentive axiom.').

omega_variable(
    copyleft_parasitism_direction,
    'Are FOSS developers net victims of the property regime, or dual-positioned agents who parasitize it, given that copyleft enforcement runs on copyright exclusivity?',
    'Litigation record of copyleft enforcement plus counterfactual analysis of license efficacy under weakened copyright.',
    'A net-benefit finding weakens this seat''s victim declaration, narrowing measured asymmetry and pressing classification toward rope; confirmation as net victim widens asymmetry toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_parasitism_direction, empirical, 'Sign ambiguity in the FOSS seat''s structural position.').

omega_variable(
    coercion_vs_switch_cost_suppression,
    'How much of the measured suppression is state-backed enforcement rather than voluntary lock-in?',
    'Separate enforcement-action counts (anti-circumvention suits, audits, takedowns) from churn studies of users who leave despite the legality of alternatives.',
    'If suppression is mostly switching friction, effective suppression drops and the arrangement reads closer to rope; if enforcement-driven, the authored value stands and enforcement capacity becomes the variable to track.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_switch_cost_suppression, empirical, 'Composition of the suppression scalar: enforcement versus switching cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scpr_tr_t0, software_control_legitimacy__property_rights_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(scpr_tr_t8, software_control_legitimacy__property_rights_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(scpr_tr_t16, software_control_legitimacy__property_rights_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(scpr_tr_t24, software_control_legitimacy__property_rights_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(scpr_tr_t32, software_control_legitimacy__property_rights_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(scpr_tr_t40, software_control_legitimacy__property_rights_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(scpr_be_t0, software_control_legitimacy__property_rights_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(scpr_be_t8, software_control_legitimacy__property_rights_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(scpr_be_t16, software_control_legitimacy__property_rights_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(scpr_be_t24, software_control_legitimacy__property_rights_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(scpr_be_t32, software_control_legitimacy__property_rights_reading, base_extractiveness, 32, 0.49).
narrative_ontology:measurement(scpr_be_t40, software_control_legitimacy__property_rights_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(scpr_su_t0, software_control_legitimacy__property_rights_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(scpr_su_t8, software_control_legitimacy__property_rights_reading, suppression_requirement, 8, 0.47).
narrative_ontology:measurement(scpr_su_t16, software_control_legitimacy__property_rights_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(scpr_su_t24, software_control_legitimacy__property_rights_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(scpr_su_t32, software_control_legitimacy__property_rights_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(scpr_su_t40, software_control_legitimacy__property_rights_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__property_rights_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__property_rights_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% 'Software control' decomposes into four structurally distinct constraints — one per reading of the software_control_legitimacy kernel — because the colloquial label conflates a normative question that different committers answer differently, yielding different epsilon, victim sets, and enforcement stories. This file carries the property-rights instantiation. Family links run through network.affects_constraints in all four files. In empirical-dominance terms this reading is upstream: the enacted regime conditions the operating environment of the commons and pragmatic readings, while the freedom-imperative reading stands in mutual foreclosure with this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__property_rights_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
