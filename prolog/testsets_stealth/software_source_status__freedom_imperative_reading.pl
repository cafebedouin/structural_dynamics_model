% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Software Freedom as Inalienable Ethical Requirement (Freedom-Imperative Reading)
 *   domain: software engineering / political economy of technology / intellectual property
 *
 * SUMMARY:
 *   This story instantiates the freedom_imperative_reading of the
 *   software_source_status kernel: the doctrine, originating with the GNU
 *   project (1983) and the Free Software Foundation, that every user has
 *   inalienable rights to run, study, modify, and redistribute software, and
 *   that proprietary licensing — any license restricting those acts — is an
 *   injustice as such, not a regrettable tradeoff. The doctrine operates
 *   through copyleft instruments (the GPL family), through the FSF's
 *   stewardship of the free software definition, through compliance
 *   enforcement against distributors, and through a community normative order
 *   in which reliance on proprietary software carries censure. Per the
 *   epsilon-referent rule for kernel readings, epsilon is authored over the
 *   standing arrangement under contest — the proprietary-licensing regime —
 *   assessed by this reading's own lights: by them the regime is profoundly
 *   extractive (0.85), denying users control over their own computing while
 *   charging repeatedly for the denial. The claim/metrics split is deliberate
 *   and independent: claimed_type is mountain because the reading presents
 *   the doctrine in natural-law form — a fundamental ethical requirement,
 *   categorical scope, inalienable rights — while the metrics describe an
 *   authored, contested, actively enforced movement doctrine with
 *   identifiable beneficiaries; the natural-law-versus-constructed ambiguity
 *   is carried by omega natural_law_vs_constructed_doctrine and the
 *   false-summit machinery should evaluate it. Sibling readings (pragmatic,
 *   property-rights, utilitarian-hybrid) are separate constraint files linked
 *   through network.affects_constraints; each authors its own epsilon over
 *   the shared referent.
 *
 * KEY AGENTS:
 *   - free_software_foundation: agenda-setter and doctrinal steward (institutional/identity_locked) — defines the freedoms, stewards the GPL, enforces compliance; enforcement accrues precedent, authority, and funding to it
 *   - software_users_collectively: primary beneficiary (moderate/mobile) — holds the asserted inalienable rights, rarely invokes them
 *   - free_software_developers: beneficiary-payer (organized/identity_locked) — builds the commons, bears maintenance burden and doctrinal policing of licensing choices
 *   - proprietary_software_vendors: primary target of condemnation (powerful/arbitrage) — business model declared unjust; escapes the enforcement machinery via service delivery
 *   - gpl_distributors: enforcement target (organized/constrained) — bears source-disclosure obligations and litigation risk through code they did not license per-component
 *   - proprietary_software_users: condemned-in-absentia majority (powerless/mobile) — the object of the doctrine's judgment, absent from the discourse that renders it
 *   - software_policy_scholars: analytical observer — maps the doctrine's structure, enforcement record, and drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.85).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.6).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, mountain).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Software Freedom as Inalienable Ethical Requirement (Freedom-Imperative Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software engineering / political economy of technology / intellectual property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).
domain_priors:emerges_naturally(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '8a8b622f-ecda-4d23-a53c-60e27178c977').
narrative_ontology:cs_kernel_codification('8a8b622f-ecda-4d23-a53c-60e27178c977', fixed_text).
narrative_ontology:cs_authority_grounding('8a8b622f-ecda-4d23-a53c-60e27178c977', lineage).
narrative_ontology:cs_interpretation_layer_present('8a8b622f-ecda-4d23-a53c-60e27178c977').
narrative_ontology:cs_reading_relation('8a8b622f-ecda-4d23-a53c-60e27178c977', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a8b622f-ecda-4d23-a53c-60e27178c977', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('8a8b622f-ecda-4d23-a53c-60e27178c977', software_source_status__utilitarian_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('8a8b622f-ecda-4d23-a53c-60e27178c977', foundational, proprietary_software_categorically_injustice).
narrative_ontology:cs_axiom_status(proprietary_software_categorically_injustice, holdable).
narrative_ontology:cs_axiom_grounding('8a8b622f-ecda-4d23-a53c-60e27178c977', proprietary_software_categorically_injustice, deontological).
narrative_ontology:cs_axiom('8a8b622f-ecda-4d23-a53c-60e27178c977', foundational, user_computing_freedoms_inalienable).
narrative_ontology:cs_axiom_status(user_computing_freedoms_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('8a8b622f-ecda-4d23-a53c-60e27178c977', user_computing_freedoms_inalienable, deontological).
narrative_ontology:cs_axiom('8a8b622f-ecda-4d23-a53c-60e27178c977', secondary, copyleft_reciprocity_secures_freedoms).
narrative_ontology:cs_axiom_status(copyleft_reciprocity_secures_freedoms, holdable).
narrative_ontology:cs_axiom_grounding('8a8b622f-ecda-4d23-a53c-60e27178c977', copyleft_reciprocity_secures_freedoms, instrumental).
narrative_ontology:cs_reference_frame('8a8b622f-ecda-4d23-a53c-60e27178c977', four_freedoms_moral_baseline).
narrative_ontology:cs_drift_state('8a8b622f-ecda-4d23-a53c-60e27178c977', contemporary_saas_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a8b622f-ecda-4d23-a53c-60e27178c977', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, software_users_collectively).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_foundation).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, gpl_distributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, free_software_developers).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, four_freedoms_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, user_sovereignty_over_computing).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, copyleft_reciprocity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what counts as free software, stewards the GPL and the free software definition, runs compliance enforcement and advocacy campaigns, and accepts donations to fund that work. Its institutional identity has fused with the doctrine it administers: revising the categorical framing would dissolve the organization's reason for existence, so doctrinal rigidity is structurally favored. Enforcement activity accrues legal precedent, public authority, and funding to it.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_foundation, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, free_software_foundation, beneficiary).

% Hold the rights the doctrine asserts: wherever a copyleft license governs, they may run, study, modify, and redistribute the software, and the license machinery stands ready to defend those acts. In practice most invoke these rights rarely or never; they move freely between free and proprietary software and bear little material pressure from the doctrine either way.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_users_collectively, beneficiary,
    moderate, biographical, mobile, global).

% Build and maintain the shared code base the doctrine protects, gaining collaboration, reuse, and reputational standing inside the movement. They pay in uncompensated maintenance burden, in licensing choices policed by doctrinal criteria (a permissive license draws criticism as insufficiently reciprocal), and in career paths whose self-concept is constituted by movement membership — leaving the framework means repudiating their own body of work.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_developers, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__freedom_imperative_reading, free_software_developers, payer).

% Sell software under restrictive licenses that the doctrine declares unjust as such. They bear compliance costs where their products incorporate GPL code, face reputational campaigns, and have responded with structural escapes: delivering software as network services (which triggers no copyleft distribution obligation), dual licensing, and open-core models. Their lobbying and litigation resources make them the doctrine's most capable adversaries.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_vendors, payer,
    powerful, generational, arbitrage, global).

% Manufacturers and enterprises that ship products incorporating GPL-covered components — Linux-based devices, embedded systems, enterprise stacks. They did not choose copyleft terms per component; the ecosystem's shared code base forces the terms on them. They bear source-disclosure and license-compatibility obligations, with litigation as the penalty for failure, and their exit would mean rewriting products to avoid the dominant shared code base.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, gpl_distributors, payer,
    organized, biographical, constrained, global).

% The large majority of the world's computer users, whose daily practice is proprietary operating systems, games, and applications. The doctrine judges their computing practice unjust, but they are not participants in the discourse that renders the judgment; they encounter it only as external censure or as licensing friction. They can ignore the doctrine at essentially no material cost.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_users, excluded,
    powerless, biographical, mobile, global).

% Academic lawyers, economists, and historians who study the free software movement's legal architecture, its enforcement record, and its divergence from industry practice. They take no side in the doctrine's claims; they map its structure, its precedents, and its drift.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_policy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, free_software_foundation).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the software-commons enclosure problem: a shared body of code that anyone may run, study, modify, and redistribute, held against proprietary appropriation by reciprocal licensing. Coordinates thousands of developers and users around one legal instrument (the GPL family) and one definitional authority (the free software definition) so that derived work stays in the commons.
% TRANSFER_FUNCTION: Moves source-code disclosure and license compliance from distributors of GPL-derived software into the commons; moves moral authority, enforcement precedent, and funding to the doctrine's steward institutions; moves the categorical judgment 'injustice' onto proprietary licensing and onto the practice of everyone who relies on it.
% ABSENT_VOICES: The world's proprietary-software user majority — billions whose actual computing practice the doctrine judges — have no seat in the discourse that renders the judgment; they are condemned in absentia. Proprietary vendors engage only adversarially (litigation, counter-campaigns), never as participants. Developers in domains where proprietary models are structurally embedded (safety-certified stacks, console platforms, licensed medical software) are largely absent from the doctrine's deliberations about their practice.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the GPL's interpretive stewardship would lapse, copyleft reciprocity would drift toward permissive licensing as compliance friction accumulated, and the commons would slowly enclose from the edges — the Linux/GNU world would not vanish, but the legal architecture keeping derived work shareable would erode within a generation, and the moral vocabulary restraining pure-appropriation licensing would lose its institutional voice.
% FOUNDING_PROBLEM: Proprietary vendors controlled users' computing by withholding source code — in the doctrine's origin narrative, a printer driver at the MIT AI Lab whose source was denied, generalizing to a software industry built on NDAs and copyright criminalizing the sharing that had been the norm among programmers. The doctrine was built to guarantee users' freedom to run, study, modify, and share software against that enclosure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: right-to-repair statutes and proceedings (automotive, agricultural, medical devices), consumer-protection actions against software lock-in and obsolescence, security research documenting telemetry and remote kill-switches in non-free software, and legislative debates such as the EU Cyber Resilience Act all attest the user-control problem the doctrine names — none of these sources shares the movement's beneficiaries. The specific origin anecdote rests on the founder's own account and is weakly corroborated; the structural problem it names is not.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, ExtMetricName, E),
    domain_priors:suppression_score(software_source_status__freedom_imperative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(software_source_status__freedom_imperative_reading),
    narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.85: the referent is the proprietary arrangement as this reading assesses it — vendor lock-in, DRM, telemetry, repair obstruction, and the SaaS delivery model that subordinates users while triggering no license at all; by the reading's own lights the extraction of computing control approaches total. Suppression 0.60: the doctrine is enforced by GPL compliance litigation, license-compatibility policing, and community censure — real coercion, but non-carceral and avoidable by exit from the movement's jurisdiction; the categorical framing (no context in which proprietary licensing is acceptable) is the suppressive core. Theater 0.30: the code and licenses are functional, but as industry adopted the pragmatic framing and drained the freedom vocabulary, a growing share of the doctrine's distinct activity is advocacy ritual — campaigns, pledge politics, identity performance — whose output is proclamation rather than code or enforcement. Accessibility_collapse 0.70: within the doctrine's premises the alternatives collapse categorically (accepting the freedoms as inalienable leaves no coherent room for 'sometimes proprietary is fine'), yet three sibling readings remain live in the world, so collapse is partial in fact. Resistance 0.70: the doctrine meets sustained, well-resourced opposition — industry hostility, the pragmatic defection of most of the developer population, and the revealed preferences of the user majority. The measurement series share one grid (T=0..40, mapping 1983..2023): base_extractiveness tracks the referent's extractiveness by the reading's lights (enclosure intensifying through the DRM and SaaS eras), suppression_requirement tracks the enforcement machinery's maturation from persuasion-era to litigation-era, theater_ratio tracks the ritual share. All points are observed history. Suppression is authored as a raw structural property of the doctrine's operation; only extractiveness is subject to the engine's directionality and scope scaling.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the FSF's seat the doctrine is moral law it stewards: enforcement is justice, condemnation is witness. From the proprietary vendors' seat it is an illegitimate categorical condemnation backed by litigation — and their arbitrage (service delivery) means they experience mostly its rhetoric, not its machinery. From the distributors' seat the operative thing is a compliance regime binding them through code they did not choose per-component, with litigation as penalty — they experience enforcement without ever having assented to the moral claim. From the developers' seat it is identity: the doctrine and their professional self-concept are one structure, so its costs (unpaid maintenance, doctrinal policing) are experienced as vocation. From the ordinary users' seat it is nearly invisible — a set of rights never invoked and a judgment never encountered. The engine computes per-seat types from these structural positions; the mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: software_users_collectively (the asserted rights-holders) and free_software_developers (commons and standing) derive d near the beneficiary end; the FSF derives low d as doctrinal steward and beneficiary of authority and funding, with identity_locked exit amplifying rather than damping its stake. Victim declarations drive high d: gpl_distributors bear the enforcement machinery's actual bite with constrained exit (the shared code base is unavoidable), so they sit nearest the full-target end despite being the least morally condemned seat; proprietary_software_vendors are declared victims but their arbitrage-grade exit (SaaS delivery, dual licensing) damps effective extraction well below their nominal condemnation; proprietary_software_users bear censure with mobile exit, damping d toward the middle. The asymmetry worth flagging: the seat the machinery actually extracts from (distributors) is not the seat the doctrine condemns (vendors) — enforcement reach and rhetorical target point at different populations, which the enforcement_condemnation_scope_gap omega tracks. No directionality overrides are authored: the beneficiary/victim plus exit data derive the directionalities without correction, and the override surface is keyed by power atom, which would be too coarse to differentiate the dual-positioned seats here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary enclosure denying users control of their computing — is live, corroborated from outside the movement by right-to-repair legislation, consumer-protection proceedings, and security research on telemetry and lock-in; mandatrophy is therefore NOT resolved and the doctrine is not an atrophied remnant. But the doctrine's mechanism is aging against its problem: it was built for a world of distributed binaries under license, and the enclosure has migrated to service delivery that its licensing machinery cannot reach. The classification work here is to keep three mislabels apart: calling the doctrine a snare erases the genuine commons coordination (the GPL ecosystem is real shared infrastructure, not cover); calling it a rope erases the categorical costs imposed on non-consenting seats (distributors bound without per-component assent, users condemned in absentia); calling it a mountain accepts the reading's self-presentation without testing whether a moral law would carry this beneficiary structure and this enforcement record. The structural data describe genuine coordination plus asymmetric extraction under active enforcement, and the false-summit signature should confirm or refute that assessment from the mountain claim. The identity_coordination declaration is made with the gaming risk in view: the movement's identity vocabulary ('free as in freedom') is genuine boundary maintenance, but the same vocabulary extracts conformity, and the beneficiary_concentration_question omega holds that risk open rather than letting the identity framing excuse it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is software freedom a genuine moral law that would bind any rational agent, or a constructed ethical doctrine authored by a particular movement whose institutions and members are among its beneficiaries?',
    'Meta-ethical analysis of the categorical claims'' source; comparative study of whether the four-freedoms structure recurs in moral traditions unconnected to the movement; historical study of the doctrine''s authorship, adoption, and enforcement record.',
    'If constructed, the fundamental-requirement presentation is a false summit: the arrangement computes as a movement doctrine with identifiable beneficiaries and enforcement costs (tangled-rope structure) rather than a natural law. If natural law, the categorical structure is what it presents itself as and the beneficiary declarations are incidental to its status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether the doctrine''s natural-law self-presentation survives scrutiny against its constructed, beneficiary-bearing operation.').

omega_variable(
    saas_loophole_premise_challenge,
    'Does network-service delivery refute the doctrine''s foundational premise that licensing restrictions are the injustice — since services can subordinate users identically while triggering no license at all?',
    'Track user-control outcomes (repair, modification, data portability, termination rights) across licensed versus service-delivered software; doctrinal analysis of whether the freedom framework can extend to services without abandoning its licensing mechanism.',
    'If the premise is mechanism-specific, the categorical axiom is over-broad and the doctrine''s enforcement apparatus targets the wrong layer of the practice it condemns. If extendable, the interpretation layer absorbs the challenge and the doctrine survives with its kernel intact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(saas_loophole_premise_challenge, empirical, 'Whether the SaaS delivery model overrides the doctrine''s licensing-focused foundational premise.').

omega_variable(
    enforcement_condemnation_scope_gap,
    'Does the doctrine''s operative enforcement (GPL compliance actions against distributors) match its claimed scope (all proprietary software is unjust), or has the operative arrangement narrowed to a distributor-compliance regime while the universal moral claim persists unreached?',
    'Compare the set of enforcement actions, license-compatibility rulings, and compliance settlements against the set of doctrinal pronouncements across the interval; measure the share of condemned proprietary practice the enforcement machinery can actually reach.',
    'A widening gap supports reading the operative arrangement as enforced reciprocity over a shrinking reachable code base wearing a universal moral claim; a narrowing gap would support the categorical self-presentation as the operative reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_condemnation_scope_gap, empirical, 'Whether enforcement reach and rhetorical scope have diverged over the doctrine''s life.').

omega_variable(
    identity_fusion_persistence_question,
    'Is adherence to the categorical doctrine maintained by its moral force or by identity fusion between the founding institutions and the doctrine, such that doctrinal revision is indistinguishable from institutional dissolution?',
    'Observe steward-institution behavior under doctrinal stress (leadership crises, the SaaS challenge, industry defection to the pragmatic framing): does interpretation absorb drift, or does the institution defend the kernel at material cost to itself?',
    'If identity fusion dominates, the doctrine''s persistence is increasingly inertial and its performative share grows (piton-direction drift); if moral force dominates, the doctrine remains live normative infrastructure capable of revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_persistence_question, empirical, 'Whether the doctrine''s persistence runs on conviction or on institutional self-preservation.').

omega_variable(
    beneficiary_concentration_question,
    'Do the doctrine''s benefits flow to users collectively as claimed, or disproportionately to movement institutions and the credentialed developer class, while ordinary users'' revealed preferences are condemned rather than served?',
    'Adoption and survey data on who uses free software by deliberate choice versus by embedding; analysis of who captures doctrinal authority, enforcement precedent, and funding flows.',
    'If benefits concentrate in the movement class, the beneficiary structure is inverted relative to the doctrine''s self-presentation and the false-summit reading strengthens; if diffuse, the coordination story is as claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration_question, empirical, 'Whether the doctrine''s benefit distribution matches its universal-user claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(software_freedom_imperative_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(software_freedom_imperative_tr_t0, observed).
narrative_ontology:measurement(software_freedom_imperative_tr_t8, software_source_status__freedom_imperative_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(software_freedom_imperative_tr_t8, observed).
narrative_ontology:measurement(software_freedom_imperative_tr_t16, software_source_status__freedom_imperative_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement_basis(software_freedom_imperative_tr_t16, observed).
narrative_ontology:measurement(software_freedom_imperative_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(software_freedom_imperative_tr_t24, observed).
narrative_ontology:measurement(software_freedom_imperative_tr_t32, software_source_status__freedom_imperative_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement_basis(software_freedom_imperative_tr_t32, observed).
narrative_ontology:measurement(software_freedom_imperative_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(software_freedom_imperative_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(software_freedom_imperative_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(software_freedom_imperative_be_t0, observed).
narrative_ontology:measurement(software_freedom_imperative_be_t8, software_source_status__freedom_imperative_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(software_freedom_imperative_be_t8, observed).
narrative_ontology:measurement(software_freedom_imperative_be_t16, software_source_status__freedom_imperative_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(software_freedom_imperative_be_t16, observed).
narrative_ontology:measurement(software_freedom_imperative_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(software_freedom_imperative_be_t24, observed).
narrative_ontology:measurement(software_freedom_imperative_be_t32, software_source_status__freedom_imperative_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(software_freedom_imperative_be_t32, observed).
narrative_ontology:measurement(software_freedom_imperative_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement_basis(software_freedom_imperative_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(software_freedom_imperative_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(software_freedom_imperative_su_t0, observed).
narrative_ontology:measurement(software_freedom_imperative_su_t8, software_source_status__freedom_imperative_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement_basis(software_freedom_imperative_su_t8, observed).
narrative_ontology:measurement(software_freedom_imperative_su_t16, software_source_status__freedom_imperative_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement_basis(software_freedom_imperative_su_t16, observed).
narrative_ontology:measurement(software_freedom_imperative_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement_basis(software_freedom_imperative_su_t24, observed).
narrative_ontology:measurement(software_freedom_imperative_su_t32, software_source_status__freedom_imperative_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement_basis(software_freedom_imperative_su_t32, observed).
narrative_ontology:measurement(software_freedom_imperative_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(software_freedom_imperative_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the software freedom debate' covers four structurally distinct claims about one kernel (software_source_status). This file instantiates the freedom_imperative_reading only: a deontological doctrine whose epsilon is authored over the standing proprietary-licensing arrangement by the reading's own lights (0.85 — categorical injustice). The property_rights_reading authors low epsilon over the same referent; the pragmatic and hybrid readings author intermediate values. The readings are separate constraint files linked through network.affects_constraints; the epsilon differences across them are reading-indexed assessments of one shared referent, not measurement noise, and no story may hedge or average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
