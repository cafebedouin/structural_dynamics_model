% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software Control as Negotiated Commons Governance
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This story instantiates the COMMONS reading of the contested
 *   software-control-legitimacy kernel: software control is neither a matter
 *   of absolute user freedom nor of absolute creator property, but a
 *   negotiated collective-management problem over shared digital
 *   infrastructure. Under this reading, governance bodies (foundations,
 *   steering committees, contributor councils) administer decision rights
 *   that no single party holds unilaterally. Both absolutist positions —
 *   freedom-imperative advocates who consider any restriction illegitimate,
 *   and property-rights holders who consider any negotiated limit on their
 *   control illegitimate — are structurally denied the unilateral authority
 *   their own frameworks claim they should have, and both therefore enter
 *   this reading's victim set. The beneficiaries are the organized
 *   stakeholder communities and better-resourced downstream integrators who
 *   can actually participate in and shape governance; unfunded solo
 *   maintainers occupy an ambiguous position, formally elevated as
 *   stakeholders but structurally under-compensated for the coordination
 *   labor the regime depends on.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.42).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.38).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Negotiated Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '5bba6537-7236-4150-9625-0ffe9ee5fa6a').
narrative_ontology:cs_kernel_codification('5bba6537-7236-4150-9625-0ffe9ee5fa6a', distributed).
narrative_ontology:cs_authority_grounding('5bba6537-7236-4150-9625-0ffe9ee5fa6a', distributed).
narrative_ontology:cs_reading_relation('5bba6537-7236-4150-9625-0ffe9ee5fa6a', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bba6537-7236-4150-9625-0ffe9ee5fa6a', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('5bba6537-7236-4150-9625-0ffe9ee5fa6a', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('5bba6537-7236-4150-9625-0ffe9ee5fa6a', foundational, control_legitimacy_requires_negotiated_multi_stakeholder_process).
narrative_ontology:cs_axiom_status(control_legitimacy_requires_negotiated_multi_stakeholder_process, holdable).
narrative_ontology:cs_axiom_grounding('5bba6537-7236-4150-9625-0ffe9ee5fa6a', control_legitimacy_requires_negotiated_multi_stakeholder_process, conventional).
narrative_ontology:cs_axiom('5bba6537-7236-4150-9625-0ffe9ee5fa6a', foundational, no_single_party_premise_unilaterally_settles_control).
narrative_ontology:cs_axiom_status(no_single_party_premise_unilaterally_settles_control, holdable).
narrative_ontology:cs_axiom_grounding('5bba6537-7236-4150-9625-0ffe9ee5fa6a', no_single_party_premise_unilaterally_settles_control, instrumental).
narrative_ontology:cs_reference_frame('5bba6537-7236-4150-9625-0ffe9ee5fa6a', distributed_maintainer_led_project_norms).
narrative_ontology:cs_drift_state('5bba6537-7236-4150-9625-0ffe9ee5fa6a', contemporary_foundation_governed_infrastructure_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5bba6537-7236-4150-9625-0ffe9ee5fa6a', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, downstream_integrators).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, long_tail_maintainers).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_property_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, unfunded_solo_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, unfunded_solo_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Users, downstream projects, corporate consumers, and contributors who jointly depend on a piece of shared software infrastructure. Under a commons framing they negotiate governance bodies, contribution rules, license terms, and dispute processes rather than deferring wholesale to either an original author's absolute property claim or an ideological demand for unrestricted freedom. They gain durable, jointly-legitimated access but must invest time and political capital in governance participation, and outcomes reflect whoever shows up and organizes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, stakeholder_communities, agenda_setter).

% Hold that any restriction on copying, modifying, or redistributing software is illegitimate regardless of negotiated process. Under commons governance their categorical position is treated as one input among many rather than a trump card; governance bodies can adopt licensing compromises (e.g., dual-licensing, contributor agreements, trademark carve-outs) that they experience as a betrayal of principle. They cannot exit the negotiation without losing influence over infrastructure they depend on, so they remain inside a process whose legitimacy they reject at the premise level.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_freedom_advocates, payer,
    moderate, civilizational, constrained, global).

% Original authors, founding companies, or IP holders who hold that their creation entitles them to unilateral control over use, modification, and monetization. Commons governance subjects their control to negotiated limits — community fork rights, governance board vetoes, contributor license reciprocity — that they experience as expropriation of legitimate authority. They retain formal exit (closing the project, relicensing prospectively) but lose the practical ability to act unilaterally once a community has organized around the software.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_property_holders, payer,
    powerful, biographical, constrained, global).

% Individual maintainers of widely-depended-upon components who did not ask to become infrastructure. Commons governance formally elevates their voice (they are 'stakeholders') but in practice the governance apparatus rarely allocates them resources commensurate with the load they carry; they absorb the coordination costs of a negotiated regime — RFC processes, community moderation, security triage — without proportional compensation, while larger organized stakeholders capture most of the negotiated benefit. Exit means abandoning the project, which downstream commons participants treat as a crisis to be managed, not a signal to compensate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, unfunded_solo_maintainers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, unfunded_solo_maintainers, beneficiary).

% Corporations and large projects that consume the commons-governed software as infrastructure. They benefit from negotiated stability (predictable licensing, community-maintained security patching, forkability as insurance against capture) without bearing the ongoing maintenance burden. They can exit to alternative infrastructure or fund a fork if governance turns against them, giving them outsized influence over governance outcomes relative to their numeric share of the community.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, downstream_integrators, beneficiary,
    powerful, generational, arbitrage, global).

% The foundation, steering committee, or board structure that administers the negotiated governance process — sets contribution rules, adjudicates disputes, manages licensing decisions, and represents the commons framing institutionally. It exists because the commons reading requires an administering body; its legitimacy depends on being seen as neutral among the absolutist positions and the differently-resourced participants.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, governance_body, agenda_setter,
    institutional, generational, analytical, global).

% Courts and regulators who occasionally adjudicate disputes arising from commons governance arrangements — license enforceability, antitrust concerns around dominant open infrastructure, or property claims contesting a governance body's authority. They observe the negotiated arrangement from outside and can validate or destabilize it through rulings.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, competition_and_ip_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shared software infrastructure requires ongoing decisions about contribution, modification rights, security response, and forking that no single absolutist position can make unilaterally without excluding parties who depend on the software; a negotiated governance process lets heterogeneous stakeholders (authors, corporate consumers, volunteer maintainers, downstream users) continue to jointly build on and depend on the same codebase despite disagreeing about the underlying legitimacy question.
% TRANSFER_FUNCTION: Moves practical control — decision rights over licensing terms, contribution acceptance, fork legitimacy, and security disclosure — away from whoever would otherwise hold it unilaterally (either the original property claimant or an ungoverned free-for-all) and toward whichever coalition of stakeholders is organized enough to participate in governance; moves uncompensated coordination labor toward unfunded maintainers who carry the day-to-day cost of the negotiated regime.
% ABSENT_VOICES: End users with no technical capacity to participate in governance forums (they consume the software but never attend a steering committee meeting or file an RFC); future maintainers not yet recruited who will inherit governance decisions made before they had any voice; the absolutist positions are present but structurally out-voted rather than absent — their objection is that mere presence in a negotiation they consider illegitimate is itself the harm.
% DISAPPEARANCE_RATIONALE: If commons governance structures vanished overnight, control would revert by default to whoever holds the strongest unilateral lever — typically the original IP holder or corporate steward — and freedom-oriented forks would proliferate uncoordinated, fragmenting the shared infrastructure; downstream integrators would face sudden uncertainty about license stability and security maintenance, and unfunded maintainers would lose the (thin) institutional cover the governance body currently provides.
% FOUNDING_PROBLEM: Widely-used software infrastructure was being pulled apart by two absolutist failure modes: unilateral proprietary control that excluded contributors and users from decisions affecting their dependency, and unlicensed freedom-maximalism that produced unmaintainable forks and no stable authority for security or compatibility decisions. Commons governance was built to let heterogeneous, mutually-dependent parties keep building on shared code without either failure mode winning outright.
% FOUNDING_PROBLEM_CORROBORATION: Independent empirical study of open-source foundation governance (e.g., academic research on Apache Software Foundation, Linux Foundation, and CNCF governance models) documents ongoing coordination failures under both pure-proprietary and pure-permissive regimes, corroborating the problem from outside the governance bodies themselves; however, most sitting testimony to the problem's continued severity still comes from the governance bodies and organized stakeholder communities that administer and benefit from the negotiated arrangement, so corroboration is partial rather than fully independent.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.42) sits at a moderate mid-range because the commons reading genuinely redistributes decision rights away from both absolutist poles toward organized participants — real coordination is happening, but the redistribution is uneven: better-resourced actors (corporate downstream integrators, well-organized community factions) capture disproportionate influence relative to under-resourced ones (solo maintainers, individual freedom advocates without institutional backing). Suppression (0.38) reflects that the negotiated regime does foreclose unilateral action by either absolutist pole through governance rules, contributor agreements, and community-enforced norms, but does not use hard coercion — exit via forking remains technically available, which caps suppression well below a snare-level reading. Theater ratio (0.28) captures that some governance activity (advisory boards with limited real authority, symbolic community input processes) is more performative than decisive, though the core coordination function (security response coordination, license stewardship, contribution triage) is genuinely functional. Accessibility collapse (0.35) is moderate: once a project's commons governance calcifies, meaningfully contesting its structure from outside becomes hard, but forking remains a real, if costly, alternative — this is not a near-total collapse like a natural law. Resistance (0.55) is elevated because both absolutist camps actively contest the legitimacy of the negotiated framing itself, not merely its particular outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the governance body's and organized stakeholder communities' seats, this looks like legitimate, functioning coordination — a negotiated middle path that keeps infrastructure alive and stakeholders engaged. From either absolutist seat, the same structure looks like an illegitimate imposition that never should have had authority to negotiate away their categorical claim in the first place. From the unfunded maintainer's seat, it looks like being nominally empowered while structurally exploited. The engine should compute genuinely different per-seat classifications from these structural positions rather than collapsing them to one verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities and downstream integrators are declared beneficiaries because commons governance gives them durable, jointly-legitimated access to infrastructure they would otherwise have to either build unilaterally or depend on precariously; the engine should derive low-to-moderate d for these seats. Absolutist freedom advocates and absolutist property holders are declared victims not because commons governance extracts material resources from them in the conventional sense, but because it structurally denies both groups the unilateral authority their own frameworks assert they are entitled to — the extraction here is of decision-rights, not money, and both camps experience the negotiated compromise as an imposition on a premise they reject categorically. Unfunded solo maintainers are the hardest case: formally a beneficiary (elevated stakeholder status) but structurally closer to a victim (uncompensated load-bearing labor), hence the dual role declaration and trapped exit_options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating mutually-dependent parties around shared infrastructure without either unilateral-property or unlicensed-freedom failure modes — remains live by most independent accounts, which weighs against reading this as pure mandatrophy (a governance apparatus persisting after its function died). However, the accumulating extractiveness trend (0.30 to 0.42 over the interval) combined with a rising theater ratio suggests governance bodies may be layering procedural overhead and diffuse-benefit capture onto a genuine coordination core over time — worth flagging for the T17 abductive trigger rather than treating as settled. This is exactly the kind of reading where classifying too quickly as either pure Rope (ignoring the uneven capture by organized/resourced actors) or pure Snare (ignoring the real coordination function that keeps shared infrastructure alive) would mislabel the structure; the Tangled Rope claim keeps both the coordination and the extraction visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_legitimacy_vs_capture,
    'Is the negotiated governance process a genuine collective-management solution, or is it a legitimating veneer over capture by whichever stakeholder faction is best organized (typically well-resourced corporate downstream integrators)?',
    'Longitudinal study of governance-body decision records against contribution/funding data: if decision outcomes track funding and organizational resourcing more than technical merit or broad stakeholder input over time, capture is the better model.',
    'If capture-dominant, effective extraction is higher than the authored 0.42 and the classification should drift toward snare for the under-resourced payer seats even though it remains tangled_rope in aggregate; if genuinely balanced, the tangled_rope reading with moderate extraction is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_legitimacy_vs_capture, empirical, 'Whether commons governance is genuine multi-stakeholder balance or resourcing-driven capture.').

omega_variable(
    kernel_framing_choice,
    'Is ''software control'' best modeled as a single contested kernel with four readings (as done here), or does the commons reading itself further decompose depending on which governance model (foundation-based, BDFL-with-council, fully distributed consensus) is instantiated — each of which might carry a materially different epsilon?',
    'Compare epsilon and victim/beneficiary structure across concrete governance-model case studies (Apache-style foundation vs. Linux-style BDFL-plus-maintainers vs. Debian-style constitutional democracy); if epsilon diverges sharply across these, the commons_reading itself should decompose per the ε-invariance principle rather than remaining one story.',
    'If governance models produce sharply divergent epsilon, this single commons_reading constraint is itself an averaged fiction and should split into per-governance-model sibling stories under this same kernel reading; if epsilon stays in a similar band across models, the single-story treatment is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the commons reading is itself a family of structurally distinct governance-model constraints rather than one constraint.').

omega_variable(
    unfunded_maintainer_classification,
    'Should unfunded solo maintainers be modeled primarily as beneficiaries (elevated stakeholder status under commons framing) or primarily as victims (uncompensated load-bearing labor), given that the commons reading''s own legitimacy partly depends on claiming to represent their interests?',
    'Track compensation, decision-influence, and burnout/departure rates for solo maintainers of commons-governed critical infrastructure (e.g., following patterns documented in incidents like the Log4j and OpenSSL maintenance crises) against the same metrics for organized stakeholder factions.',
    'If maintainer burden and burnout substantially exceed their decision-influence and compensation relative to organized factions, the dual-role declaration understates their victimhood and effective extraction on that seat is higher than modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfunded_maintainer_classification, empirical, 'Whether solo maintainers are net beneficiaries or net victims of commons governance framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t4, software_control_legitimacy__commons_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__commons_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__commons_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__commons_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__commons_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t4, software_control_legitimacy__commons_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__commons_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__commons_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__commons_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__commons_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(soft_su_t4, software_control_legitimacy__commons_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__commons_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(soft_su_t12, software_control_legitimacy__commons_reading, suppression_requirement, 12, 0.32).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__commons_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__commons_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of four siblings decomposing the natural-language concept 'software control legitimacy' per the ε-invariance principle. The commons_reading, freedom_imperative_reading, pragmatic_openness_reading, and property_rights_reading are structurally distinct claims about who legitimately holds control over software and on what basis — they carry different beneficiary/victim structures and different epsilon values rather than being one constraint viewed from four angles. The commons_reading is distinguished by placing BOTH absolutist positions (freedom_imperative and property_rights) in its own victim set, since it denies either unilateral premise standing; pragmatic_openness_reading, by contrast, treats both open and proprietary models as equally legitimate methodology choices and should show a very different victim structure (likely minimal victims, since it does not deny anyone's premise, only relativizes the stakes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
