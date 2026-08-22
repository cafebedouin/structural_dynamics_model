% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Software Control as Commons Governance (Negotiated Collective Management)
 *   domain: technological/political-economy/intellectual-property
 *
 * SUMMARY:
 *   Software control is governed through a kernel claim—'who legitimately
 *   decides what software does?'—that multiple readings contest. This story
 *   instantiates the COMMONS READING: software control is neither absolute
 *   property nor absolute user freedom, but negotiated collective management
 *   of shared infrastructure. Under this reading, legitimacy derives from
 *   pluralistic governance processes (standards bodies, forge operators,
 *   license stewards) that include developers, users, downstream builders,
 *   and affected communities. The reading acknowledges extraction: absolutist
 *   positions are reframed as stakeholder claims requiring justification, not
 *   foundational principles; excluded communities remain outside governance
 *   tables; and governance institutions exercise power to enforce rules they
 *   set. The constraint is TANGLED ROPE: it coordinates shared-infrastructure
 *   management AND asymmetrically extracts through the reframing
 *   itself—absolutist advocates lose unilateral authority, identity-locked
 *   freedom advocates experience principled compromise as betrayal, and
 *   powerless workers remain excluded from tables that shape their
 *   infrastructure dependency.
 *
 * KEY AGENTS:
 *   - Participatory developer communities: shape commons rules through license stewards and standards committees; benefit from shared infrastructure and participatory legitimacy.
 *   - User coalitions: claim governance voice over interoperability, security, accessibility; benefit from representation in rule-setting; constrained exit.
 *   - Downstream ecosystem actors: benefit from negotiated stability guarantees; institutional power to influence governance.
 *   - Absolutist property advocates: reframed as one stakeholder claim, not foundational authority; experience extraction as loss of unilateral control; mobile exit.
 *   - Absolutist freedom advocates: principled position reframed as negotiable claim; identity-locked—their worldview fuses with their identity; psychological extraction through forced compromise.
 *   - Standards bodies and forge operators: enforce commons rules operationally; institutional agenda-setters.
 *   - Excluded workers and marginalized users: structurally absent from governance tables despite infrastructure dependence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.48).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.52).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Commons Governance (Negotiated Collective Management)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "technological/political-economy/intellectual-property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, 'b42795ec-16e9-40d3-b943-de0803a0542e').
narrative_ontology:cs_kernel_codification('b42795ec-16e9-40d3-b943-de0803a0542e', distributed).
narrative_ontology:cs_authority_grounding('b42795ec-16e9-40d3-b943-de0803a0542e', distributed).
narrative_ontology:cs_reading_relation('b42795ec-16e9-40d3-b943-de0803a0542e', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b42795ec-16e9-40d3-b943-de0803a0542e', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b42795ec-16e9-40d3-b943-de0803a0542e', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_axiom('b42795ec-16e9-40d3-b943-de0803a0542e', foundational, software_governance_requires_pluralism).
narrative_ontology:cs_axiom_status(software_governance_requires_pluralism, holdable).
narrative_ontology:cs_axiom_grounding('b42795ec-16e9-40d3-b943-de0803a0542e', software_governance_requires_pluralism, conventional).
narrative_ontology:cs_axiom('b42795ec-16e9-40d3-b943-de0803a0542e', foundational, governance_legitimacy_derives_from_inclusive_process).
narrative_ontology:cs_axiom_status(governance_legitimacy_derives_from_inclusive_process, holdable).
narrative_ontology:cs_axiom_grounding('b42795ec-16e9-40d3-b943-de0803a0542e', governance_legitimacy_derives_from_inclusive_process, deontological).
narrative_ontology:cs_reference_frame('b42795ec-16e9-40d3-b943-de0803a0542e', software_as_negotiated_collective_infrastructure).
narrative_ontology:cs_drift_state('b42795ec-16e9-40d3-b943-de0803a0542e', contemporary_institutional_gatekeeping_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b42795ec-16e9-40d3-b943-de0803a0542e', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, participatory_developer_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, user_coalitions).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, downstream_ecosystem_actors).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_property_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolutist_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, excluded_governance_voices).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developer collectives that participate in negotiating commons rules: license stewards, forge governance bodies, standards committees. They shape what 'collective management' looks like in practice—what modifications are permitted, what attribution is owed, how derivative works are treated. They benefit from shared infrastructure (version control, CI/CD, community support) and from the legitimacy of participatory governance itself. Exit: they can fork, start parallel governance structures, or migrate to different licensing frameworks.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, participatory_developer_communities, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__commons_reading, participatory_developer_communities, agenda_setter).

% Organized user groups claiming stake in software governance: consumer rights advocates, disability access coalitions, privacy-focused users, workers in software-dependent sectors. Under commons framing they claim governance voice over interoperability, security disclosure, accessibility standards, and data-handling norms. The commons structure legitimates their participation in rule-setting. Exit: limited—they depend on the software ecosystem but can lobby for standards, fork projects, or migrate to alternatives.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, user_coalitions, beneficiary,
    moderate, biographical, constrained, global).

% Enterprises, platforms, and service providers that build on shared software infrastructure (Linux kernel, web browsers, AI frameworks). Under commons governance they gain legitimacy to participate in design decisions affecting their dependencies and to benefit from stability guarantees negotiated through collective governance. Exit: they can maintain forks, invest in alternative stacks, or migrate platforms.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, downstream_ecosystem_actors, beneficiary,
    powerful, generational, mobile, global).

% Commercial software firms and intellectual-property advocates who argue creators have unilateral authority to control software use. Under the commons reading, their position is reframed as one stakeholder claim among many, not a foundational principle. They are asked to justify restrictions within a governance negotiation rather than assert rights unilaterally. They experience the commons frame as denial of their legitimate authority. Exit: they can reject commons participation and assert property claims unilaterally (which some do), or invest in lobbying to reframe the kernel away from commons reading toward property reading.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_property_advocates, payer,
    institutional, generational, mobile, global).

% Free software movement purists who argue proprietary software is ethically illegitimate and that all software must grant user freedom unconditionally. Under the commons reading, their position is reframed as one normative claim among many (valuable, but not exclusive). Commons governance permits negotiating which freedoms are mandatory (repair, audit, fork) and which are optional (commercial use, modification). Purists experience this negotiation as compromise away from principle. Exit: identity-locked—their worldview fuses freedom maximalism with their identity. They can exit by rejecting the commons frame entirely and asserting freedom unilaterally, but this dissolves their participation in pluralistic governance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolutist_freedom_advocates, payer,
    organized, generational, identity_locked, global).

% Workers in software-dependent sectors (healthcare, education, public infrastructure, low-income users in the Global South) whose interests are materially affected by software control decisions but who have no structured voice in commons governance processes. They would object to privatization (which excludes them entirely) and to absolutist freedom maximalism (which assumes costless modification and audit capacity they do not have). They are structurally absent from governance tables where commons rules are negotiated.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, excluded_governance_voices, excluded,
    powerless, biographical, trapped, global).

% Technical governance institutions (IETF, W3C, Linux Foundation, GitHub, GitLab) that enforce and maintain commons rules. They adjudicate licensing disputes, set defaults for new repositories, manage fork coordination, and mediate between competing governance claims. They implement the commons frame operationally and have structural power over what 'collective management' requires in practice.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, standards_bodies_and_forge_operators, agenda_setter,
    institutional, generational, mobile, global).

% The abstract entity: the negotiated body of rules, norms, and institutions that constitute commons governance. Not an actor, but the arrangement itself. Appears in the constraint because the reading vindicates a proposition: that software control is legitimately governed through collective negotiation rather than unilateral assertion.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, collective_software_commons, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(software_control_legitimacy__commons_reading, collective_software_commons).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__commons_reading, standards_bodies_and_forge_operators).
narrative_ontology:fixing_cost_class(software_control_legitimacy__commons_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a governance structure for shared digital infrastructure in which stakeholders with different interests (creators, users, downstream builders, workers, marginalized communities) negotiate rules about software control: what modifications are permitted, what freedoms are mandatory, what restrictions are legitimate, how disputes are arbitrated. Solves the legitimacy problem: who decides what software can do, and on what basis?
% TRANSFER_FUNCTION: Moves authority over software governance from unilateral assertion (property holders or freedom absolutists) to pluralistic negotiation. Transfers decision-making power from creators/commercial interests to multi-stakeholder governance bodies. Transfers a substantive claim: that legitimacy derives from participatory process, not from foundational principle unilaterally asserted.
% ABSENT_VOICES: Workers and users from the Global South with stake in infrastructure governance; people with disabilities whose access needs require negotiated rather than unilateral design; marginalized communities whose interests are affected by software control decisions but who lack representation in technical governance institutions. They would object to both absolute property and absolute freedom framings, each of which excludes them—property because exclusion is its point; freedom because it assumes modification and audit capacity they do not have.
% DISAPPEARANCE_RATIONALE: If the commons reading and its governance infrastructure disappeared, the kernel would collapse into either property-rights framing or freedom-imperative framing or a pragmatic split without legitimacy negotiation. Institutions would revert to unilateral assertion. The distinction between 'software governed by consensus-building institutions' and 'software governed by creators' assertions or market forces' would dissolve. Billions of lines of infrastructure code would be re-contextualized either as legitimate property (if property reading dominated) or as illegitimate (if freedom reading dominated). The ecosystem's current mixed model—some proprietary, some open, all participating in standards bodies—relies on the commons frame to legitimate coexistence.
% FOUNDING_PROBLEM: Early software was written by individual programmers and researchers; control was incidental. As software became critical infrastructure and involved many stakeholders—users, downstream builders, companies, open communities, public interests—unilateral control became a coordination problem: one actor's choices imposed externalities on everyone else (security bugs, incompatibilities, withheld source, anticompetitive lock-in). The founding problem: how to govern shared infrastructure when many stakeholders have material stakes and no single party has the right to dictate terms.
% FOUNDING_PROBLEM_CORROBORATION: The commons reading attests the problem is still live: software is infrastructure, stakes are real, and unilateral control produces externalities. Property-rights advocates argue the problem is solved by IP law and contracts (creators can choose governance, buyers consent). Freedom advocates argue the problem is solved by license mandates (all software must grant freedom). Outside the benefiting parties: infrastructure operators (cloud providers, telcos), public agencies (healthcare, education systems), and marginalized users testify that none of these framings alone governs the actual ecosystem—that mixed governance with negotiated rules is the de facto arrangement and the founding problem persists unsolved.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.48) is moderate because the commons frame genuinely coordinates stakeholders with conflicting interests—it solves a real problem (who governs shared infrastructure?). However, it extracts through reframing: absolutist advocates lose the authority they claimed; freedom maximalists lose the purity they demanded; and excluded voices remain shut out. Suppression (0.52) is moderate because commons rules require active enforcement—governance institutions must suppress absolute-property claims and absolute-freedom claims to maintain the negotiated framework. Theater (0.41) is moderate-high because governance institutions perform legitimacy: they stage participation (advisory boards, comment periods) while institutional power concentrates in standards bodies and forge operators. The measurement series show gentle increase in extractiveness through t=25 (accumulation of governance overhead, increasing scope of what requires governance negotiation), then plateau—a pattern consistent with a constraint that reaches steady state. Suppression requirement rises slightly and stabilizes similarly. Theater remains elevated and stable: the participatory performance is the infrastructure, not a temporary cover.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap is the reading itself. From the commons standpoint, the constraint coordinates legitimate governance. From the property-rights standpoint, it denies creators their legitimate authority. From the freedom-imperative standpoint, it compromises principle. The engine computes these divergences from the structural data: high d for absolutists reflects structural loss of unilateral authority; moderate d for participators reflects genuine coordination benefit coupled with suppression cost. The authored claim (tangled_rope) reflects the structure: coordination (multi-stakeholder governance of infrastructure) + asymmetric extraction (reframing of absolutist positions, exclusion of marginalized voices, institutional power concentration).
 *
 * DIRECTIONALITY LOGIC:
 *   The reading reframes the authority structure. Under property reading, creators have d ≈ 0 (full beneficiaries of their own creation). Under commons reading, creators have d ≈ 0.75 (they benefit from participation, but lose unilateral control). This is not a measurement error; it is a different constraint. The commons reading's directionality is honest: it acknowledges extraction even as it justifies governance coordination. The property reading's directionality would be honest under property-rights reasoning (creation = unilateral authority). They are structurally different constraints reading the same kernel differently.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—'how to govern shared infrastructure when many stakeholders have stakes'—is live and contested. Property advocates claim it is solved by IP law. Freedom advocates claim it is solved by license mandates. Commons advocates argue neither is sufficient and that the actual ecosystem operates through negotiated collective management. The commons constraint prevents mislabeling: it acknowledges both coordination function (genuine problem solved: multi-stakeholder governance) and extraction (reframing of absolutist claims, institutional power, exclusion of marginalized voices). If the founding problem were dead, the constraint would degrade to piton (performance of governance without real function); if it were solved, it would degrade to rope (pure coordination, no extraction). The 'contested' status and the moderate extractiveness both reflect the structural reality: governance is necessary, but contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_vs_capture_ambiguity,
    'Is the observed governance structure genuinely pluralistic negotiation, or is it captured by institutional gatekeepers who perform inclusion while concentrating power?',
    'Trace decision-making processes in actual governance bodies (Linux Foundation, W3C, GitHub, etc.): do decisions reflect explicit negotiation among stakeholders with different interests, or do they track the preferences of institutional insiders? Survey stakeholders on perceived voice and influence.',
    'If governance is captured, the constraint reclassifies as snare—pluralism is cover for institutional extraction. If genuinely pluralistic (different stakeholders win different decisions over time), the tangled_rope classification holds. If partially both (some domains genuinely negotiated, others captured), the constraint family decomposes into multiple per-domain stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_vs_capture_ambiguity, empirical, 'Whether commons governance is genuinely pluralistic or institutionally captured.').

omega_variable(
    identity_locked_freedom_advocates_exit,
    'Can absolutist freedom advocates participate in commons governance without experiencing the frame itself as a betrayal of principle, or is the identity-lock so tight that commons participation is impossible?',
    'Qualitative interviews with freedom-maximalist advocates in governance roles; analysis of discourse in FSM (Free Software Movement) communities about commons/negotiation participation; historical drift in identity-fusion intensity.',
    'If exit is possible (identity-lock is not total), the directionality for freedom advocates is lower (d ≈ 0.65-0.75) and represents constrained participation in a framework they partly reject. If exit is impossible (total identity-fusion), d ≈ 0.85-0.95 and the constraint functions as coercive identity-reframing, reclassifying toward snare. If identity-lock loosens over time (younger developers less identity-fused), theater_ratio should rise and resistance should shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_freedom_advocates_exit, empirical, 'Whether identity-lock for freedom absolutists permits meaningful participation or forces performative exclusion.').

omega_variable(
    excluded_workers_countervailing_power,
    'Do excluded workers and marginalized users have latent countervailing power to demand governance inclusion, or are they structurally powerless to participate?',
    'Track organizing efforts among affected workers (healthcare IT, public sector, Global South); analyze outcomes of inclusion initiatives (disability access committees, labor representation proposals) in governance bodies; measure whether power dynamics shift when excluded voices mobilize.',
    'If countervailing power exists but is latent, the constraint permits potential reclassification if power mobilizes (excluded → organized, trapped → mobile). If power is truly absent (structural powerlessness), the constraint traps marginalized voices indefinitely and the exclusion is built-in, supporting snare classification. The distinction matters for remediation: countervailing power suggests organizational scaffolding; structural absence suggests the commons frame itself may exclude (and alternative framings might not).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_workers_countervailing_power, empirical, 'Whether excluded workers have latent power to shift governance participation or are structurally powerless.').

omega_variable(
    committer_constraint_plurality,
    'This is one reading of the kernel ''software_control_legitimacy''. Are the sibling readings genuinely live positions in contemporary discourse, or have some been foreclosed by material conditions or intellectual drift?',
    'Analysis of contemporary software policy discourse: how many parties actively defend property-rights reading? How many defend freedom-imperative reading? How many occupy pragmatic-openness reading? Do any readings show declining active defense (foreclosure in practice)?',
    'If all four readings remain live, the kernel is genuinely contested (coexists_with relations are appropriate). If one or more have been abandoned, the kernel may have partially collapsed and the constraint family should be revisited—sibling constraints may transition from ''contested readings'' to ''historically important but superseded positions''. If the commons reading specifically has been adopted as the dominant framework (all parties now negotiate within it), the kernel has resolved and the entire constraint family transitions from active contest to historical study.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_constraint_plurality, conceptual, 'Whether all sibling readings of the kernel remain live in contemporary discourse or some have been materially foreclosed.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the measured suppression (0.52) structural (active enforcement machinery required to maintain commons frame) or internalized (developers and advocates have internalized the negotiation norm such that suppression persists even in the absence of enforcement)?',
    'Post-exit observation: if enforcement institutions were dismantled, would suppression persist or dissipate? Natural experiment from jurisdictions that reject commons governance (e.g., software policy regimes that mandate property-rights or freedom-imperative reading): what enforcement machinery is required to maintain the alternative frame?',
    'If structural, suppression (0.52) is accurate and reflects real institutional cost of maintaining pluralism. If internalized, the effective suppression is higher than measured—the frame persists through cognitive capture, not just institutional coercion. If both, the relative proportion matters for understanding stability: high internalization suggests the constraint has successfully normalized; high structural suppression suggests it is maintained artificially and brittle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether commons-governance suppression is structural enforcement or internalized cognitive capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(soft_tr_t0, observed).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(soft_tr_t5, observed).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(soft_tr_t10, observed).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(soft_tr_t15, observed).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(soft_tr_t20, observed).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__commons_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(soft_tr_t25, observed).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__commons_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(soft_tr_t30, observed).
narrative_ontology:measurement(soft_tr_t35, software_control_legitimacy__commons_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(soft_tr_t35, observed).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__commons_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(soft_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(soft_be_t0, observed).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement_basis(soft_be_t5, observed).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement_basis(soft_be_t10, observed).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement_basis(soft_be_t15, observed).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(soft_be_t20, observed).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__commons_reading, base_extractiveness, 25, 0.49).
narrative_ontology:measurement_basis(soft_be_t25, observed).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__commons_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(soft_be_t30, observed).
narrative_ontology:measurement(soft_be_t35, software_control_legitimacy__commons_reading, base_extractiveness, 35, 0.47).
narrative_ontology:measurement_basis(soft_be_t35, observed).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__commons_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(soft_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(soft_su_t0, observed).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement_basis(soft_su_t5, observed).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement_basis(soft_su_t10, observed).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(soft_su_t15, observed).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(soft_su_t20, observed).
narrative_ontology:measurement(soft_su_t25, software_control_legitimacy__commons_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement_basis(soft_su_t25, observed).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__commons_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(soft_su_t30, observed).
narrative_ontology:measurement(soft_su_t35, software_control_legitimacy__commons_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement_basis(soft_su_t35, observed).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__commons_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(soft_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__commons_reading, 0.18).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'software_control_legitimacy'. The constraint family consists of four sibling readings, each instantiating a different constraint from the same kernel. Commons reading structures pluralistic governance; property reading asserts creator authority; freedom reading asserts user freedom; pragmatic reading treats licensing as methodology choice. Constraints linked by network.affects_constraints form the complete kernel contest. Each reading authors its own ε, beneficiary/victim set, and type; they are not perspectives on one constraint but distinct constraints reading the same kernel differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_control_legitimacy__commons_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
