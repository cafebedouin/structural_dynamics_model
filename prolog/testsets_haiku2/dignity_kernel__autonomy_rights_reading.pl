% ============================================================================
% CONSTRAINT STORY: dignity_kernel__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__autonomy_rights_reading, []).

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
 *   constraint_id: dignity_kernel__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Grounding of Dignity (vs. Divine Image)
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This constraint instantiates the autonomy-rights reading of the dignity
 *   kernel — a particular framing of human worth as grounded in rational
 *   agency and enforceable rights rather than in theological concepts like
 *   imago dei. The reading has become institutionalized in AI governance,
 *   labor protections, and privacy regulation over the past two decades. It
 *   is simultaneously a genuine coordination achievement (enabling shared
 *   language for protecting agency against algorithmic manipulation) and an
 *   extractive mechanism (concentrating dignity-adjudication power in secular
 *   institutions, subordinating alternative dignity frameworks, and creating
 *   new compliance burdens that corporations and states enforce through
 *   administrative power). The constraint's claim/metric gap is intentional
 *   and central: the reading claims coordination (protecting autonomy) while
 *   the metrics describe a tangled situation where the autonomy-rights frame
 *   has become a tool for institutional power consolidation.
 *
 * KEY AGENTS:
 *   - Secular governance frameworks: institutional agenda-setters defining dignity operationally and enforcing autonomy-rights language in policy
 *   - Transparency advocates: civil society organizations benefiting from the autonomy-rights frame by demanding disclosure
 *   - Rights-protection institutions: courts and regulators translating autonomy language into enforceable standards
 *   - Users and workers denied autonomy: powerless agents whose dignity violation is named and partially protected under this reading
 *   - Religious traditions and non-Western dignity framings: excluded from the institutional conversation, denied policy-setting power
 *   - AI corporations: powerful actors constrained by the reading's transparency and accountability demands
 *   - Analytical observer: tracing the reading's institutional function and its role in replacing alternative dignity grounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, 0.68).
domain_priors:suppression_score(dignity_kernel__autonomy_rights_reading, 0.74).
domain_priors:theater_ratio(dignity_kernel__autonomy_rights_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__autonomy_rights_reading, "Autonomy-Rights Grounding of Dignity (vs. Divine Image)").
narrative_ontology:topic_domain(dignity_kernel__autonomy_rights_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__autonomy_rights_reading, '47a40017-2065-4ebe-9186-21a36747287c').
narrative_ontology:cs_kernel_codification('47a40017-2065-4ebe-9186-21a36747287c', distributed).
narrative_ontology:cs_authority_grounding('47a40017-2065-4ebe-9186-21a36747287c', extraction).
narrative_ontology:cs_interpretation_layer_present('47a40017-2065-4ebe-9186-21a36747287c').
narrative_ontology:cs_reading_relation('47a40017-2065-4ebe-9186-21a36747287c', dignity_kernel__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('47a40017-2065-4ebe-9186-21a36747287c', dignity_kernel__posthumanist_reading, coexists_with).
narrative_ontology:cs_axiom('47a40017-2065-4ebe-9186-21a36747287c', foundational, autonomy_as_dignity_foundation).
narrative_ontology:cs_axiom_status(autonomy_as_dignity_foundation, holdable).
narrative_ontology:cs_axiom_grounding('47a40017-2065-4ebe-9186-21a36747287c', autonomy_as_dignity_foundation, deontological).
narrative_ontology:cs_axiom('47a40017-2065-4ebe-9186-21a36747287c', foundational, rationality_as_agency_marker).
narrative_ontology:cs_axiom_status(rationality_as_agency_marker, holdable).
narrative_ontology:cs_axiom_grounding('47a40017-2065-4ebe-9186-21a36747287c', rationality_as_agency_marker, empirically_contingent).
narrative_ontology:cs_axiom('47a40017-2065-4ebe-9186-21a36747287c', secondary, rights_as_enforcement_mechanism).
narrative_ontology:cs_axiom_status(rights_as_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('47a40017-2065-4ebe-9186-21a36747287c', rights_as_enforcement_mechanism, conventional).
narrative_ontology:cs_reference_frame('47a40017-2065-4ebe-9186-21a36747287c', enlightenment_secular_dignity).
narrative_ontology:cs_drift_state('47a40017-2065-4ebe-9186-21a36747287c', contemporary_ai_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47a40017-2065-4ebe-9186-21a36747287c', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(dignity_kernel__autonomy_rights_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, secular_governance_frameworks).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, transparency_advocates).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, rights_protection_institutions).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, users_denied_autonomy).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, workers_in_opaque_ai_systems).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, populations_subject_to_coercive_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, ai_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legal and policy frame within which AI governance, labor protections, and privacy regimes operate. Defines dignity operationally as autonomy, consent, and rights. Enforces this frame through regulation, litigation, and institutional authority. Collects legitimacy and regulatory power from the successful deployment of autonomy-rights language in tech governance.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, secular_governance_frameworks, agenda_setter,
    institutional, generational, arbitrage, national).

% Civil society organizations, technologists, and researchers who promote disclosure of AI decision-making, algorithmic audits, and interpretability. They frame transparency as essential to autonomy — users cannot exercise agency over systems they cannot understand. Benefit from the autonomy-rights reading by having a principled justification for demanding disclosure.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, transparency_advocates, beneficiary,
    organized, biographical, mobile, global).

% Courts, human rights commissions, labor regulators, and data protection authorities that translate autonomy-rights language into enforceable standards. They gain institutional mandate and legal standing from the autonomy-rights foundation; it provides the doctrine they use to constrain corporate and state power.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, rights_protection_institutions, beneficiary,
    institutional, generational, analytical, national).

% Individuals whose autonomy is violated by opaque AI systems — recommendation algorithms that manipulate behavior, hiring systems that reject without explanation, facial recognition that surveils without consent. Under the autonomy-rights reading, their dignity is specifically violated; they are the identified victims whose protection justifies enforcement.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, users_denied_autonomy, payer,
    powerless, biographical, trapped, global).

% Gig workers, content moderators, data laborers whose work is coordinated by algorithmic systems they cannot interrogate or challenge. Treated as optimization variables rather than autonomous agents. The autonomy-rights reading mandates that their labor terms be transparent and subject to consent, making opacity a violation of their dignity.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, workers_in_opaque_ai_systems, payer,
    powerless, biographical, constrained, global).

% Communities subject to predictive policing, welfare surveillance, or border enforcement systems that deny agency in the name of security or efficiency. Under the autonomy-rights reading, their autonomy violation is the primary harm, not the security outcome — making the constraint itself a tool to resist such systems.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, populations_subject_to_coercive_systems, payer,
    powerless, biographical, trapped, global).

% Faith communities and theologians who ground dignity in imago dei or other non-rationalist frameworks. They are excluded from the autonomy-rights reading's institutional power — their framing is treated as private belief rather than public policy language. They would contest that autonomy and rationality are sufficient grounds for dignity.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, religious_dignity_traditions, excluded,
    organized, civilizational, trapped, global).

% Technology developers and philosophers who argue for radical cognitive enhancement and superintelligence as continuous with human flourishing. The autonomy-rights reading's caution toward enhancement and its assumption of a stable human form constrains their agenda. They would argue the reading artificially freezes human capacity as a baseline.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, transhumanist_advocates, excluded,
    moderate, biographical, mobile, global).

% Technology companies deploying large-scale AI systems. The autonomy-rights reading imposes transparency, accountability, and consent requirements on their operations — translating into compliance costs, disclosure obligations, and regulatory constraint. They pay through legal compliance and reduced proprietary advantage; they are also excluded from setting the terms of dignity itself.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, ai_corporations, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, ai_corporations, excluded).

% Scholars and analysts examining the constraint from outside its enforcement apparatus — tracing how autonomy-rights language functions institutionally, where it produces dignity protection and where it becomes cover for new extraction.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__autonomy_rights_reading, secular_governance_frameworks).
narrative_ontology:fixing_cost_class(dignity_kernel__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared language and institutional framework for recognizing that individuals have inherent moral status grounded in their capacity for autonomous agency and rational choice. Solves the coordination problem of how to build AI governance systems that respect agency rather than treat humans as optimization variables or objects of administrative manipulation.
% TRANSFER_FUNCTION: Moves interpretive authority over what constitutes dignity from religious and theological traditions to secular institutions (governments, courts, corporations, international bodies). Subordinates non-rationalist dignity framings (imago dei, embodied care, collective flourishing) to rationality-and-rights language. Concentrates enforcement power in secular governance, making access to dignity protection dependent on institutional capacity to invoke and deploy the autonomy-rights frame.
% ABSENT_VOICES: Religious dignity traditions and non-Western philosophical framings of human worth (Ubuntu, Confucian relational dignity, indigenous cosmologies that do not separate the human from ecological or spiritual dimensions) are not in the conversation at the policy level. They would object that autonomy-rights language erases their grounding of dignity and imposes secular individualism as the only legitimate framework.
% DISAPPEARANCE_RATIONALE: If the autonomy-rights reading vanished and were replaced by, say, imago dei grounding, AI governance would reorganize around human inviolability and equality rather than autonomy protection. Privacy protections might weaken (if equal dignity flows from being made in God's image, not from rational consent). Enhancement pressures might intensify or reverse depending on theological stance. Labor and consent frameworks would shift. The institutional power structure would realign toward religious authority or pluralistic accommodation of multiple dignity grounds.
% FOUNDING_PROBLEM: Rapid deployment of opaque, manipulative AI systems that treat human agency as an optimization variable rather than as intrinsically valuable. Digital systems that extract behavioral data without consent, make decisions affecting lives without explanation, and concentrate power in corporate hands. The founding problem is the mismatch between technical power and human autonomy — systems can predict and steer behavior more accurately than humans can understand or resist.
% FOUNDING_PROBLEM_CORROBORATION: Technologists, rights organizations (Amnesty International, Electronic Frontier Foundation, algorithmic justice researchers), labor advocates, and policy researchers independent of the institutional beneficiaries attest the founding problem persists and has accelerated. The problem is empirically documented: algorithmic bias, recommendation system manipulation, surveillance scope, and labor platform opacity are measurable and growing. The autonomy-rights reading is the framing secular institutions have adopted to address it; whether it is the only or the best frame is contested.
narrative_ontology:disappearance_verdict(dignity_kernel__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignity_kernel__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__autonomy_rights_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is elevated (0.68 at interval end) because the autonomy-rights reading consolidates interpretive authority over dignity in secular institutions, subordinating theological and non-rationalist framings. This is genuine extraction: other traditions must either accept the secular frame or be treated as private belief outside public policy. Suppression is higher (0.74) because institutional acceptance of the autonomy-rights reading requires active maintenance — alternative readings must be rhetorically suppressed (marginalized as 'faith' rather than policy), and dissenting traditions must be excluded from the rooms where dignity is defined operationally. Theater is moderate (0.41): the transparency and accountability demands are real, but they increasingly function as compliance theater — corporations adopt 'ethical AI' frameworks while opacity actually deepens, and governments pass transparency laws while surveillance expands. The measurement series show this degradation over time: extractiveness rising as the reading's institutional capture solidifies, theater rising as compliance language outpaces actual protection. Both trends are observable: early adoption of autonomy-rights language (t=0–5) was genuinely disruptive to corporate power; later adoption (t=15–25) becomes administrative routine.
 *
 * PERSPECTIVAL GAP:
 *   Secular governance institutions and transparency advocates experience the autonomy-rights reading as liberatory — a principled basis for constraining corporate and state power over human agency. From their seat, the constraint is genuine coordination because it enables shared standards and legal recourse. Powerless users and workers experience it as a framing that names their violation but often fails to prevent it — compliance mechanisms are weak, enforcement is expensive, and the legal right to autonomy often does not translate to material capability. Religious traditions experience the reading as colonial — an imposition of secular rationality as the only legitimate dignity ground, marginalizing their own deep philosophical resources. The engine should compute substantial seat divergence here, with governance institutions and advocates computing tangled_rope (coordination + extraction from religious alternatives) while victims compute snare (the autonomy framing names their harm but does not prevent it) and excluded traditions compute snare (systematic subordination to secular power). This is not a flaw in the constraint story — it is the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular governance frameworks benefit from defining dignity operationally and enforcing the autonomy-rights language — they concentrate interpretive authority and institutional power. d ≈ 0.2 (beneficiary, but constrained by rights frameworks). Transparency advocates genuinely benefit from the reading without bearing extraction costs — they have what they want. d ≈ 0.1. Rights-protection institutions gain mandate and authority. d ≈ 0.2. Users and workers denied autonomy should be protected but often are not — they are named as victims but face high costs to invoke their rights. d ≈ 0.8 (target). Religious traditions are excluded and subordinated. d ≈ 0.85 (target, non-agent extracted from). AI corporations must comply but have workarounds and still hold power. d ≈ 0.65. The directionality distribution is asymmetric because the reading privileges secular institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy-rights reading prevents mandatrophy by remaining grounded in a live founding problem: opaque, manipulative AI systems do proliferate and do violate autonomy. However, there is a secondary mandatrophy risk: the founding problem (technical power outpacing human understanding) could be solved by redesigning systems to be genuinely transparent and human-controllable, which would make autonomy-rights enforcement unnecessary. If such redesign occurred, the reading's mandate would expire but the institutional structures (rights bodies, compliance regimes, regulatory agencies) would persist, shifting the constraint toward piton (performance of autonomy protection after the technical problem is solved). The measurement series show a rising theater ratio, which is consistent with this trajectory: as transparency demands become formalized and institutionalized, actual technical change may lag behind compliance language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_enhancement_boundary,
    'Where does autonomy-respecting cognitive enhancement end and instrumentalizing rationality begin? Can enhancement be authored as consistent with this reading, or does enhancement risk treating rationality as raw material to be optimized rather than as an expression of dignity?',
    'Comparative analysis of enhancement frameworks that distinguish agent-directed improvement (autonomy-consistent) from imposed capability modification (instrumentalizing). Empirical test: do individuals consent to enhancements freely, or are enhancement pressures economic/social coercion masked as choice?',
    'If enhancement pressures are coercive, this reading''s victim set expands to include enhanced subjects whose autonomy is compromised by the path to enhancement. If enhancement can be authored as autonomy-consistent, the reading gains scope but faces empirical scrutiny about consent conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_enhancement_boundary, conceptual, 'Whether cognitive enhancement is compatible with the autonomy-rights foundation of dignity or constitutes instrumentalization.').

omega_variable(
    secular_foundation_institutional_capture,
    'Is the secular autonomy-rights foundation of dignity inherently captured by secular institutional power (the state, corporations, markets), such that autonomy as a governing principle becomes a cover for administrative extraction?',
    'Historical comparison of autonomy-rights regimes: where has secular dignity grounding resisted institutional capture, and where has it been absorbed as legitimating theater for power? Structural test: do autonomy-rights frameworks produce enforceable limits on institutional power, or do they become tools for rationalizing surveillance and control?',
    'If autonomy-rights frameworks are structurally vulnerable to institutional capture, this reading''s claim to ground dignity without extraction becomes contested — the constraint itself may slide from tangled_rope to snare as administrative power consolidates. If enforceable limits emerge, the reading''s capacity to protect dignity is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_foundation_institutional_capture, empirical, 'Whether secular autonomy-rights foundations are structurally resistant to institutional capture or constitute a new vector for administrative extraction.').

omega_variable(
    rationality_as_contested_capacity,
    'Is rationality a stable, measurable human capacity that can ground dignity claims, or is rationality itself culturally constructed, contestable, and weaponizable in ways that undermine dignity?',
    'Genealogy of rationality: what counts as rational has shifted across contexts (instrumental/communicative, individual/collective, formal/embodied). Empirical test: have rationality metrics been used to exclude or dehumanize (historical IQ racism, psychiatric rationality gates, disability rationality discrimination)?',
    'If rationality is contested and culturally constructed, this reading''s grounding becomes circular — dignity flows from a capacity whose definition and measurement are themselves contested power plays. This would require either redefining autonomy to precede rationality, or acknowledging that this reading encodes a particular rationality paradigm as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationality_as_contested_capacity, empirical, 'Whether rationality is a stable foundation for dignity or a contested, weaponizable concept.').

omega_variable(
    kernel_reading_identity,
    'This constraint instantiates the autonomy_rights_reading of the dignity kernel. Are there empirical, conceptual, or preference-based grounds for treating this reading as more legitimate than the imago_dei or posthumanist readings? Or is the reading choice itself a power choice that cannot be adjudicated from within secular rationality alone?',
    'Meta-level: acknowledgment that reading choice is constitutive, not discovered. Different epistemic communities (secular governance institutions, religious traditions, technological transhumanists) ground dignity differently, and no single reading can exhaust the kernel''s political and philosophical weight.',
    'High. This omega documents the irreducible pluralism the constraint story represents: one coherent framing of dignity among others. Acknowledging this opens pathways for cross-reading dialogue rather than zero-sum foreclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the autonomy-rights reading is the only defensible reading of the dignity kernel, or whether reading pluralism must be sustained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__autonomy_rights_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignity_kernel__autonomy_rights_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dign_tr_t5, dignity_kernel__autonomy_rights_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(dign_tr_t10, dignity_kernel__autonomy_rights_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(dign_tr_t15, dignity_kernel__autonomy_rights_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(dign_tr_t20, dignity_kernel__autonomy_rights_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(dign_tr_t25, dignity_kernel__autonomy_rights_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignity_kernel__autonomy_rights_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dign_be_t5, dignity_kernel__autonomy_rights_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(dign_be_t10, dignity_kernel__autonomy_rights_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(dign_be_t15, dignity_kernel__autonomy_rights_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(dign_be_t20, dignity_kernel__autonomy_rights_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(dign_be_t25, dignity_kernel__autonomy_rights_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignity_kernel__autonomy_rights_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(dign_su_t5, dignity_kernel__autonomy_rights_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(dign_su_t10, dignity_kernel__autonomy_rights_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(dign_su_t15, dignity_kernel__autonomy_rights_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(dign_su_t20, dignity_kernel__autonomy_rights_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(dign_su_t25, dignity_kernel__autonomy_rights_reading, suppression_requirement, 25, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__autonomy_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dignity_kernel__autonomy_rights_reading, 0.18).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__imago_dei_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__posthumanist_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, ai_transparency_mandate).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, algorithmic_labor_regulation).

% DUAL FORMULATION NOTE:
% The dignity kernel has three constraint stories, each instantiating a different reading. The autonomy_rights_reading frames dignity through secular institutions and rationality; it structurally competes with and influences the imago_dei and posthumanist readings by claiming institutional authority over what dignity means. All three are live readings of the same kernel, not competing empirical claims about what dignity is. The network links preserve the constraint family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignity_kernel__autonomy_rights_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
