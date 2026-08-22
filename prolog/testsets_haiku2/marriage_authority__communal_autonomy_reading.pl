% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Marriage Authority Grounded in Community Religious Tradition (Communal Autonomy Reading)
 *   domain: legal_pluralism/constitutional_law
 *
 * SUMMARY:
 *   In a pluralist state that recognizes multiple religious traditions,
 *   marriage authority is grounded in community religious law rather than
 *   secular legislation. Religious leadership bodies (elders, councils, high
 *   priests) set formation rules, dissolution procedures, and property rights
 *   according to their faith traditions. The state enforces these orders
 *   through courts but does not author the substantive norms. This
 *   arrangement protects minority religious communities from majoritarian
 *   imposition of uniform marriage law. However, it also traps
 *   intra-community dissenters—especially women seeking divorce on grounds
 *   the religious leadership rejects, and individuals in interfaith
 *   relationships or with non-traditional gender identities—in law they
 *   cannot exit without severing community ties. The constraint is claimed as
 *   ROPE by its proponents (genuine coordination solving the pluralism
 *   problem) while exhibiting extractive features (suppression of dissenters,
 *   asymmetric burdens, identity-locked exit) that the authored metrics
 *   capture. The reading/metric gap is structural: the engine will compute
 *   from the authored data; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - Religious leadership bodies: set and enforce marriage law; retain institutional authority; benefit from cultural autonomy — institutional power, mobile exit, agenda-setter role
 *   - Intra-community dissenters (especially women): trapped by identity-locked exit; bear concentrated costs of religious law interpretation; no recourse to secular courts without losing community standing — powerless, identity-locked, payer role
 *   - Majority religious community: gains cultural autonomy and rule that aligns with values; broadly accepts the arrangement — organized power, mobile exit, beneficiary role
 *   - State legislature: cedes rule-making to religious leadership; maintains enforcement infrastructure; slower to reform; fragments legal landscape — institutional power, mobile exit, secondary agenda-setter role
 *   - Judicial system: enforces religious leadership orders; navigates conflicts; under pressure to impose constitutional floors — observer role, institutional power
 *   - Constitutional equality proponents: structurally excluded from rule-making; advocate for uniform code or judicial override — excluded role, organized power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.58).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.62).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority Grounded in Community Religious Tradition (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '38c3dbea-4858-41aa-8daa-2d233cd1e313').
narrative_ontology:cs_kernel_codification('38c3dbea-4858-41aa-8daa-2d233cd1e313', distributed).
narrative_ontology:cs_authority_grounding('38c3dbea-4858-41aa-8daa-2d233cd1e313', lineage).
narrative_ontology:cs_interpretation_layer_present('38c3dbea-4858-41aa-8daa-2d233cd1e313').
narrative_ontology:cs_reading_relation('38c3dbea-4858-41aa-8daa-2d233cd1e313', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('38c3dbea-4858-41aa-8daa-2d233cd1e313', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('38c3dbea-4858-41aa-8daa-2d233cd1e313', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_reading_relation('38c3dbea-4858-41aa-8daa-2d233cd1e313', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('38c3dbea-4858-41aa-8daa-2d233cd1e313', foundational, community_self_determination_in_family_law).
narrative_ontology:cs_axiom_status(community_self_determination_in_family_law, holdable).
narrative_ontology:cs_axiom_grounding('38c3dbea-4858-41aa-8daa-2d233cd1e313', community_self_determination_in_family_law, deontological).
narrative_ontology:cs_axiom('38c3dbea-4858-41aa-8daa-2d233cd1e313', foundational, religious_leadership_authority_grounded_in_tradition).
narrative_ontology:cs_axiom_status(religious_leadership_authority_grounded_in_tradition, holdable).
narrative_ontology:cs_axiom_grounding('38c3dbea-4858-41aa-8daa-2d233cd1e313', religious_leadership_authority_grounded_in_tradition, conventional).
narrative_ontology:cs_reference_frame('38c3dbea-4858-41aa-8daa-2d233cd1e313', communal_religious_autonomy_framework).
narrative_ontology:cs_drift_state('38c3dbea-4858-41aa-8daa-2d233cd1e313', contemporary_rights_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38c3dbea-4858-41aa-8daa-2d233cd1e313', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership_bodies).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, majority_religious_community).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, state_legislature).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, minority_religious_communities).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_within_communities).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, consociational_democracy_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, cultural_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious elders, councils, and authorized interpreters set marriage formation rules, dissolution procedures, property rights, and inheritance within their communities. They author family law norms grounded in sacred texts and tradition. The state enforces their orders through courts but does not write the substantive law. They retain authority to refuse remarriage, annul unions, and impose moral sanctions. This authority is the institutional foundation of their religious leadership.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership_bodies, agenda_setter,
    institutional, generational, mobile, national).

% Delegates marriage authority to religious communities rather than legislating a uniform civil code. The state retains enforcement machinery (courts, registrars) but has ceded rule-making. Amending personal law requires community consent, which slows legislative reform and fragments the legal landscape. The state pays the cost of maintaining parallel adjudication structures and the inability to impose uniform standards across communities.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_legislature, payer,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, state_legislature, agenda_setter).

% Community members who contest religious leadership's interpretation of marriage law (women seeking divorce on grounds the leadership rejects, individuals marrying outside the faith, LGBTQ+ members seeking recognition, those rejecting arranged marriage norms). They are bound by religious law they did not consent to, cannot appeal outside the community without losing identity and social standing, and have no exit to secular law without severing community ties. Exit is identity-fusing: leaving the community means leaving the religion.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, national).

% The numerically dominant religion in a pluralist state gains institutional authority to govern marriage within its boundaries. Members broadly accept the rules as legitimate (either through genuine agreement or tradition-based compliance). The community benefits from cultural autonomy and the authority's rules align with their values and practices.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, majority_religious_community, beneficiary,
    organized, generational, mobile, national).

% Smaller faith traditions gain protection of their marriage practices from majoritarian imposition. They can maintain endogamous rules, honor familial authority structures, and transmit tradition. They also shoulder the burden of maintaining parallel legal infrastructure and face the risk that majoritarian values will be imposed through constitutional reinterpretation if the consensus supporting pluralism erodes.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, minority_religious_communities, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, minority_religious_communities, payer).

% Courts enforce religious leadership's decisions and register marriages according to community norms, but do not author or review the substantive rules. They navigate conflicting religious laws (when parties have dual heritage or convert) and increasing pressure to impose constitutional floors (gender equality, due process). Judicial role sits between enforcement of delegated authority and emerging demands for rights protection.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, judicial_system, observer,
    institutional, generational, analytical, national).

% Advocates and civil-rights organizations who argue that personal law pluralism violates constitutional equality guarantees. They would push for a uniform civil code or judicial override of discriminatory religious practices. Their voice is formally excluded from rule-making within the personal law system, though they can litigate and lobby legislatively.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_equality_proponents, excluded,
    organized, generational, constrained, national).

% Bear concentrated costs if religious law restricts divorce, requires male consent for remarriage, limits property rights, or enforces guardianship norms. They lack exit because leaving means losing family, community standing, and often economic security. The constraint falls asymmetrically on them because religious law often embeds gender hierarchy. Escape routes (secular courts, legal aid) exist in theory but are closed by identity and economic dependency.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_within_communities, payer,
    powerless, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_leadership_bodies).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables religious communities to maintain internal marriage governance, transmit faith-based family norms across generations, and preserve cultural autonomy without state interference. Solves the coordination problem of how a pluralist state recognizes diverse family structures without imposing one majoritarian standard.
% TRANSFER_FUNCTION: Transfers rule-making authority from the democratic legislature to religious leadership bodies. Transfers the cost of fragmented legal administration to the state (maintaining parallel courts, registries, and adjudication structures). Transfers exit costs onto intra-community dissenters, who lose legal recourse and community status if they seek secular divorce or marry outside the faith.
% ABSENT_VOICES: Intra-community dissenters (especially women in restrictive regimes) are structurally excluded from rule-making within the personal law system. Constitutional equality advocates are excluded from the personal law negotiation table. Individuals from interfaith couples and LGBTQ+ members are absent from deliberations about whose norms get recognized.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight—if religious leadership authority were revoked and marriage law were unified under secular code—religious communities would lose the institutional autonomy they depend on for cultural transmission. The state would gain enforcement simplicity and the ability to impose uniform standards. Intra-community dissenters would gain legal access to secular divorce and equality protections, though many would lose community standing. Religious leadership would retain moral authority but lose legal power.
% FOUNDING_PROBLEM: How can a pluralist state recognize diverse family structures and faith traditions without imposing majoritarian norms that violate minority conscience and religious practice?
% FOUNDING_PROBLEM_CORROBORATION: Religious leadership and minority-rights advocates attest the founding problem is live: majorities would impose uniform marriage rules if given unchecked power, violating minority conscience. Constitutional equality advocates and gender-rights advocates attest the founding problem is poorly solved: personal law pluralism sacrifices equality guarantees to avoid majoritarian imposition. Judicial and legislative observers from multiple democracies (India, Canada, UK) confirm the problem persists in practice, though they disagree on solutions.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.58 endpoint) reflects genuine coordination (avoiding majoritarian imposition) layered with asymmetric burden-bearing (dissenters exit-trapped, women concentrated as victims). Suppression is moderately high (0.62) because the constraint depends on maintaining intra-community compliance through social and legal mechanisms—dissenters cannot appeal outside without losing identity. Theater is substantial (0.41): religious leadership legitimates its authority partly through genuine tradition and partly through performative invocation of sacred sources and immemorial practice. The measurement series (t=0 to t=25) shows slow extraction creep and theater rise as constitutional pressure from equality advocates mounts—the constraint must increasingly perform its cultural-autonomy function as its founding problem (majoritarian threat) fades. The mid-series spike (t=20, projected) reflects a hypothetical moment of constitutional pressure; the subsequent decline (t=25) reflects partial stabilization as the constraint adapts. All metrics are authored on one shared time grid (every metric at every time point) to avoid temporal misalignment that would fabricate type transitions.
 *
 * PERSPECTIVAL GAP:
 *   The religious leadership and majority community seats should compute as beneficiaries experiencing rope-like coordination; the intra-community dissenters and women seats should compute as targets experiencing snare-like extraction. The seat divergence arises from power asymmetry, exit structure, and whether the measured extraction serves beneficiary interests (leadership: yes) or imposes costs (dissenters: yes). The state legislature occupies an intermediate position: it benefits from simplicity if pluralism persists (no pressure to legislate), but pays the cost of fragmented legal administration and constitutional vulnerability.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership: directionality low (d ≈ 0.2–0.3), beneficiary, collects authority, constrained exit, institutional power. They benefit from the arrangement directly through rule-making authority and do not wish to exit. Intra-community dissenters: directionality high (d ≈ 0.7–0.8), victims, identity-locked, powerless, concentrated costs. They bear the constraint's extraction and cannot exit without severing identity. Majority community: directionality mid-low (d ≈ 0.35–0.45), beneficiary role, organized power, mobile exit; they benefit broadly from cultural autonomy but face diffuse costs (legal complexity, vulnerability to constitutional challenge). State legislature: directionality mid (d ≈ 0.45–0.55), secondary agenda-setter, institutional power, mobile exit; they benefit from avoiding majoritarian backlash if they tried to impose uniform code, but pay the cost of fragmented administration.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—majoritarian imposition on minorities—remains contested. If it is live, personal law pluralism serves real coordination: the rope classification holds. If it is dead (constitutional protections make pluralism permanent and majorities no longer threaten), the arrangement persists by inertia despite its extraction costs—a piton. The theater-ratio creep (0.32 to 0.41) suggests the constraint is increasingly performing its coordination function (invoking cultural autonomy) rather than delivering it (actually protecting minorities from majoritarian pressure). This fits the mandatrophy pattern: the founding purpose may have degraded while institutional machinery persists. The gender-equity gap (women as concentrated victims despite formal equality law) further suggests the constraint has become partly theater—it performs cultural protection while selectively extracting from dissenters. An omega addresses whether the founding problem is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_identity_lock,
    'Do intra-community dissenters genuinely consent to religious law by remaining in the community, or is their assent coerced by identity-fusion and exit barriers?',
    'Post-exit surveys and longitudinal studies tracking individuals who leave communities and whether they report relief or regret; analysis of exit-cost barriers (economic, relational, legal) distinguishing constraint from choice.',
    'If consent is genuine, the constraint is a true rope serving the community''s coordination interests. If identity-locked exit means coerced assent, the constraint is partly snare-like for that subset—the classification would shift toward tangled_rope with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_identity_lock, empirical, 'Whether intra-community dissenters'' acceptance of religious law reflects genuine consent or identity-locked coercion.').

omega_variable(
    religious_leadership_extraction,
    'Do religious leadership bodies use marriage authority primarily to serve community coordination and cultural transmission, or do they extract economic and political power from it?',
    'Analysis of leadership compensation, property control over marriage-related assets (bride price, dower, inheritance), disciplinary power (excommunication, social ostracism) used as leverage, and whether authority is challenged from within the community.',
    'If extraction is minimal and authority is genuinely accepted, the constraint is rope-like for the majority community. If leadership captures substantial rents or uses marriage authority to consolidate political power, the constraint becomes tangled_rope with leadership as clear beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_leadership_extraction, empirical, 'Whether religious leadership uses marriage authority to extract economic or political power from communities.').

omega_variable(
    alternative_readings_foreclosure,
    'Is the communal autonomy reading logically compatible with the gender-rights reading, or do they foreclose each other within a single framework?',
    'Can a framework simultaneously honor both community self-determination AND guarantee intra-community gender equality? If yes, the readings coexist; if no, one forecloses the other.',
    'If foreclosure exists, the two readings represent incompatible constitutional visions and one must eventually dominate. If they coexist, both can remain live in different jurisdictions or at different evolutionary moments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Whether communal autonomy and gender rights readings can coexist in one constitutional framework or if one logically forecloses the other.').

omega_variable(
    state_enforcement_role_ambiguity,
    'When the state enforces religious leadership''s marriage orders, how much implicit state endorsement and coercive power flows into those orders?',
    'Comparative analysis of jurisdictions: do state courts modify, refuse, or overturn religious marriage orders? Are there constitutional limits on what orders the state will enforce? Do dissenters perceive state power as neutral enforcement or as state backing for religious authority?',
    'If state enforcement is neutral and limited, the constraint remains communal-autonomy-focused rope. If state power substantially amplifies religious authority''s coercive reach, the constraint slides toward snare-like extraction masquerading as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_role_ambiguity, empirical, 'Whether state enforcement of religious marriage orders remains neutral or becomes implicit state endorsement of religious authority.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem—majoritarian imposition on minorities—still live, or has constitutional equality law made it substantially moot?',
    'Review of actual majoritarian pressure on minorities in the jurisdiction; evidence of whether majorities currently attempt to impose uniform marriage law and whether minorities still need pluralism to resist it.',
    'If the problem is live, personal law pluralism serves a real coordination need. If it is dead—majorities have no appetite for uniform code and constitutional protections make pluralism permanent—the arrangement persists by inertia (piton-like), not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem of majoritarian imposition remains active or has been superseded by constitutional protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_authority__communal_autonomy_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t15, marriage_authority__communal_autonomy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(marr_tr_t15, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(marr_tr_t20, projected).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__communal_autonomy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(marr_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_authority__communal_autonomy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t15, marriage_authority__communal_autonomy_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(marr_be_t15, observed).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(marr_be_t20, projected).
narrative_ontology:measurement(marr_be_t25, marriage_authority__communal_autonomy_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(marr_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_authority__communal_autonomy_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t15, marriage_authority__communal_autonomy_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(marr_su_t15, observed).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(marr_su_t20, projected).
narrative_ontology:measurement(marr_su_t25, marriage_authority__communal_autonomy_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(marr_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the MARRIAGE AUTHORITY kernel family (5 readings: communal_autonomy, gender_rights, secularist, federalist_millet, judicial_harmonization). Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and type claims. They are linked through network.affects_constraints to enable cross-reading contamination analysis. The communal autonomy reading here emphasizes cultural self-determination and consociational protection of minorities; the gender-rights reading contests it on grounds of intra-community equality; the secularist reading treats personal law as a transitional anomaly; the federalist-millet reading reframes pluralism as anti-majoritarian for all minorities; the judicial-harmonization reading proposes evolutionary constitutional-floor enforcement. Decomposition rationale: ε_communal (0.58) models genuine coordination (protecting minorities from majoritarian imposition) plus asymmetric burden-bearing (dissenters pay); ε_gender-rights would be higher (~0.72+) because the gender-rights reading emphasizes the extraction side and treats cultural autonomy as cover for gender inequality; ε_secularist would be lower (~0.35) because the secularist reading treats pluralism as a transitional coordination problem soon to be solved by uniform code; ε_judicial-harmonization would be mid-range (~0.55) because it emphasizes gradual constitutional floor-imposition without abolishing pluralism. Each reading has different beneficiaries (communal_autonomy: religious leadership + majority community; gender_rights: women and dissenters as implicit beneficiaries of equality-focused reform; secularist: future unified citizenry under UCC; federalist: all minorities as institutional protection beneficiaries). The constraint family is fully specifiable: each story carries ε fixed to that reading's premises, with no ε-shifting across observable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
