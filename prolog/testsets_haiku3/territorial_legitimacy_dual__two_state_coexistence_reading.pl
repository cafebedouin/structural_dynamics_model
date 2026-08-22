% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Framework: Dual Legitimacy Recognition with 1967 Boundaries
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the two-state coexistence reading of the
 *   territorial-legitimacy-dual kernel — a contested political commitment to
 *   recognize both Jewish and Palestinian peoples' legitimacy within
 *   partitioned territory (1967 lines as basis). The reading accepts the
 *   premise that BOTH 1948 Israeli statehood AND Palestinian right to
 *   statehood are legitimate, and that mutual recognition with territorial
 *   limitation is the path to coexistence rather than perpetual zero-sum
 *   competition. The claim (tangled_rope) reflects the structure: real
 *   coordination function (enabling self-determination for both), but active
 *   enforcement required to suppress maximalist alternatives from both sides,
 *   with asymmetric extraction from refugees and ideological hardliners. The
 *   metrics show stable moderate extractiveness (0.38 at interval end),
 *   rising theater ratio (0.41), and increasing suppression requirement
 *   (0.52), indicating that enforcement machinery must actively defend the
 *   compromise against drift toward territorial revisionism on both sides.
 *
 * KEY AGENTS:
 *   - Israeli state institutional (powerful institutional actor, beneficiary-agenda-setter, territorial security)
 *   - Palestinian state institutional (moderate institutional actor, beneficiary-agenda-setter, self-determination)
 *   - Palestinian refugees external (powerless, trapped, bearing displacement cost)
 *   - Israeli security hawks (powerful, identity-locked, paying through accepting Palestinian statehood)
 *   - Palestinian irredentist movements (moderate, identity-locked, paying through accepting Jewish state)
 *   - International peace guarantors (institutional analytical seat, enforcer)
 *   - Regional authoritarian regimes (excluded, historically wielded Palestinian proxy power)
 *   - Diaspora Jewish communities (organized beneficiary, normalization support)
 *   - Palestinian diaspora solidarity networks (excluded, maximalist alternative)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.38).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.52).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework: Dual Legitimacy Recognition with 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '3f19cc83-6c26-41d7-9672-2480734993d9').
narrative_ontology:cs_kernel_codification('3f19cc83-6c26-41d7-9672-2480734993d9', fixed_text).
narrative_ontology:cs_authority_grounding('3f19cc83-6c26-41d7-9672-2480734993d9', extraction).
narrative_ontology:cs_interpretation_layer_present('3f19cc83-6c26-41d7-9672-2480734993d9').
narrative_ontology:cs_reading_relation('3f19cc83-6c26-41d7-9672-2480734993d9', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f19cc83-6c26-41d7-9672-2480734993d9', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_axiom('3f19cc83-6c26-41d7-9672-2480734993d9', foundational, dual_legitimacy_mutual_recognition).
narrative_ontology:cs_axiom_status(dual_legitimacy_mutual_recognition, holdable).
narrative_ontology:cs_axiom_grounding('3f19cc83-6c26-41d7-9672-2480734993d9', dual_legitimacy_mutual_recognition, deontological).
narrative_ontology:cs_axiom('3f19cc83-6c26-41d7-9672-2480734993d9', foundational, territorial_partition_as_coexistence_mechanism).
narrative_ontology:cs_axiom_status(territorial_partition_as_coexistence_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3f19cc83-6c26-41d7-9672-2480734993d9', territorial_partition_as_coexistence_mechanism, instrumental).
narrative_ontology:cs_axiom('3f19cc83-6c26-41d7-9672-2480734993d9', secondary, id_1967_boundaries_as_legitimate_partition_basis).
narrative_ontology:cs_axiom_status(id_1967_boundaries_as_legitimate_partition_basis, holdable).
narrative_ontology:cs_axiom_grounding('3f19cc83-6c26-41d7-9672-2480734993d9', id_1967_boundaries_as_legitimate_partition_basis, conventional).
narrative_ontology:cs_reference_frame('3f19cc83-6c26-41d7-9672-2480734993d9', post_1967_partition_negotiation_framework).
narrative_ontology:cs_drift_state('3f19cc83-6c26-41d7-9672-2480734993d9', contemporary_territorial_dispute_escalation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f19cc83-6c26-41d7-9672-2480734993d9', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutional).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_institutional).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_external).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_hawks).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_irredentist_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepts Palestinian statehood within 1967 boundaries in exchange for international recognition, security cooperation, and settlement of refugee claims outside Israel proper. Administers the security framework and settlement within agreed borders. Benefits from legitimacy, territorial certainty, and normalized regional integration; pays through territorial concessions and refugee-claims limitations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutional, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_state_institutional, beneficiary).

% Accepts Israeli statehood and 1967 boundaries as partition basis in exchange for Palestinian sovereignty, refugee resettlement in Palestinian state, and international recognition. Administers Palestinian territory and coordinates with Israel on security. Benefits from statehood and self-determination; pays through accepting Jewish state's legitimacy and limiting refugee return to Palestinian state only.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_institutional, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_state_institutional, beneficiary).

% Diaspora populations (primarily in Lebanon, Syria, Jordan camps) who fled or were expelled in 1948 and subsequent wars. Under this reading, right of return is recognized but channeled exclusively to Palestinian state territory, not to 1948 home villages in Israel proper. They bear the cost of permanent displacement or resettlement outside original homes; exit is trapped by host-country legal status and Palestinian state capacity limitations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_external, payer,
    powerless, biographical, trapped, global).

% Israeli constituencies (settlement movements, security establishment hardliners, ideological nationalists) who view territorial concessions and Palestinian statehood as security threats. They pay through accepting permanent Palestinian sovereignty, demilitarization agreements, and security interdependence rather than unilateral control. Identity-locked by security ideology and settlement commitment.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_hawks, payer,
    powerful, biographical, identity_locked, regional).

% Palestinian political factions and movements (historically including nationalist and Islamist currents) committed to original territorial maximalism and unlimited right of return. They pay through accepting Jewish state legitimacy, territorial partition, and return limitations. Identity-locked by liberation narratives and historical grievance.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_irredentist_movements, payer,
    moderate, biographical, identity_locked, regional).

% Neighboring state governments (Syria, Jordan, Lebanon historically) that have mediated or hosted Palestinian refugees and factional organizations. They are excluded from the two-state framework's core partition logic — their interests in Palestinian movements, refugee populations, and regional balance are not formal seats in the coexistence arrangement. Their exclusion structures the constraint's incompleteness.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_authoritarian_regimes, excluded,
    powerful, biographical, mobile, regional).

% UN bodies, international law frameworks, and external mediating powers (US, EU, regional diplomats) that would enforce or witness the coexistence arrangement. They administer neutrality, arbitrate disputes, and underwrite security commitments. Analytical seat with institutional power to activate or suspend enforcement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_peace_guarantors, agenda_setter,
    institutional, generational, analytical, global).

% Jewish communities outside Israel who provide political, financial, and cultural support to the Israeli state. They benefit from normalized Jewish statehood, regional stability, and the legitimacy this reading confers on Zionism as national movement. They do not directly pay, though some ideological constituencies view territorial concessions as delegitimizing the refuge mandate.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Transnational Palestinian solidarity movements (BDS networks, diaspora nationalist organizations) committed to maximalist territorial claims and right of return without limitation. They are excluded from the coexistence framework's core compromise — their voice opposes the framework's foundational premise of mutual recognition and limited return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_diaspora_solidarity_networks, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a zero-sum territorial competition by establishing mutual recognition, clear borders (1967 lines), and separate but coordinated governance. Enables both peoples to exercise self-determination within defined territory, replacing perpetual conflict over legitimacy and boundaries with institutionalized coexistence and security cooperation.
% TRANSFER_FUNCTION: Moves territorial control from contested/overlapping claims to a partition; transfers refugee return rights from Israel-proper to Palestinian state only; transfers security responsibility from zero-sum military dominance to cooperative security architecture. Extracts from those committed to territorial maximalism and unlimited return (Israeli hawks, Palestinian irredentists) and from refugees whose return is geographically limited.
% ABSENT_VOICES: Palestinian refugees in diaspora camps (powerless, excluded by host states from decision-making); Palestinian maximalist movements and Israeli settlement ideologies (excluded because their core claims are foreclosed by the framework itself); neighboring authoritarian regimes whose refugee and factional interests are unrepresented in partition logic; regional actors who historically wielded Palestinian proxy power.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the territorial dispute would revert to zero-sum competition: Israeli claims to historical Zion and security buffers, Palestinian claims to 1948 displacement and right of return without territorial limit, renewed cycles of conflict over Jerusalem, settlements, and refugee status. The international order anchored on this or similar partitions would fragment; mediating institutions would lose their mandate.
% FOUNDING_PROBLEM: After 1948 independence and subsequent wars, both peoples held irreducible legitimacy claims to the same territory: Jewish historical presence and persecution-driven refuge, Palestinian continuous habitation and colonial-era displacement. Neither could be erased without genocide; both required institutional recognition. The founding problem: how to acknowledge BOTH legitimacies while enabling coexistence rather than perpetual zero-sum conflict.
% FOUNDING_PROBLEM_CORROBORATION: International peace frameworks (Oslo Accords, Road Map, UN resolutions) from outside both peoples' advocacy have endorsed the two-state framing as the operative solution. Academic international-relations scholarship on partition and ethno-territorial conflict recognizes the dual-legitimacy problem as the central structural issue. Palestinian and Israeli constituencies committed to the framework attest the founding problem remains live (coexistence still contested); maximalist movements and settlement ideologies from BOTH sides attest the problem is 'solved' only by accepting mutual constraint, which they reject — their disagreement is about the problem's *necessity*, not its existence.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).
:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint coordinates genuine self-determination for both institutional actors while extracting from refugees (who bear displaced-return limitation) and from ideological maximalists who must accept mutual constraint. The extraction is neither comprehensive (coordination is real) nor negligible (displacement costs are borne by powerless agents). Suppression is higher (0.52) than extraction because the constraint must actively exclude maximalist alternatives from both sides — settlement ideologies, irredentist movements, and refugee claims for unlimited return. Theater ratio rises over the interval (0.25→0.41), indicating that enforcing the compromise increasingly requires performative displays of mutual commitment and security theater, as natural incentive alignment weakens and ideology drift creates pressure toward revisionism. Accessibility collapse (0.48) reflects contested status: for institutional actors committed to the framework, alternatives have partly collapsed (partition is now 'natural'); for maximalists, alternatives remain vivid and active (resistance=0.74). The one shared time grid ensures every metric is authored at every examined point, enabling proper temporal analysis of the constraint's lifecycle.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian state institutional seat, this reading appears as genuine coexistence coordinate: self-determination, territorial sovereignty, refugee resettlement in Palestinian territory, international recognition. From the Palestinian refugee diaspora seat, it appears as extraction: permanent displacement, return to Palestinian state (often thousands of km away from original homes), no right to reclaim 1948 properties. From the Israeli security-hawk seat, it appears extractive (territorial concessions, permanent Palestinian state, security interdependence). From the Israeli institutional seat, it appears as beneficial coordination (legitimacy, territorial certainty, regional integration). The engine computes these per-seat classifications from the structural data — the divergence is not a measurement error but the central evidence the corpus exists to document.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutional and Palestinian state institutional both occupy agenda-setter + beneficiary dual roles, but with asymmetric directionality. Israeli institutional: d ≈ 0.35 (moderate beneficiary — gains legitimacy and territory certainty, but constrained by security interdependence and settlement limitations). Palestinian institutional: d ≈ 0.45 (closer to symmetric — gains statehood but constrained by limited territory and external security dependence on international guarantors). Palestinian refugees external: d ≈ 0.85 (target — bear displacement cost, trapped exit, return limited to Palestinian state only). Israeli security hawks: d ≈ 0.70 (target — constrained by accepting permanent Palestinian sovereignty and security interdependence, identity-locked by security ideology). Palestinian irredentist movements: d ≈ 0.75 (target — constrained by accepting Jewish state legitimacy and territorial partition, identity-locked by liberation narratives). The divergence between institutional beneficiary seats and maximalist payer seats is structural: the framework requires extracting constraint acceptance from those committed to territorial maximalism. International guarantors sit at d ≈ 0.5 (symmetric analytical role — they coordinate but extract no direct gain).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate is mutual recognition and coexistence; the constraint is NOT degraded. However, the founding-problem mismatch is high: the founding problem (dual legitimate claims requiring institutional coexistence) remains contested in status. The constraint persists because international law, mediation frameworks, and both institutional actors' survival interests anchor it — not because the underlying ideological dispute is resolved. Mandatrophy does NOT apply (the constraint is not atrophied), but the rising theater ratio (0.25→0.41) indicates increasing performative maintenance as ideological drift pushes toward revisionism. This is not piton-scale degradation, but it is a leading indicator that the constraint may become piton-like if institutional commitment erodes further.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    refugee_return_legitimacy_gap,
    'Is limiting right of return to Palestinian state territory a legitimate recognition of refugee claims, or is it a perpetuation of displacement? Can return outside original homes be genuine restitution, or does it constitute a second displacement?',
    'Palestinian and diaspora refugee communities'' acceptance or rejection of the framework over generations; empirical data on whether resettlement in Palestinian state resolves or perpetuates grievance; comparison with other post-conflict partition/return models (India-Pakistan, Cyprus, Yugoslavia).',
    'If refugees experience resettlement as legitimate restitution, the framework''s extraction from refugees (d≈0.85) may be reinterpreted as enforced but acceptable compromise. If resettlement is experienced as perpetuation, the extraction persists and the framework''s claimed coexistence function deteriorates toward extraction-only (snare trajectory). The classification may shift from tangled_rope toward snare if refugee grievance remains live across generations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_return_legitimacy_gap, preference, 'Legitimacy of limited right of return vs. perpetuation of displacement.').

omega_variable(
    institutional_commitment_durability,
    'Will Israeli and Palestinian institutional actors maintain commitment to the coexistence framework across generational time horizons, or will ideological drift (settlement expansion, irredentist movements) erode the institutional consensus underlying the constraint?',
    'Temporal tracking of institutional actors'' actual behavior (settlement policy, sovereignty claims, security cooperation maintenance); measurement of ideological drift in majority constituencies; comparison with pre-commitment baseline.',
    'If institutional commitment erodes, suppression requirement (currently 0.52) must rise to maintain the constraint against increasing revisionist pressure — theaters of enforcement expand, theater_ratio climbs. If erosion is severe, the constraint may degrade into piton (inert institutional survival without functional coexistence). Conversely, if ideological drift is contained by institutional stability, suppression plateaus and the constraint remains functionally tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_commitment_durability, empirical, 'Institutional stability of coexistence commitment vs. ideological drift pressure.').

omega_variable(
    security_interdependence_feasibility,
    'Can genuine security cooperation replace zero-sum military competition, or is structural mistrust too deep to enable shared security infrastructure? Is security interdependence a path to mutual vulnerability (and thus enforced restraint) or a path to catastrophic failure if trust breaks?',
    'Historical precedent from partition cases with security cooperation requirements (India-Pakistan, Korea, Germany, Cyprus); empirical measurement of trust indicators (joint command structures, shared intelligence, hotline usage); modeling of failure scenarios and escalation paths.',
    'If security interdependence proves feasible, the constraint''s enforcement burden (suppression requirement) can stabilize or decline as mutual vulnerability creates structural incentives for restraint. If infeasible (deep mistrust, repeated breakdown), enforcement burden must rise, theater ratio climbs, and the constraint trends toward piton or collapse. The classification remains tangled_rope only if security cooperation is substantively operational; purely ceremonial cooperation would be piton-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_interdependence_feasibility, empirical, 'Feasibility of security interdependence as enforcer of the coexistence framework.').

omega_variable(
    kernel_reading_committer_frame,
    'Is this reading (two-state coexistence) the expression of genuine compromise that both peoples endorse, or is it an externally imposed framework that each people experiences as constraint on their legitimate claims?',
    'Distinction between internally-generated consensus (both peoples'' indigenous political movements converge on the framework) vs. external mediation (international powers impose the framework as least-bad option). Temporal analysis: did the framework emerge from intra-Palestinian and intra-Israeli deliberation, or from Cold War / post-Cold War mediation structures?',
    'If internally generated, the constraint is a genuine coexistence coordinate held by both parties, with extraction only from ideological maximalists. If externally imposed, it is a constraint ON BOTH peoples'' maximalist claims, with extraction from all territorial populations (not just refugees and hardliners). Classification remains tangled_rope in either case, but the interpretation differs: internal consensus = coordination with extraction; external imposition = imposed partition with dual extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Internal consensus vs. external imposition in the origin of the two-state reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(terr_tr_t25, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(terr_tr_t35, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(terr_tr_t50, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(terr_be_t25, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 25, 0.39).
narrative_ontology:measurement(terr_be_t35, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement(terr_be_t50, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(terr_su_t25, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement(terr_su_t35, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(terr_su_t50, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy_dual kernel decomposes into three constraints, one per reading: zionist_refuge_reading (legitimacy grounded in Jewish persecution and refuge need), two_state_coexistence_reading (mutual recognition with 1967 partition), palestinian_autochthony_reading (legitimacy grounded in continuous habitation and displacement trauma). Each reading has a different ε, different victim/beneficiary sets, different institutional structures. The two-state reading AFFECTS the siblings by establishing a reference frame (1948 statehood as legitimate for both) that changes what each sibling reading must argue against. Zionist_refuge_reading and palestinian_autochthony_reading coexist as live positions in contemporary political discourse; two_state_coexistence influences both by reframing legitimacy as compatible rather than zero-sum.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
