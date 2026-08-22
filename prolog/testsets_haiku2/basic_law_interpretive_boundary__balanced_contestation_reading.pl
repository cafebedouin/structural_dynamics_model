% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary — Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   Israel's constitutional system has operated since 1992 (the Basic Laws
 *   codification) without a unified written constitution. Instead, courts
 *   (especially the Supreme Court) and the legislature (Knesset) interpret
 *   Basic Laws as quasi-constitutional text, but the Knesset retains
 *   statutory amendment authority. The balanced-contestation reading frames
 *   this as a deliberate institutional boundary: courts interpret within
 *   their domain; legislatures legislate within theirs; both are constrained
 *   by international human rights norms and by the norm of judicial
 *   independence. Neither institution is formally supreme, but neither is
 *   symmetrically positioned either — the court claims interpretive
 *   authority, the Knesset claims amendment authority, and both claim
 *   legitimacy to represent competing constitutional values. This reading
 *   sits between the judicial-supremacy reading (courts are the final
 *   arbiters of constitutional meaning) and the parliamentary-sovereignty
 *   reading (legislatures hold unilateral amendment authority). The
 *   measurement series (1992–2024) captures how this boundary has evolved
 *   from relatively informal coordination (early 1990s) toward more explicit
 *   institutional contestation (2008–2020), with a slight recent relaxation
 *   as legislative pressure on judicial independence has created dialogue
 *   opportunities rather than pure veto dynamics.
 *
 * KEY AGENTS:
 *   - Supreme Court: interprets Basic Laws as constitutional text; claims authority within 'jurisdictional domain' but cannot compel legislative compliance without acquiescence.
 *   - Knesset majority: legislates and amends Basic Laws; faces judicial review veto; constrained by international obligations and judicial independence norms.
 *   - Executive branch: operates under laws vetted by both court and legislature; benefits from constitutional clarity but constrained by both institutions.
 *   - International human rights bodies: monitor and influence Israeli constitutional legitimacy without direct participation in internal negotiation.
 *   - Legislative minorities and public constituencies: access courts as veto mechanism against legislative majorities; trapped in the system but dependent on judicial review for protection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.62).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.48).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary — Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'a8c07bb5-da4c-4067-862a-26724101b4ed').
narrative_ontology:cs_kernel_codification('a8c07bb5-da4c-4067-862a-26724101b4ed', distributed).
narrative_ontology:cs_authority_grounding('a8c07bb5-da4c-4067-862a-26724101b4ed', lineage).
narrative_ontology:cs_interpretation_layer_present('a8c07bb5-da4c-4067-862a-26724101b4ed').
narrative_ontology:cs_reading_relation('a8c07bb5-da4c-4067-862a-26724101b4ed', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a8c07bb5-da4c-4067-862a-26724101b4ed', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a8c07bb5-da4c-4067-862a-26724101b4ed', foundational, mutual_institutional_legitimacy).
narrative_ontology:cs_axiom_status(mutual_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a8c07bb5-da4c-4067-862a-26724101b4ed', mutual_institutional_legitimacy, conventional).
narrative_ontology:cs_axiom('a8c07bb5-da4c-4067-862a-26724101b4ed', foundational, bounded_interpretive_authority).
narrative_ontology:cs_axiom_status(bounded_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('a8c07bb5-da4c-4067-862a-26724101b4ed', bounded_interpretive_authority, deontological).
narrative_ontology:cs_reference_frame('a8c07bb5-da4c-4067-862a-26724101b4ed', institutional_dialogue_framework).
narrative_ontology:cs_drift_state('a8c07bb5-da4c-4067-862a-26724101b4ed', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a8c07bb5-da4c-4067-862a-26724101b4ed', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, institutional_legitimacy_system).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_rule_of_law).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_majority_constituencies).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, rapid_policy_response_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_minority_constituencies).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, public_constituencies).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, public_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws within claimed jurisdictional domain; applies constitutional review to legislation; frames authority as bounded by statutory language and precedent, not sovereign mandate. Justifies interpretive role as enforcing the written constitution against majoritarian override. Depends on legislative acquiescence and international legitimacy for enforcement. Cannot physically compel Knesset compliance without legislative cooperation; institutional identity is constituted through the interpretive function.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, beneficiary).

% Retains statutory authority to legislate and amend Basic Laws; faces judicial review veto over claimed-unconstitutional measures. Constrained by international treaty obligations and international human rights norms; constrained by norm of judicial independence that domestic consensus and international recognition enforce. Can override judicial decisions via supermajority amendment but faces legitimacy cost domestically and reputationally. Bears the cost of negotiating every constitutional boundary; moves are slowed and must be defended against judicial invalidation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset_majority, payer).

% Operates under laws vetted by both court and legislature; gains stability from constitutional clarity and predictable court review, but also bound by judicial interpretations it did not author. Can challenge court decisions through legislative amendment or constitutional revision, but must mobilize the Knesset to do so. Benefits from the constraint's capacity to slow radical policy shifts; bears cost when judicial review blocks executive initiatives.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer).

% Monitor Israeli constitutional practice through treaty compliance mechanisms; influence Knesset and court legitimacy through soft law recommendations and fact-finding. Are not represented in internal constitutional negotiation but shape the reputational environment in which both court and legislature operate. Their framing of judicial independence and rights protection influences what either institution can claim.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% Depend on judicial review to block majority legislation that affects their rights; gain protection when courts invalidate measures they could not defeat in the Knesset. Also vulnerable to majority-supported constitutional amendments that override prior judicial protections. Have no direct seat in either institution but access courts as a veto mechanism against legislative majorities; this access is the constraint's primary recourse for non-majoritarian voices.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_minority_constituencies, beneficiary,
    moderate, biographical, constrained, national).

% Subject to legislation passed under constitutional constraints; benefit from stable constitutional rules that protect core rights against simple-majority override. Also bear the cost of slower policy implementation (court review delays legislation) and periodic invalidation of measures they may have supported. Cannot exit the constitutional system; their only recourse is electoral pressure on the Knesset or mobilization of court access via litigation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, public_constituencies, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, public_constituencies, payer).

% Analyzes the boundary dynamics between courts and legislatures in comparative context; documents how different constitutional systems resolve or fail to resolve interpretive disputes. Provides external reference frame for evaluating whether the balanced-contestation reading accurately describes the structural relationship.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, comparative_constitutional_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action under a written constitution by dividing interpretive authority: courts provide authoritative interpretation of constitutional text within their jurisdictional domain; legislature retains ultimate amendment authority but constrained by norms of stare decisis and judicial independence. Solves the problem of constitutional change without institutional collapse — neither court nor legislature holds unilateral veto; both must negotiate.
% TRANSFER_FUNCTION: Moves decision-making authority from legislative majorities to courts over defined constitutional questions, slowing policy implementation on contested issues and shifting veto power to judicial interpretation. Transfers legitimacy from electoral mandate to constitutional fidelity as the basis for rejecting legislation. Transfers reputational cost from legislature (when it loses) to court (when it decides against popular measures).
% ABSENT_VOICES: Direct stakeholders absent from this reading: legislatures of other democracies whose systems resolve this boundary differently (presidential systems, civil law traditions); non-elected minorities who depend on court access but are not institutionally represented; future legislatures constrained by prior constitutional settlement their constituents did not ratify; international bodies that shape the legitimacy environment but do not participate in internal negotiation.
% DISAPPEARANCE_RATIONALE: If the balanced-contestation boundary dissolved (court loses review power or gains supremacy, legislature gains unilateral amendment authority), the constitutional system would reorganize: either toward pure parliamentary sovereignty (court authority becomes purely advisory), judicial supremacy (Knesset authority over interpretation becomes derivative), or constitutional breakdown. The institutional dialogue that defines this reading would vanish; one institution's reading would stabilize or conflict would escalate to extra-constitutional resolution.
% FOUNDING_PROBLEM: A written constitution requires authoritative interpretation, but interpretation authority must be distributed among elected and unelected institutions such that neither monopolizes constitutional change and neither loses legitimacy to constitutional enforcement. The founding problem is the simultaneous need for constitutional stability (courts enforce text) and democratic accountability (legislatures represent constituents).
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars and comparative constitutionalists outside the Israeli system attest the problem is live and substantive. The founding problem is also attested internally: Supreme Court opinions cite constitutional stability as justification for review authority; Knesset majority statements claim democratic legitimacy as the basis for amendment prerogative. Independent judicial commissions and international human rights bodies confirm that judicial independence norms remain contested globally and that the Israeli case exemplifies the tension.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the constraint imposes costs on legislative majorities seeking rapid policy implementation and on public constituencies wanting simple-majority rule. Suppression is moderate (0.48) because the boundary is enforced primarily through institutional norm-following and international legitimacy rather than coercive machinery — both court and legislature comply with the boundary through appeals to legitimacy, not force. Theater ratio is moderate (0.41 at interval end, rising through 2020 then declining slightly in 2024) because increasingly the institutions engage in performative constitutional debate while the underlying boundary dynamics persist — speeches about judicial independence and parliamentary sovereignty have grown more elaborate while the actual institutional distribution of authority has remained relatively stable. The claim is tangled_rope because both coordination and extraction are present: genuine coordination function (neither court nor legislature can unilaterally dominate; constitutional stability requires both) and asymmetric extraction (legislatures bear the cost of veto through delayed implementation; minorities gain veto access; majorities lose policy velocity). The measurement trajectory shows rising extractiveness and theater through 2020 (peak institutional conflict), followed by slight decline in 2024 as exhaustion and negotiation partially replaced veto dynamics. All measurements are authored on a single shared time grid (1992, 2000, 2008, 2016, 2020, 2024) so the engine samples all three metrics at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the Supreme Court's seat, this boundary is genuine constitutional coordination — the court interprets, the legislature legislates, and both are constrained by constitutional principles. This reading emphasizes the coordination function and legitimate authority distribution. From the Knesset majority's seat, this boundary is institutional overreach disguised as coordination — the court uses constitutional interpretation to substitute its values for legislative judgment, slowing implementation and extracting political cost from elected majorities. This reading emphasizes extraction and asymmetric institutional power. From the international observer's seat, both readings are partially correct: the boundary does distribute authority genuinely AND does extract from majoritarians while protecting minorities. The engine computes a per-seat classification that captures this divergence — the court's seat may compute as rope (pure coordination from that viewpoint), while the legislature's seat computes as tangled_rope or snare (extraction from that viewpoint). This story authors the global view that sits between these perspectives: the balanced-contestation reading asserts that both institutions hold legitimate authority and that neither can claim full supremacy, which is neither the court's claim nor the legislature's claim, but which may be the functional equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Supreme Court: beneficiary (collects interpretive authority and legitimacy from constitutional role) and agenda-setter (sets the terms of boundary contestation); directionality near 0.3 (benefits from the constraint, identity-locked to it). Knesset majority: payer (bears costs of veto and delayed implementation) and agenda-setter (retains amendment authority but constrained); directionality near 0.7 (extracted from, though not fully targetted — can override via supermajority). Executive: mixed (benefits from constitutional stability, constrained by both court and legislature); directionality near 0.5 (symmetric). Legislative minorities: beneficiary (gain court veto access against majorities) but payer (trapped in the system, no exit); directionality near 0.4 (benefits more than harmed). Public constituencies: beneficiary (protected by constitutional constraints against majoritarian override) and payer (policies slowed by review process); directionality near 0.5 (symmetric). The engine derives d from this structural account; the authored metrics describe what the constraint looks like when these directionalities are active.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to distribute constitutional authority between elected and unelected institutions without either monopolizing constitutional change) remains live. The legislative majority has not abandoned the quest for simple-majority amendment authority; the court has not abandoned interpretation authority. International human rights norms remain active constraints on both. The constraint persists because both institutions have institutional incentives to maintain it, and because international legitimacy depends on it. However, the theater ratio has risen significantly (from 0.28 to 0.41), suggesting that performative constitutional debate has increased as a share of actual institutional activity. The slight decline in 2024 may indicate either a temporary relaxation of tension or a shift toward negotiation rather than veto. Mandatrophy is not declared because the founding problem is live; the constraint is not a zombie. However, the rising theater ratio suggests that institutional actors are increasingly defending the boundary through speech rather than structural activity, which is a warning signal for either renewed conflict or eventual delegitimation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_coexistence,
    'Are the three sibling readings (judicial_supremacy, parliamentary_sovereignty, balanced_contestation) genuinely coexisting as live positions held by different institutional and scholarly factions, or does one reading structurally foreclose the others within a single coherent constitutional framework?',
    'Analysis of Knesset legislative history, Supreme Court opinion patterns, and constitutional scholarship to determine whether each reading can be held without logical contradiction by actors within the same system, or whether adoption of one reading''s core premise necessarily excludes another''s.',
    'If truly coexisting, this reading''s claim to describe institutional dialogue is supported; if one reading forecloses others, the characterization as ''balanced contestation'' may misrepresent hierarchical or asymptotic dominance. The omegas routing will shift from coexists_with to forecloses or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether the three sibling readings can coexist as live positions or whether one logically forecloses another.').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the measured extractiveness (0.62) the price of genuine constitutional coordination (courts and legislature both needed to prevent unilateral dominance), or does it reflect asymmetric institutional capture where one actor uses the boundary structure to extract from the other?',
    'Comparative analysis: examining how different constitutional democracies distribute interpretive authority and what extractiveness levels emerge; studying whether the Israeli system''s extractiveness changes if amendment supermajority requirement shifts; evaluating whether court review predominantly blocks measures affecting minorities or majoritarian constituencies.',
    'High extractiveness explained by coordination cost supports the tangled_rope classification; extractiveness explained by institutional capture would suggest snare dynamics within the rope frame. This distinction determines whether the constraint is genuinely bidirectional or whether one seat extracts from the other using legitimacy cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether extractiveness reflects coordination cost or institutional capture.').

omega_variable(
    international_constraint_internalization,
    'To what extent do international human rights norms and treaty obligations function as an external constraint on both court and legislature, versus representing a captured narrative used by one institutional actor to legitimize its interpretive authority?',
    'Tracing the causal path of international norm influence: do both court and legislature cite international precedent independently, or does one use international citations strategically to support its institutional position? Examining whether international pressure functions equally on both institutions or privileges the court''s legitimacy narrative.',
    'If international norms genuinely constrain both institutions symmetrically, they support the balanced-contestation reading; if they function asymmetrically, they undermine the balanced framing and suggest one institution leverages international legitimacy against the other. This affects whether suppression_requirement reflects genuine enforcement cost or performative international positioning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_constraint_internalization, empirical, 'Whether international constraints operate symmetrically on both institutions or asymmetrically privilege one institutional reading.').

omega_variable(
    amendment_supermajority_as_veto_or_coordination,
    'Does the supermajority requirement for Basic Law amendment function as a genuine coordination constraint (slowing change to ensure broad consensus) or as a veto mechanism disguised as coordination (enabling a legislative minority to block majority-backed constitutional change)?',
    'Historical analysis of amendment attempts: measuring how often supermajority requirements have prevented constitutional change that would have passed at simple majority, and whether those blocked changes were contested on constitutional grounds or partisan grounds. Comparing to systems with different amendment thresholds.',
    'If supermajority requirements genuinely coordinate (building consensus), extractiveness is justified as coordination cost; if they function as hidden vetoes, the extraction is asymmetric — the majority bears cost but cannot unilaterally override. This determination affects whether the constraint should be classified as rope (symmetric coordination) or snare (asymmetric extraction with legitimacy cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_supermajority_as_veto_or_coordination, conceptual, 'Whether supermajority requirements function as coordination mechanisms or hidden vetoes.').

omega_variable(
    balanced_reading_institutional_identity_fusion,
    'Are the Supreme Court and Knesset genuinely distinct institutions with separate institutional identities, or has the balanced-contestation reading fused their identity around a shared ''constitutional system'' narrative such that each institution''s authority is defined only in relation to the other, creating an institutional mutual-hostage dynamic rather than genuine contestation?',
    'Examining whether either institution can coherently articulate its authority independently of references to the other institution''s legitimate role. Studying whether institutional policy is driven by the constraint''s structure or by it. Analyzing identity_locked exit dynamics: could each institution exit this particular boundary arrangement without ceasing to be that institution?',
    'If institutions are identity-locked to the balanced-contestation frame, their exit options are genuinely constrained to identity_locked status; the arrangement becomes self-perpetuating through institutional self-definition rather than external enforcement. This would shift the classification from tangled_rope toward piton dynamics (performative maintenance of a boundary that neither can coherently abandon).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_reading_institutional_identity_fusion, conceptual, 'Whether institutional identities are fused to the balanced-contestation boundary or remain analytically distinct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 1992, 0.28).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(basi_tr_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2008, 0.37).
narrative_ontology:measurement(basi_tr_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2016, 0.42).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2020, 0.46).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(basi_be_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 1992, 0.45).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement(basi_be_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(basi_be_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1992, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(basi_su_t2008, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2008, 0.44).
narrative_ontology:measurement(basi_su_t2016, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2016, 0.5).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__balanced_contestation_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The basic_law_interpretive_boundary kernel decomposes into three structurally distinct constraint stories, one per reading. The balanced_contestation_reading (this story) emphasizes institutional dialogue and symmetrical authority distribution; the judicial_supremacy_reading emphasizes court primacy; the parliamentary_sovereignty_reading emphasizes legislative primacy. All three share the same referent (the Basic Laws interpretive boundary) but differ in ε, beneficiary structure, and institutional configuration. Each story is linked via network.affects_constraints to the other two; they form a constraint family unified by kernel identity but differentiated by reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_boundary__balanced_contestation_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
