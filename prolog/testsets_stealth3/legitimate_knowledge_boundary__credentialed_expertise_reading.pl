% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Peer Review as the Boundary of Legitimate Knowledge
 *   domain: epistemology/sts/political_theory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the credential-gated legitimacy
 *   economy: knowledge counts as legitimate when produced by methodologically
 *   rigorous inquiry and validated through credentialed peer review, with
 *   universities minting credentials, publishers and panels operating the
 *   validation venues, and funders allocating resources through the gate.
 *   This file instantiates ONE reading of the legitimate_knowledge_boundary
 *   kernel — the credentialed_expertise_reading — and authors a single,
 *   stable epsilon for that standing arrangement; the sibling readings are
 *   separate constraints in separate files, linked through the network, and
 *   are not averaged or hedged here. KEY AGENTS (by structural relationship):
 *   credentialed experts (primary beneficiary,
 *   institutional/identity_locked); scholarly publishers and funding agencies
 *   (agenda-setters, institutional); research universities (beneficiary and
 *   co-administrator, institutional/constrained); professional societies
 *   (secondary beneficiary, organized); uncredentialed independent
 *   researchers and experiential knowledge communities (primary targets,
 *   trapped exits); heterodox scholars (targets inside the gate,
 *   identity_locked); lay publics (excluded seat, powerless/trapped); STS and
 *   epistemology scholars (analytical observer).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.45).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.7).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Peer Review as the Boundary of Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/sts/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'aafb1966-e607-4813-9d08-34b996960323').
narrative_ontology:cs_kernel_codification('aafb1966-e607-4813-9d08-34b996960323', distributed).
narrative_ontology:cs_authority_grounding('aafb1966-e607-4813-9d08-34b996960323', expertise).
narrative_ontology:cs_interpretation_layer_present('aafb1966-e607-4813-9d08-34b996960323').
narrative_ontology:cs_reading_relation('aafb1966-e607-4813-9d08-34b996960323', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_reading_relation('aafb1966-e607-4813-9d08-34b996960323', legitimate_knowledge_boundary__hybrid_coproduction_reading, forecloses).
narrative_ontology:cs_axiom('aafb1966-e607-4813-9d08-34b996960323', foundational, credentialed_validation_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(credentialed_validation_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('aafb1966-e607-4813-9d08-34b996960323', credentialed_validation_constitutes_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('aafb1966-e607-4813-9d08-34b996960323', secondary, consensus_tracks_truth).
narrative_ontology:cs_axiom_status(consensus_tracks_truth, holdable).
narrative_ontology:cs_axiom_grounding('aafb1966-e607-4813-9d08-34b996960323', consensus_tracks_truth, instrumental).
narrative_ontology:cs_reference_frame('aafb1966-e607-4813-9d08-34b996960323', credentialed_peer_validation_standard).
narrative_ontology:cs_drift_state('aafb1966-e607-4813-9d08-34b996960323', post_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aafb1966-e607-4813-9d08-34b996960323', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, research_universities).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, scholarly_publishers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_agencies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_societies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, uncredentialed_independent_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, heterodox_scholars).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_supremacy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, expert_consensus_truth_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold advanced degrees, staff editorial boards and review panels, and collect salaries, grants, and public standing premised on the boundary between validated and unvalidated knowledge. Their professional self-concept is constituted by the credential system itself: leaving it means forfeiting the standing that makes their judgments count, so even internal critics work to reform the boundary rather than dissolve it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, beneficiary,
    institutional, generational, identity_locked, global).

% Produce and sell the credentials, house the journals and laboratories, and administer the hiring and tenure decisions through which the boundary is enforced day to day. Ranking and assessment regimes bind them into competing on the boundary's terms; they cannot stop minting credentials without dissolving their revenue and status base, but they maneuver vigorously within the rules.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, research_universities, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, research_universities, agenda_setter).

% Own the journals in which validation formally occurs, control acceptance and rejection, and collect subscription fees and article charges for gating work that was largely publicly funded. Their capital and portfolios are mobile across jurisdictions and business models, giving them the freest hand of any seat inside the arrangement.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, scholarly_publishers, agenda_setter,
    institutional, biographical, arbitrage, global).

% Award research grants on the advice of peer panels, inheriting the boundary's legitimacy for their decisions and insulation from lay challenge. Their mandates tie them to funding within accredited channels; they administer the resource side of the boundary without owning it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Accredit training programs, run the conferences where standing is performed, and issue the ethics codes that mark the boundary's edges. Membership dues and meeting revenue depend on the boundary remaining salient; they police deviance through censure and expulsion but depend on universities and publishers for reach.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, professional_societies, beneficiary,
    organized, generational, constrained, continental).

% Produce research outside degree and appointment channels. Their findings are inadmissible in the venues that confer standing regardless of quality, and no volume of output converts into recognition; the available exits are abandoning inquiry or accepting permanent amateur status.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, uncredentialed_independent_researchers, payer,
    moderate, biographical, trapped, global).

% Patient cohorts, indigenous land managers, and affected resident groups hold knowledge of their conditions that credentialed observers lack. Their testimony is admitted only as raw data after methodological extraction, never as validation; organizing has won consultation rights in some forums but not validation authority, and there is no venue they can exit to that would convert experience into standing.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_communities, payer,
    organized, generational, trapped, global).

% Credentialed insiders pursuing lines the gate discounts: contested paradigms, unfashionable methods, unwelcome findings. Their careers ride on reputational standing within the very community whose orthodoxy they strain against; deviation costs citations, grants, and invitations, while leaving the academy forfeits the audience they address.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, heterodox_scholars, payer,
    moderate, biographical, identity_locked, national).

% Bear the consequences of expert-validated policy in medicine, technology, and the environment without a seat in the venues where validity standards are set. Deference to expert judgment is the price of receiving its products; they are the objects of legitimation rather than participants in it.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, lay_publics, excluded,
    powerless, generational, trapped, global).

% Study the boundary itself: its history, its enforcement, its failures. They occupy an ambivalent seat inside the academy whose gate they analyze, producing the critiques that the boundary's beneficiaries read alternately as reform agenda and as threat.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, sts_and_epistemology_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters error and fraud from the knowledge supply at scale: laypeople and institutions cannot verify specialized claims directly, so standardized methodological training plus peer validation supplies a trust proxy that enables division of cognitive labor, cumulative science, and safe reliance on expert judgment.
% TRANSFER_FUNCTION: Moves epistemic authority, and the funding, publication, employment, and policy voice attached to it, toward credentialed insiders and away from uncredentialed claimants; correspondingly moves deference from lay publics to expert consensus.
% ABSENT_VOICES: Uncredentialed researchers, experiential-knowledge holders, and lay publics have no seat on editorial boards, review panels, or standards committees. The bodies that set validity criteria are staffed almost wholly by the credentialed, so agreement about the boundary's fairness arises partly because those it excludes were never in the room.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, knowledge claims would compete on visibility and persuasion rather than validated standing; policy bodies would lose their truth-proxy and face every technical dispute raw; universities, journals, and credential markets would lose their product. Error, fraud, and charisma would regain ground the filter currently holds, while long-suppressed lines of inquiry and testimony would resurface. The rearrangement would be deep and disruptive in both directions.
% FOUNDING_PROBLEM: Charlatanism and unreliable expertise in industrializing societies: states, courts, and markets needed a way to distinguish trained competence from quackery as knowledge production scaled beyond face-to-face communities.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and medicine corroborate the founding problem, documenting the pre-licensure quackery era that professionalization answered; contemporary misinformation researchers and public-health bodies attest its persistence. The reflexive limit should be stated plainly: the strongest attesters of the problem's continued life are themselves credentialed experts, so corroboration from outside the beneficiary set is partial and arrives ambivalently, from historians and STS scholars sitting inside the gate they criticize.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).
:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon (0.45) is authored from this reading's own seat over the shared referent: by the credentialed-expertise reading's lights, the arrangement's extraction is the surplus beyond genuine filtration cost — commercial publisher margins on publicly funded work, prestige cascades and gerontocracy, credential inflation taxing entrants, and asymmetric scrutiny of challengers — which this reading concedes is real but judges tolerable against the filter's error-removal function. A sibling reading of the identical referent would author materially higher epsilon; that divergence is the indexical datum the corpus exists to take, not an inconsistency to reconcile. Suppression (0.70) is a raw structural property, unscaled: licensure statutes, editorial control, grant-panel gatekeeping, tenure discipline, and audit regimes constitute extensive enforcement machinery, with only marginal leak channels (preprints, gray literature, industry and civic research) surviving outside it. Theater ratio (0.42) reflects the growth of ritual review, metric-chasing, and formulaic output alongside a still-functioning filter. Accessibility collapse (0.48): within the frame, non-credentialed routes collapse almost completely, but the frame's own acceptance is partial and contested. Resistance (0.58): open-science reform, STS and postcolonial critique, citizen-science networks, and indigenous data-sovereignty movements constitute sustained, growing opposition. The measurement series runs on one shared eight-point grid (1860-2025) for all three tracked metrics; the 1950 dip in extractiveness marks the postwar delivery peak, when the filter's visible payoff (vaccines, radar, transistors) maximally justified its costs even by this reading's lights. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity buildup (formalized peer review, bibliometric accountability, assessment regimes), a rising ratchet rather than a static picture. Coordination type is identity_coordination because the dominant function is boundary and membership maintenance — deciding who counts as a knower — whose failure mode is membership dilution; the FNL gaming alert applies: identity framing ('science is a community') must not excuse coupling that concentrates costs on powerless agents at global scope.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute a materially harsher arrangement than the beneficiary seat experiences: an uncredentialed researcher or an experiential-knowledge community meets the boundary as a wall, while a tenured expert meets it as professional routine. The agenda-setter seats sit between — publishers collect and administer without bearing the credential tax themselves. Heterodox scholars uniquely straddle the divide, holding standing inside the academy while being denied legitimacy for their actual claims, which is why their exit option is identity_locked rather than merely constrained. The engine computes these per-seat classifications from the structural data; this file's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the five beneficiary seats toward the subsidy end of directionality; the three victim declarations drive their seats toward the target end, amplified by trapped and identity_locked exits — the uncredentialed researcher cannot convert output into standing anywhere, and the heterodox scholar's career is fused with the community discounting them. Lay publics, though absent from the victims array, are excluded-and-trapped and therefore derive near-full-target directionality: they bear the arrangement's consequences without participating in it. Global spatial scope raises verification difficulty, which the engine reflects as modest amplification of effective extraction. No directionality overrides are needed: the role and exit declarations already differentiate the seats the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — separating trained competence from charlatanism at scale — remains live, so mandatrophy is NOT resolved and the arrangement has not outlived its function; the mismatch consumer reading (status=live x verdict=world_rearranges) correctly flags no zombie. The live danger runs in the opposite direction from obsolescence: a permanently live founding problem licenses indefinite extraction, the classic tangled-rope ratchet. The measurement series shows the mechanism — enforcement capacity and theatrical maintenance have risen monotonically since 1950 while the filter's marginal yield is increasingly questioned. Guard condition: if the founding problem were ever resolved (reliable decentralized verification) while the gate persisted, this story would flip toward piton; the truth_tracking_premise_status omega watches the premise whose collapse would start that clock.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the legitimate_knowledge_boundary kernel — what changes structurally if a sibling reading is instantiated instead?',
    'Read against the sibling stories (legitimate_knowledge_boundary__experiential_pluralism_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading): under the pluralist reading the victim set expands to include all methodologically-dismissed testimony as primary, barriers flatten, and epsilon over the same referent rises sharply; under the hybrid reading the gate survives but loses sole validating authority. The disagreement is located precisely in whether credentialed validation is constitutive of legitimacy or merely one instrument among several.',
    'Classification is stable within this reading; the kernel-level verdict on which reading should govern is carried by the sibling comparison, not by this file. Cross-reading epsilon comparisons are the designed indexical measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one reading of a contested kernel, with sibling deltas and the location of the disagreement.').

omega_variable(
    filtration_cost_vs_gatekeeping_rent,
    'How much of the measured extraction is the irreducible cost of filtering error from specialized knowledge, and how much is surplus accruing to incumbents?',
    'Natural experiments where the gate loosened (arXiv-era fields, open-peer-review pilots, jurisdictions relaxing licensure) tracked against error and fraud rates; cost accounting of review effort versus publisher margins and credential-premium wages.',
    'If most extraction is filtration cost, the coordination component dominates and this reading''s defense strengthens; if most is surplus, the extraction component dominates and even this reading''s seat must concede drift toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filtration_cost_vs_gatekeeping_rent, empirical, 'Separating coordination cost from incumbent surplus inside the gate.').

omega_variable(
    truth_tracking_premise_status,
    'Does credentialed peer validation actually track truth better than available alternatives — the empirical premise beneath this reading''s foundational axiom?',
    'Replication-crisis literature, prediction tournaments pitting credentialed experts against calibrated non-expert aggregates, and error-rate comparisons across gated and ungated knowledge channels.',
    'Sustained failure would override the foundational axiom (its empirically_contingent grounding routes to computed foreclosure pressure) and push the reading''s drift state toward repudiation; success would stabilize the reference frame and blunt the sibling readings'' strongest attack.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_tracking_premise_status, empirical, 'Status of the empirical warrant under the reading''s foundational axiom.').

omega_variable(
    asymmetric_enforcement_direction,
    'Is methodological rigor enforced asymmetrically as incumbent-protective gatekeeping (challengers scrutinized harder than incumbents) or as proportionate quality control?',
    'Review-outcome datasets comparing acceptance trajectories of paradigm-confirming versus paradigm-challenging submissions matched for quality signals; grant-panel scoring studies by proposal novelty.',
    'Confirmed asymmetry raises effective extraction on challenger seats specifically and supports the tangled-rope structure over a coordination-dominant account; refutation supports the reading''s self-understanding and lowers the payer seats'' computed severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_enforcement_direction, empirical, 'Direction of enforcement asymmetry at the validation gate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 1860, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1860, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1860, 0.08).
narrative_ontology:measurement(legi_tr_t1890, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(legi_tr_t1920, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1920, 0.13).
narrative_ontology:measurement(legi_tr_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1950, 0.16).
narrative_ontology:measurement(legi_tr_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(legi_tr_t2000, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2000, 0.29).
narrative_ontology:measurement(legi_tr_t2012, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2012, 0.36).
narrative_ontology:measurement(legi_tr_t2025, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t1860, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1860, 0.18).
narrative_ontology:measurement(legi_be_t1890, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1890, 0.24).
narrative_ontology:measurement(legi_be_t1920, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1920, 0.28).
narrative_ontology:measurement(legi_be_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(legi_be_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 1980, 0.31).
narrative_ontology:measurement(legi_be_t2000, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2000, 0.37).
narrative_ontology:measurement(legi_be_t2012, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement(legi_be_t2025, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1860, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1860, 0.3).
narrative_ontology:measurement(legi_su_t1890, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1890, 0.4).
narrative_ontology:measurement(legi_su_t1920, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1920, 0.47).
narrative_ontology:measurement(legi_su_t1950, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(legi_su_t1980, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(legi_su_t2000, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement(legi_su_t2012, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2012, 0.67).
narrative_ontology:measurement(legi_su_t2025, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who decides what counts as knowledge' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle — each reading authors its own epsilon over the shared referent (the standing credential-gated arrangement), so no single story may average across them. This file is the credentialed-expertise instantiation. Historically this reading is upstream: its gate controls the venues, funds, and standing through which any hybrid co-production must pass, creating structural influence-pressure on the siblings even where the logical relation is foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
