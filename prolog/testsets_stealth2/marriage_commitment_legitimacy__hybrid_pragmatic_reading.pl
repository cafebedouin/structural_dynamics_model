% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: The 1890 Manifesto as Strategic Institutional Adaptation (Hybrid Pragmatic Reading)
 *   domain: religious/political
 *
 * SUMMARY:
 *   In 1890, facing confiscation of temples, disincorporation, and mass
 *   imprisonment under the Edmunds-Tucker Act, LDS President Wilford Woodruff
 *   issued a declaration advising members against contracting any marriage
 *   forbidden by the law of the land. This story authors ONE reading of that
 *   event — the hybrid pragmatic reading — as a clean, epsilon-invariant
 *   constraint: prophetic authority deployed to manage an exogenous crisis
 *   while preserving the core theological commitment through scope ambiguity.
 *   On this reading the arrangement has a genuine coordination function (it
 *   ended prosecution, secured amnesty and statehood, and unified member
 *   behavior under existential threat) AND an asymmetric extraction structure
 *   (institutional leadership collected both federal compliance and doctrinal
 *   flexibility, while rank-and-file members bore interpretive uncertainty
 *   and plural-marriage-faithful members ultimately bore discipline). The
 *   referent of epsilon is the standing Manifesto arrangement as this reading
 *   assesses it — never the arrangement a sibling reading would defend. The
 *   sibling readings (endogenous reinterpretation; exogenous override) are
 *   separate constraints with their own epsilon values, linked through
 *   network.affects_constraints. Claim and metrics are independent: the
 *   claimed type states what this reading holds structurally true; the
 *   metrics describe the arrangement's operation as the documentary record
 *   shows it.
 *
 * KEY AGENTS:
 *   - first_presidency_leadership: agenda-setting beneficiary (institutional/arbitrage) — issued the text, controls its reading, collects both compliance and flexibility
 *   - federal_government: beneficiary (institutional/mobile) — receives compliance, applies intermittent coercive leverage
 *   - utah_territorial_community: beneficiary (organized/trapped) — consumes amnesty, returned property, and statehood
 *   - rank_and_file_members: primary payer (powerless/identity_locked) — bears interpretive uncertainty and obedience costs
 *   - plural_marriage_faithful: concentrated payer (moderate/constrained) — bears doctrinal loss and eventual discipline
 *   - nonconforming_apostles: institutional payers (institutional/identity_locked) — resign or are expelled for rejecting the official reading
 *   - mexico_colony_communities: payer with incidental benefit (moderate/constrained) — absorb colonization costs, host the loophole's marriages, inherit its closure
 *   - analytical_observer: analytical seat — reconstructs the record from diaries, council minutes, and hearing testimony
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.72).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "The 1890 Manifesto as Strategic Institutional Adaptation (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious/political").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '4bfbafd1-e5bc-4be3-8e01-ee142702a2bb').
narrative_ontology:cs_kernel_codification('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', fixed_text).
narrative_ontology:cs_authority_grounding('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', extraction).
narrative_ontology:cs_interpretation_layer_present('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb').
narrative_ontology:cs_reading_relation('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', foundational, prophetic_authority_serves_institutional_continuity).
narrative_ontology:cs_axiom_status(prophetic_authority_serves_institutional_continuity, holdable).
narrative_ontology:cs_axiom_grounding('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', prophetic_authority_serves_institutional_continuity, instrumental).
narrative_ontology:cs_axiom('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', foundational, scope_ambiguity_preserves_core_commitments).
narrative_ontology:cs_axiom_status(scope_ambiguity_preserves_core_commitments, holdable).
narrative_ontology:cs_axiom_grounding('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', scope_ambiguity_preserves_core_commitments, conventional).
narrative_ontology:cs_reference_frame('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', strategic_scope_ambiguity_framework).
narrative_ontology:cs_drift_state('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', post_second_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4bfbafd1-e5bc-4be3-8e01-ee142702a2bb', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, utah_territorial_community).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, plural_marriage_faithful).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, nonconforming_apostles).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, mexico_colony_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, mexico_colony_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 declaration over the president's signature and thereafter controlled what it meant: which jurisdictions it reached, which marriages counted as violations, and which questions would go unanswered. Collected the arrangement's principal returns — federal prosecution wound down, seized property was restored, Utah statehood was secured — while the underlying covenant doctrine stayed formally unrevised and available for future use. Exit was never in question: this seat wrote the text and staffed the councils that fixed its reading.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership, agenda_setter,
    institutional, generational, arbitrage, national).

% Sought the extinction of plural marriage as a matter of territorial governance and constitutional supremacy. Received compliance without administering the church: prosecutors stood down, escheat proceedings eased, Utah was admitted. Its leverage was the threat of resumed seizure and imprisonment, applied intermittently rather than continuously, and it could reopen enforcement whenever compliance looked hollow.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, beneficiary,
    institutional, generational, mobile, national).

% Latter-day Saint settlements across the Intermountain West bore the raids, asset seizures, and disenfranchisements of the enforcement years and gained most from their end: amnesty, returned property, statehood, and normalized civic participation. Embedded in the region and the institution at once, they had nowhere else to go and little standing to contest how the declaration was read.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, utah_territorial_community, beneficiary,
    organized, generational, trapped, regional).

% Ordinary members were instructed to obey a declaration whose theological status was never settled — repealed, suspended, or deferred? They paid tithing to an institution negotiating its own footing, raised children inside the ambiguity, and chose between trusting the prophet's public word and noticing the private divergences. Leaving meant losing community, family, and perceived salvation; staying meant carrying the unresolved question indefinitely.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, identity_locked, national).

% Families and elders who regarded plural marriage as a covenant condition of exaltation received a public retraction with no doctrinal replacement. Some relocated to Mexico or Canada, where the declaration's wording seemed not to reach; others waited for clarification that arrived only as discipline. When enforcement hardened after 1904, their adherence became grounds for church courts, loss of temple privileges, and eventually expulsion — the cost of reading the ambiguity differently than intended.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, plural_marriage_faithful, payer,
    moderate, biographical, constrained, regional).

% Members of the Quorum of the Twelve who continued to perform or defend plural marriages after the declaration and declined to affirm the official reading as binding on conscience. Pressured through 1904–1905, two resigned their positions rather than endorse the narrower construction; one was later excommunicated. Their exit ran through surrendering the office and community standing that constituted their life's identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, nonconforming_apostles, payer,
    institutional, biographical, identity_locked, national).

% Settlements in Chihuahua established partly to provide space where the covenant practice could continue under Mexican law. They absorbed the colonization costs — land purchase, isolation, hardship — and hosted sealings the declaration's wording appeared to permit. When the loophole closed, those marriages became liabilities and their communities became the visible remainder of the abandoned strategy.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, mexico_colony_communities, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, mexico_colony_communities, beneficiary).

% Historians and scholars working from diaries, council minutes, court records, and Senate hearing testimony reconstruct what was decided, when, and by whom — including the post-declaration marriages and the gap between public instruction and private administration. They hold no stake in the arrangement and can place the competing accounts of what the declaration was side by side.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managed the collision between a core covenant practice and federal law: supplied a single authoritative signal that ended public plural marriage, unified member behavior under existential threat, defused prosecution, and opened the path to amnesty and statehood — a collective-survival problem no member or congregation could solve individually.
% TRANSFER_FUNCTION: Moved interpretive burden and legitimacy risk downward: institutional leadership retained doctrinal flexibility (the public text's narrow wording left the principle formally intact), while ordinary members absorbed the cost of obeying a rule whose theological status was deliberately unresolved, and plural-marriage-faithful members absorbed discipline when they read the ambiguity differently than intended.
% ABSENT_VOICES: The plural-marriage-faithful and the apostles who refused the declaration were progressively excluded from the councils where its meaning was fixed; Mexican colony settlers affected by post-declaration sealing policy had no seat in Salt Lake deliberations; federal prosecutors shaped the public text's target but not its internal meaning. The consensus that the declaration 'settled' the question arose in rooms from which the parties who read it otherwise had already been removed.
% DISAPPEARANCE_RATIONALE: Had the declaration vanished overnight, prosecutions and asset seizures resume under the Edmunds-Tucker Act, Utah statehood dies in committee, the church's corporate existence dissolves, and the membership fractures between open defiance and capitulation — the entire institutional geography of the American West reorganizes around the wreckage.
% FOUNDING_PROBLEM: Federal destruction of the church's legal existence: the Edmunds-Tucker Act disincorporated the church, seized its temples and property, disenfranchised polygamists, and imprisoned practitioners — the declaration was built to end prosecution, recover assets, and secure Utah statehood while deciding as little as possible about the doctrine itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Smoot hearing testimony record (Senate, 1904–1907), federal court and pardon records from the enforcement years, and Woodruff's contemporaneous private journals describing the alternative as institutional annihilation all attest the founding crisis. Documentary editions of post-declaration marriage records, compiled by historians outside the tradition, attest that the arrangement outlived its declared public purpose. No non-beneficiary source attests the hybrid framing as such — the synthesis is assembled from adversarial testimonies that agree on the facts and disagree on the character of the act.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.60 at interval end) because the arrangement delivered a real survival dividend — prosecution ended, assets returned, statehood secured — while transferring its costs onto seats that did not author it. Suppression is high (0.72) because persistence required escalating internal enforcement: from advisory language in 1890, through quiet pressure on holdouts, to church courts, temple-recommend revocation, resignation, and excommunication after 1904. Theater peaks mid-interval (0.60 at 1904) — the public text and the known private administration diverged maximally while the Smoot hearings forced the issue — then falls as the Second Manifesto replaced ambiguity with explicit rule and performance became unnecessary. Accessibility collapse is moderate (0.50): exits existed (Mexico, Canada, silence, later schism) but each carried severe identity or material cost. Resistance is moderate (0.50): apostolic refusal, continued post-declaration marriages, and eventual fundamentalist separation. All three series run on one shared nine-point grid (1890–1910); the 1904 inflection is the structural event — theater declining while suppression climbs marks the strategy converting from ambiguity-management to enforcement-management.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat the arrangement is successful statecraft: the church survived, the doctrine survived, the people were spared. From the rank-and-file seat it is an unresolved command whose meaning shifted with enforcement needs; from the plural-marriage-faithful seat it is a suspended covenant administered by the same authority that taught it; from the nonconforming-apostle seat it is a loyalty test that priced conscience out of the quorum. Same text, same decade, opposite experiences — the engine computes per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits nearest the beneficiary pole: it wrote the text, administers its meaning, and collects both compliance and flexibility, with arbitrage-grade exit from any particular reading of its own document. The federal government is a beneficiary with mobile exit — it could always resume prosecution, which is why its benefit never required administering the church. The Utah community consumes the statehood dividend while trapped regionally. Rank-and-file members sit far toward the target pole: identity-locked exit amplifies their exposure to an ambiguity they did not author. Plural-marriage-faithful members and nonconforming apostles sit nearest the full-target end — constrained or identity-locked, they bore the arrangement's sharpest costs precisely because they took its doctrine seriously. Mexican colonists are genuinely mixed: subsidized into colonization, then stranded when the loophole their settlements served was closed. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against two symmetrical errors. Reading the arrangement as pure coordination (the endogenous-friendly view) erases the identifiable payers — the families disciplined after 1904, the apostles expelled, the colonists stranded by the loophole's closure. Reading it as pure extraction (the exogenous-friendly view) erases the genuine collective-action achievement — prosecution ended, assets returned, a community preserved under existential threat. The mandate question is live rather than dead: the crisis the arrangement was built to solve was substantially resolved by 1896, yet the arrangement persisted and hardened, which is why founding_problem_status is contested — the parties dispute whether what persisted was protection or position. No sunset clause was ever declared; the ambiguity was not designed to expire, which rules the scaffold reading out. Nor is this a degraded remnant maintained by inertia: the enforcement machinery was actively built up across the interval, and the administrator seat demonstrably profits — the cost-asymmetry test for a piton fails at the agenda-setting seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the marriage_commitment_legitimacy kernel is structurally accurate: divine command (endogenous), coerced capitulation (exogenous), or strategic hybrid adaptation?',
    'Adjudication against the documentary record — Woodruff''s 1889–1890 diaries, council minutes, the timing and location of post-declaration marriages, and the drafting history of the public text — weighed against each reading''s predictive signature.',
    'Selecting the endogenous reading removes leadership''s strategic agency and recasts the arrangement as coordination; selecting the exogenous reading removes leadership''s beneficiary position and recasts it as imposed; the hybrid reading alone yields the moderate-extraction, leadership-beneficiary profile authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which of three sibling readings the historical record actually instantiates.').

omega_variable(
    manifesto_scope_ambiguity_deliberate,
    'Was the declaration''s narrow wording (''marriages forbidden by the law of the land'') a deliberate strategic reservation of scope, or an imprecision whose exploitable breadth emerged only after publication?',
    'Drafting history, council minutes preceding publication, and the leadership''s contemporaneous private instructions distinguishing public teaching from internal administration.',
    'Deliberate ambiguity strengthens the hybrid reading''s extraction attribution — leadership engineered the loophole it later closed; emergent ambiguity weakens it and shifts weight toward the exogenous reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_scope_ambiguity_deliberate, empirical, 'Whether the scope ambiguity was designed or accidental.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of plural-marriage faithfulness primarily structural (church courts, temple-recommend revocation, expulsion) or internalized (trained equation of prophetic counsel with divine will making self-enforcement automatic)?',
    'Post-discipline trajectories: members who left after 1904-era courts reporting intact conviction, versus members who conformed without ever facing a court, indicate the share of compliance carried internally.',
    'If substantially internalized, effective suppression exceeds the structural measure — the enforcement machinery was smaller than the compliance it produced, and the arrangement''s persistence is more robust to enforcement decay than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of member compliance.').

omega_variable(
    rank_and_file_partial_beneficiary,
    'Did rank-and-file members receive enough of the arrangement''s dividends (amnesty, statehood, peace, returned assets) to sit partially on the beneficiary side rather than purely among those bearing its costs?',
    'Distributional analysis of who consumed the statehood dividend versus who bore the interpretive and disciplinary costs, using membership records and contemporary periodical debate.',
    'If members are partial beneficiaries, their directionality moves toward symmetry and the arrangement computes as closer to coordination; if costs dominated, the extraction asymmetry sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rank_and_file_partial_beneficiary, empirical, 'Whether the primary payer seat is pure or mixed.').

omega_variable(
    second_manifesto_regime_boundary,
    'Does the 1904 Second Manifesto terminate the hybrid arrangement, or instantiate the same strategy under harder constraints, with the ambiguity template recurring in later institutional adaptations?',
    'Comparative analysis of the post-1904 regime against later scope-ambiguous institutional adaptations; whether leadership treated ambiguity as an expendable tactic or a durable method of governance.',
    'If terminated, this constraint is a bounded episode ending near 1904–1910; if instantiated, the arrangement persists as a reusable governance template and the classification carries forward to successor arrangements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(second_manifesto_regime_boundary, conceptual, 'Whether the Second Manifesto ended or extended the hybrid strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(manifesto_hybrid_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1890, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1893, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1893, 0.36).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1893, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1896, 0.44).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1896, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1899, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1899, 0.52).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1899, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1902, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1902, 0.58).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1902, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.6).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1904, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1906, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1906, 0.52).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1906, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1908, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1908, 0.46).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1908, observed).
narrative_ontology:measurement(manifesto_hybrid_tr_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1910, 0.4).
narrative_ontology:measurement_basis(manifesto_hybrid_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(manifesto_hybrid_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1890, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1893, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1893, 0.5).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1893, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1896, 0.56).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1896, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1899, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1899, 0.6).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1899, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1902, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1902, 0.65).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1902, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.68).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1904, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1906, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1906, 0.65).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1906, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1908, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1908, 0.63).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1908, observed).
narrative_ontology:measurement(manifesto_hybrid_be_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1910, 0.6).
narrative_ontology:measurement_basis(manifesto_hybrid_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(manifesto_hybrid_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.3).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1890, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1893, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1893, 0.36).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1893, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1896, 0.43).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1896, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1899, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1899, 0.49).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1899, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1902, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1902, 0.56).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1902, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.63).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1904, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1906, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1906, 0.68).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1906, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1908, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1908, 0.7).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1908, observed).
narrative_ontology:measurement(manifesto_hybrid_su_t1910, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1910, 0.72).
narrative_ontology:measurement_basis(manifesto_hybrid_su_t1910, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the 1890 Manifesto' covers three structurally distinct claims about the same text, decomposed per the epsilon-invariance principle. This story authors the hybrid pragmatic reading (strategic adaptation; moderate epsilon ~0.60; leadership-beneficiary, member-payer structure). The endogenous reading (divine command) would author low extraction with the membership as net beneficiary of revealed guidance; the exogenous reading (coerced capitulation) would author high extraction with the federal government as extractor and the church as victim. Upstream/downstream: the endogenous account supplies the legitimating warrant the hybrid arrangement trades on, so the endogenous story typically influences this one; the exogenous story shares the coercion premise but denies strategic agency. Each reading is epsilon-invariant on its own; the epsilon spread across the family is the datum the decomposition exists to expose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
