% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Indigenous Return Framing of Jewish Self-Determination
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   The constraint is the indigenous-return doctrine as it operates in
 *   political discourse: the requirement, enforced through advocacy
 *   infrastructure and institutional adoption, that the Zionist enterprise be
 *   understood as the return of an indigenous people with unbroken connection
 *   to the land — hence decolonization rather than colonization. Presented by
 *   its holders as settled historical fact, it functions in practice as a
 *   contested legitimacy framework that must be continuously asserted,
 *   litigated, and defended, and whose acceptance ranks the rival narrative
 *   of the land's other long-present people. KEY AGENTS (by structural
 *   relationship): - zionist_advocacy_organizations: agenda-setting enforcer
 *   (organized/identity_locked) — runs deployment, treats challenge as
 *   existential - israeli_state_diplomatic_establishment: primary beneficiary
 *   (institutional/arbitrage) — collects the legitimacy yield, hedges across
 *   available accounts - palestinian_indigeneity_claimants: primary
 *   cost-bearer (organized/trapped) — narrative enters pre-ranked -
 *   postcolonial_scholarly_community: secondary cost-bearer
 *   (moderate/constrained) — analytic framework pre-disqualified -
 *   dissenting_diaspora_jews: tertiary cost-bearer (moderate/identity_locked)
 *   — communal sanction for rejection -
 *   global_indigenous_solidarity_networks: dual-positioned (organized/mobile)
 *   — elective gains and exposure - international_human_rights_fora:
 *   analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.74).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.62).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Indigenous Return Framing of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '40b6aaf3-04b6-4644-b6b9-1a3d9b72c347').
narrative_ontology:cs_kernel_codification('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', distributed).
narrative_ontology:cs_authority_grounding('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', lineage).
narrative_ontology:cs_interpretation_layer_present('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347').
narrative_ontology:cs_reading_relation('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', foundational, unbroken_indigeneity_makes_zionism_decolonization).
narrative_ontology:cs_axiom_status(unbroken_indigeneity_makes_zionism_decolonization, holdable).
narrative_ontology:cs_axiom_grounding('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', unbroken_indigeneity_makes_zionism_decolonization, empirically_contingent).
narrative_ontology:cs_axiom('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', secondary, ancestral_priority_subordinates_later_arrival_claims).
narrative_ontology:cs_axiom_status(ancestral_priority_subordinates_later_arrival_claims, holdable).
narrative_ontology:cs_axiom_grounding('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', ancestral_priority_subordinates_later_arrival_claims, deontological).
narrative_ontology:cs_reference_frame('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', unbroken_presence_as_baseline).
narrative_ontology:cs_drift_state('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', contemporary_postcolonial_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('40b6aaf3-04b6-4644-b6b9-1a3d9b72c347', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, zionist_advocacy_organizations).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, israeli_state_diplomatic_establishment).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, palestinian_indigeneity_claimants).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, postcolonial_scholarly_community).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, dissenting_diaspora_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, global_indigenous_solidarity_networks).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, global_indigenous_solidarity_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the framing's day-to-day deployment: campus programs, rapid-response media desks, legislative lobbying, and legal defense funds. Staffing, donor bases, and member identities are built around the indigenous-return account; abandoning it would dissolve the organizations' reason to exist. They draft the talking points other seats repeat and treat challenges to the framing as existential attacks to be answered, not positions to be weighed.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_advocacy_organizations, agenda_setter,
    organized, generational, identity_locked, global).

% Collects the framing's principal yield: a legitimacy account under which sovereignty reads as restoration rather than conquest, deployable at international bodies, in bilateral relations, and in domestic defense of the Law of Return. It also invests in enforcement — ministries, public-diplomacy budgets, rebuttal operations — and can shift emphasis to other available legitimacy accounts when this one underperforms in a particular forum, which lowers its dependence on any single account.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, israeli_state_diplomatic_establishment, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, israeli_state_diplomatic_establishment, agenda_setter).

% Hold their own long continuous connection to the same land, documented in Ottoman, Mandate, and village records and carried in living memory. Under this framing their claim enters every conversation pre-ranked: either as later arrival or as co-presence with subordinate priority. They cannot leave the discourse — the land is the subject of their identity — and each forum that adopts the framing arrives with their narrative already discounted.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_indigeneity_claimants, payer,
    organized, generational, trapped, regional).

% Apply a settler-analytic toolkit across many cases; when they apply it to this one, the framing pre-emptively disqualifies the application as category error or animus. Professional costs follow: journal disputes, funding friction, event disinvitations. Remaining inside their discipline's mainstream method therefore carries a rising price, while leaving the method means leaving the field's core apparatus.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, postcolonial_scholarly_community, payer,
    moderate, biographical, constrained, global).

% Members of communities whose institutions have adopted the framing as consensus. Rejecting it costs standing: pulpit access, school admission, family peace, communal office. Their Jewish identity binds them to the very communities enforcing the account they doubt, so stepping outside the framing means stepping outside communal life itself.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, dissenting_diaspora_jews, payer,
    moderate, biographical, identity_locked, global).

% Some networks gain: endorsing the analogy extends their solidarity reach and brings new constituencies and donors. Others pay: the template of ancient return outweighing later presence, once validated here, is available for deployment against their own land claims, and taking a side in this conflict splits coalitions. Solidarity is elective for them, so disengagement costs less than for the bound seats.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, global_indigenous_solidarity_networks, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, global_indigenous_solidarity_networks, payer).

% Treaty bodies, courts, and United Nations committees receive the framing as argument and rival accounts as counter-argument, issue findings, and set precedents that both sides then cite. They adjudicate without bearing the arrangement's costs or collecting its yields.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_human_rights_fora, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, israeli_state_diplomatic_establishment).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the legitimacy problem of a diaspora-origin national movement operating inside a postcolonial normative order: it supplies one account — ancestral return rather than foreign settlement — through which Jewish statehood can be argued as justice, coordinating advocacy messaging, legal strategy, and communal identity around a single vocabulary connected to global indigenous-rights instruments.
% TRANSFER_FUNCTION: Moves discursive standing and legitimacy: from Palestinian claimants, whose narrative enters pre-ranked as later arrival or subordinate co-presence, and from critics, who must argue against a position presented as binary historical fact, toward Jewish national claimants and the state whose sovereignty the framing underwrites.
% ABSENT_VOICES: Palestinian historians and claimants would object that their own continuous presence is being ranked out of relevance; within the frame their objection is anticipated and pre-answered (later arrival, subordinate priority) before they speak. Mizrahi Jewish histories, which complicate any simple European-arrival account, are largely absent from the framing's flagship deployments even though they would strengthen parts of it. Arab-Jewish refugee communities are invoked instrumentally rather than seated as speakers.
% DISAPPEARANCE_RATIONALE: If the framing vanished overnight, the advocacy infrastructure built around it would lose its central argument; campus, legal, and diplomatic contests would reorganize around the equal-nationhood and covenant accounts; and the specific delegitimation costs now borne by Palestinian claimants and critical scholars would change shape rather than disappear, since every rival account imposes its own ranking.
% FOUNDING_PROBLEM: After 1945, movements that established sovereignty through settlement and conquest lost international legitimacy. A national movement whose constituency was largely European-descended, arriving into an Arab-majority country, needed an account on which its statehood was restoration rather than colonization. The indigenous-return framing was built to supply that account.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Palestinian and postcolonial scholars attest the framing's function — answering the colonialism charge — while disputing its success; published histories of Israeli public diplomacy document the deliberate post-1975 turn to indigeneity vocabulary; United Nations debate records show the framing deployed defensively against the 1975 'Zionism is racism' resolution. None of these sources collects from the framing; all locate its origin in the legitimacy deficit the founding problem names.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because the framing, to operate, must be continuously enforced against a live rival account: acceptance subordinates the Palestinian narrative and converts a bilateral grievance into a unilateral legitimacy verdict, and the enforcement bill (delegitimation, institutional pressure, professional cost) is paid by dissenters rather than by the framing's holders. Suppression (0.62) is a raw structural property, unscaled by power or scope: rival accounts remain publishable and articulable, but institutional adoption of the framing raises the price of using them. Theater ratio (0.40) reflects a growing share of performative deployment — archaeology as advocacy, symbolic indigeneity performance — alongside functional substance (repatriation law, actual migration). Accessibility collapse (0.50) is conditional-versus-population: for an agent who accepts the binary-status premise, rival accounts collapse almost completely, since 'indigenous return' and 'settler colonialism' cannot both describe the same project; at population level the alternatives remain fully alive, which is why the aggregate sits mid-scale. Resistance (0.85) is among the highest of any contemporary discursive constraint: an organized national movement, a diplomatic bloc, and a scholarly field contest it continuously — coalition capacity among the cost-bearing seats is real and exercised, which is why the value is high rather than the constraint being uncontested. The measurement series run on one shared eight-point grid (1948–2024) so every tracked metric is authored at every examined time point; all three trajectories rise monotonically, modeling an enforcement ratchet rather than a cycle. Claim and metrics are independent authored facts: the reading self-presents as mountain-grade historical fact, while the authored metrics describe contested, actively enforced, asymmetrically costly operation — the divergence is the datum, not an error to reconcile.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the advocacy seat the arrangement is genuine coordination it builds and staffs — a solved legitimacy problem, low personal cost, identity fused with the account. From the state seat it is a yielding asset with hedged exposure. From the Palestinian seat the same structure operates as enforced narrative dispossession with no exit. Scholarly and dissenting-diaspora seats experience intermediate profiles: real coordination goods received (a shared vocabulary, communal belonging) priced against real standing costs. The observer seat sees the hybrid whole. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (advocacy organizations, state establishment) drive those seats toward the beneficiary end; victim declarations (Palestinian claimants, scholars, dissenting diaspora Jews) drive them toward the target end. Exit modulation does the fine grading: the state's arbitrage option dampens its d below what a locked beneficiary would show; the trapped Palestinian seat and the identity-locked dissenters sit nearest the full-target end; the mobile solidarity networks sit mid-scale, matching their dual role. No directionality overrides are authored: the derivation chain from declared roles plus exit options reproduces the qualitative structure without correction, and an override keyed to the institutional power atom would wrongly capture the observer seat as well.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing legitimacy for a settlement-founded sovereignty under postcolonial norms — remains live, so no mandatrophy resolution is declared; the R5 mismatch consumer finds status=live paired with verdict=world_rearranges, no zombie flag. The classification work here cuts both ways. Treating the framing as mountain-grade fact (its own self-presentation) would render its subordination costs invisible — a false-summit failure in which beneficiaries hide behind naturality. Totalizing it as pure extraction would erase the genuine coordination it performs (a real legitimacy vocabulary, real historiography underneath, real identity goods for adherents) and misread a functioning hybrid as a bare trap. The hybrid classification keeps both halves priced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the indigenous_return_reading of the jewish_self_determination kernel; which structural facts of the story would change if a sibling reading governed the same referent?',
    'Read the four sibling stories in the constraint family side-by-side: victim sets, beneficiary sets, and epsilon differ by reading while the arrangement under assessment stays fixed.',
    'Under the settler_colonial_reading the same arrangement computes with Palestinians as primary cost-bearers and substantially higher extraction; under the religious_covenant_reading legitimacy detaches from historical evidence entirely. The classification in this file is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this file is one of five live readings of a single kernel.').

omega_variable(
    binary_vs_graduated_indigeneity,
    'Is indigenous status binary (one people is indigenous, full stop, making the claim self-executing) or graduated (connection admits degrees, letting the other population''s connection count equally)?',
    'Comparative ethnogenesis scholarship on Levantine populations: whether the relevant identity categories admit continuous gradations or discrete classes, and how the framing''s proponents actually operationalize status in legal and diplomatic argument.',
    'If graduated, the framing loses its exclusivity, the ranking of the rival narrative loses its warrant, and epsilon drops sharply toward the ''accepted-as-fact'' branch modeled in contest_driven_epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_vs_graduated_indigeneity, empirical, 'Whether the load-bearing premise of the framing is a binary or a degree.').

omega_variable(
    continuity_demographic_vs_memorial,
    'Does ''unbroken connection'' assert continuous demographic presence on the land, or a maintained textual-liturgical-and-directional relationship sustained across physical exile?',
    'Historiographic and population-genetic analysis that distinguishes the two continuity types and traces which one the framing''s enforcement actually cites when challenged.',
    'If the connection is memorial rather than demographic, the claim is a constructed and continuously maintained arrangement rather than a discovered fact — the mountain-grade presentation fails and classification trends toward transitional or inertial profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_demographic_vs_memorial, empirical, 'Which kind of continuity the ''unbroken connection'' premise denotes.').

omega_variable(
    subordination_transfer_or_ordering,
    'Does the framing''s ranking of the Palestinian narrative impose a transfer (standing, evidentiary weight, and narrative authority move away from the subordinated claimants) or merely record a priority ordering with no one made worse off?',
    'Trace concrete outcomes where the framing is adopted: which narratives are admitted in legal fora, curricula, and institutional statements, and what evidentiary burden shifts onto the subordinated side.',
    'If mere ordering, the declared victim set is overstated and the profile trends toward pure coordination; if transfer, the hybrid coordination-plus-extraction reading is confirmed and the victim declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_transfer_or_ordering, conceptual, 'Whether ranking a rival claim is costless ordering or extractive transfer.').

omega_variable(
    contest_driven_epsilon,
    'How much of the measured extraction is produced by the contest itself (enforcement against live rivals, costs pushed onto dissenters) rather than by the content of the claim?',
    'Compare extraction signatures in domains where the historical claim is uncontested (liturgical and communal life, where the continuity premise operates without rivals) against contested political domains.',
    'If contest-driven, universal acceptance of the claim as settled fact would collapse epsilon toward the very-low branch and the profile would reclassify toward fixed-fact or pure-coordination shapes; the current high value is then substantially a measure of disputedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contest_driven_epsilon, empirical, 'Decomposing measured extraction into contest costs versus claim-content costs.').

omega_variable(
    suppression_structural_vs_internalized,
    'For dissenting diaspora Jews, is the suppression of rejection structural (institutional sanction: pulpits, schools, offices) or internalized (anticipatory self-censorship driven by belonging-needs that would persist if sanctions were lifted)?',
    'Post-exit trajectory study: track dissenters who leave communal institutions and measure whether their willingness to voice rejection rises, and how fast.',
    'If a large share is internalized, effective suppression exceeds the structural measure — the target carries the mechanism out of the institution — and remedies aimed at formal sanctions alone will underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized split in the dissent-suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_irr_tr_t1948, jewish_self_determination__indigenous_return_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jsd_irr_tr_t1967, jewish_self_determination__indigenous_return_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jsd_irr_tr_t1975, jewish_self_determination__indigenous_return_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(jsd_irr_tr_t1990, jewish_self_determination__indigenous_return_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(jsd_irr_tr_t2000, jewish_self_determination__indigenous_return_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(jsd_irr_tr_t2010, jewish_self_determination__indigenous_return_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(jsd_irr_tr_t2017, jewish_self_determination__indigenous_return_reading, theater_ratio, 2017, 0.38).
narrative_ontology:measurement(jsd_irr_tr_t2024, jewish_self_determination__indigenous_return_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jsd_irr_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(jsd_irr_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(jsd_irr_be_t1975, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(jsd_irr_be_t1990, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(jsd_irr_be_t2000, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(jsd_irr_be_t2010, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(jsd_irr_be_t2017, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2017, 0.72).
narrative_ontology:measurement(jsd_irr_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(jsd_irr_su_t1948, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1948, 0.25).
narrative_ontology:measurement(jsd_irr_su_t1967, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(jsd_irr_su_t1975, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1975, 0.45).
narrative_ontology:measurement(jsd_irr_su_t1990, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(jsd_irr_su_t2000, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(jsd_irr_su_t2010, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(jsd_irr_su_t2017, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(jsd_irr_su_t2024, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Jewish claim to the land' conflates five structurally distinct constraints that share one referent (the standing Zionist/Israeli arrangement) and diverge on which premise grounds its legitimacy. Each reading is a separate file with its own epsilon, beneficiary/victim structure, and type; this file instantiates the indigenous_return_reading. Family members are linked via affects_constraints so contamination and foreclosure analysis can traverse the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
