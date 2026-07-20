% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Reading
 *   domain: constitutional/indigenous_rights
 *
 * SUMMARY:
 *   This constraint instantiates the partnership reading of the
 *   waitangi_sovereignty_allocation kernel. The Treaty of Waitangi / Te
 *   Tiriti o Waitangi is interpreted here as establishing an ongoing
 *   partnership between Crown and MÄori requiring good faith consultation
 *   and active protection of MÄori interests. This reading dominates
 *   contemporary New Zealand constitutional practice through the Waitangi
 *   Tribunal, Treaty settlement processes, and judicial development of Treaty
 *   principles. It occupies a structural middle position between the Crown
 *   sovereignty reading (English Article I ceded complete sovereignty) and
 *   the rangatiratanga reading (MÄori text retained full authority via tino
 *   rangatiratanga). The partnership reading coordinates Crown-MÄori
 *   relations by providing institutional pathways for redress, but extracts
 *   from MÄori by subordinating rangatiratanga to Crown sovereignty and
 *   channeling MÄori authority into Crown-controlled processes.
 *
 * KEY AGENTS:
 *   - crown_institutions: Agenda-setter and beneficiary (institutional/constrained) â administers partnership, retains parliamentary sovereignty, gains legitimacy
 *   - maori_iwi_hapu: Primary payer (organized/constrained) â bears costs of consultation and subordinated sovereignty, receives procedural rights and settlements
 *   - waitangi_tribunal: Observer (institutional/analytical) â investigates breaches, recommends redress, no binding enforcement power
 *   - nz_judiciary: Observer (institutional/analytical) â interprets partnership principles, reinforces Crown sovereignty frame
 *   - maori_sovereignty_advocates: Excluded voice (organized/trapped) â rejects partnership frame, asserts rangatiratanga, structurally suppressed
 *   - nz_public: Diffuse beneficiary (moderate/mobile) â receives social cohesion dividend from stable constitutional order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.65).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.58).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '0eb4b68c-f9ed-464d-b36a-e797a10721b0').
narrative_ontology:cs_kernel_codification('0eb4b68c-f9ed-464d-b36a-e797a10721b0', fixed_text).
narrative_ontology:cs_authority_grounding('0eb4b68c-f9ed-464d-b36a-e797a10721b0', lineage).
narrative_ontology:cs_interpretation_layer_present('0eb4b68c-f9ed-464d-b36a-e797a10721b0').
narrative_ontology:cs_reading_relation('0eb4b68c-f9ed-464d-b36a-e797a10721b0', waitangi_sovereignty_allocation__crown_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('0eb4b68c-f9ed-464d-b36a-e797a10721b0', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('0eb4b68c-f9ed-464d-b36a-e797a10721b0', foundational, ongoing_partnership_obligation).
narrative_ontology:cs_axiom_status(ongoing_partnership_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0eb4b68c-f9ed-464d-b36a-e797a10721b0', ongoing_partnership_obligation, conventional).
narrative_ontology:cs_axiom('0eb4b68c-f9ed-464d-b36a-e797a10721b0', foundational, crown_sovereignty_presumptive).
narrative_ontology:cs_axiom_status(crown_sovereignty_presumptive, holdable).
narrative_ontology:cs_axiom_grounding('0eb4b68c-f9ed-464d-b36a-e797a10721b0', crown_sovereignty_presumptive, conventional).
narrative_ontology:cs_reference_frame('0eb4b68c-f9ed-464d-b36a-e797a10721b0', treaty_partnership_equilibrium).
narrative_ontology:cs_drift_state('0eb4b68c-f9ed-464d-b36a-e797a10721b0', contemporary_constitutional_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0eb4b68c-f9ed-464d-b36a-e797a10721b0', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_institutions).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, nz_public).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the partnership framework through legislation, policy, and Treaty settlement negotiations. Retains parliamentary sovereignty and final decision-making authority while being obligated to consult MÄori and act in good faith. Gains domestic and international legitimacy from the partnership frame, and controls the institutional design of consultation and redress.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_institutions, beneficiary).

% Engage in partnership processes to seek redress for Treaty breaches and protect taonga. Bear costs of extensive evidence-gathering, protracted negotiation, and participation in Crown-designed institutions. Rangatiratanga is acknowledged but subordinated to Crown sovereignty; they cannot unilaterally establish independent jurisdiction or exit the Crown constitutional frame.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_hapu, payer,
    organized, generational, constrained, national).

% Investigates Crown breaches of Treaty principles and recommends redress. Operates entirely within the partnership reading, reinforcing it through its processes and findings. Has moral and political authority but no binding enforcement power over the Crown.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Interprets Treaty principles in litigation, defining the legal content of partnership, good faith, and active protection. Reinforces Crown sovereignty as the constitutional baseline while incrementally expanding MÄori procedural rights within that frame.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, nz_judiciary, observer,
    institutional, generational, analytical, national).

% Assert tino rangatiratanga and reject the partnership frame as legitimizing Crown sovereignty and subordinating MÄori authority. Excluded from partnership consultative processes when they deny Crown legitimacy. Their preferred constitutional order is suppressed by the partnership reading's institutional dominance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_sovereignty_advocates, excluded,
    organized, generational, trapped, national).

% Benefits from stable constitutional order and the perception of just Crown-MÄori relations. Does not directly participate in partnership processes but receives the social cohesion dividend and the reduced risk of constitutional conflict.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, nz_public, beneficiary,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, crown_institutions).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional framework for post-colonial governance that enables Crown authority to coexist with MÄori collective interests, replacing potential violent conflict with institutionalized consultation, redress mechanisms, and policy co-design.
% TRANSFER_FUNCTION: Moves MÄori claims for sovereignty and autonomy into Crown-controlled institutional processes â Waitangi Tribunal, settlement negotiations, statutory consultation â transferring authority from rangatiratanga to partnership frameworks while providing procedural rights, settlements, and policy influence in exchange.
% ABSENT_VOICES: MÄori sovereignty advocates who reject Crown sovereignty entirely and view partnership as managed subordination; Crown supremacists who reject any distinct MÄori constitutional status beyond individual citizenship. Both are partially excluded from the partnership frame's consultative processes.
% DISAPPEARANCE_RATIONALE: If the partnership reading vanished overnight, the Treaty settlement framework, co-governance arrangements, statutory consultation requirements, and Waitangi Tribunal would lose their primary constitutional foundation. Crown-MÄori relations would revert to either raw Crown supremacy or contested rangatiratanga, fundamentally reorganizing New Zealand's constitutional order and its institutional landscape.
% FOUNDING_PROBLEM: How to establish legitimate British governance over New Zealand while acknowledging MÄori authority and preventing violent conflict over land and sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal reports and MÄori claimants attest the problem is ongoing through structural breaches. Crown ministers attest settlements provide redress. Independent constitutional historians and comparative indigenous law scholars outside both beneficiary and victim parties corroborate that the foundational sovereignty dispute remains structurally unresolved, though its expression has shifted from military confrontation to institutional contestation.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the structural asymmetry: MÄori must engage Crown-designed institutions to exercise authority, and parliamentary sovereignty remains final regardless of consultation depth. Suppression (0.58) measures the suppression of rangatiratanga as an alternative constitutional frame and the channeling of MÄori political energy into partnership processes. Theater ratio (0.52) captures the growing performative dimension of consultation, where Crown increasingly goes through procedural motions while retaining decision supremacy. Accessibility collapse (0.60) indicates that once the partnership frame is accepted, full rangatiratanga appears constitutionally unreachable. Resistance (0.55) reflects ongoing MÄori sovereignty advocacy and periodic Crown political resistance to partnership obligations.
 *
 * PERSPECTIVAL GAP:
 *   The Crown seat experiences this constraint as a genuine coordination mechanism that legitimizes governance, prevents violent conflict, and enables policy co-design. The MÄori iwi/hapÅ« seat experiences it as a constrained pathway offering real but partial redress within a sovereignty structure they did not choose and cannot exit. The MÄori sovereignty advocate seat experiences it as extraction dressed in partnership language. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown institutions are structural beneficiaries with low directionality: they collect legitimacy, stable governance, and retained sovereignty while bearing only procedural costs. The NZ public receives diffuse social cohesion benefits with low directionality. MÄori iwi/hapÅ« are structural targets with high directionality: they bear the costs of procedural engagement, evidence burdens, sovereignty subordination, and the foreclosing of independent jurisdiction. MÄori sovereignty advocates are trapped targets with very high directionality: their preferred constitutional order is excluded entirely by the partnership reading's institutional dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership reading prevents mandatrophy mislabeling by preserving a genuine coordination function: the Waitangi Tribunal and settlement processes have delivered material redress, policy changes, and co-governance arrangements that pure Crown sovereignty would not have produced. However, the constraint is not merely a rope because the Crown retains final authority and MÄori are structurally subordinated within the frame. The classification as tangled_rope captures both the real coordination (genuine redress and institutional voice) and the asymmetric extraction (sovereignty subordination and procedural capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partnership_sovereignty_ambiguity,
    'Does the partnership reading structurally constrain Crown parliamentary sovereignty, or does it function primarily as a legitimacy mechanism that preserves Crown dominance while managing MÄori dissent?',
    'Comparative outcome analysis across policy domains: if Crown decisions consistently override MÄori interests despite partnership processes at rates comparable to pre-partnership eras, the constraint is primarily legitimizing. If Crown power is measurably constrained by veto points, blocked legislation, or shifted policy, the constraint is structurally binding.',
    'If legitimacy-only, classification shifts toward snare (pure extraction via theatrical consultation). If structurally constraining, tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_sovereignty_ambiguity, empirical, 'Whether partnership is structural constraint or legitimacy mechanism').

omega_variable(
    rangatiratanga_accommodation,
    'Is the partnership reading structurally compatible with meaningful MÄori self-determination, or does its presumption of Crown sovereignty foreclose full rangatiratanga regardless of consultation depth?',
    'Institutional analysis of co-governance and iwi authority bodies: identify whether they exercise autonomous authority revocable only by MÄori decision-making, or delegated Crown functions revocable by Parliament.',
    'If autonomous, partnership may function as scaffold or rope toward rangatiratanga. If revocable, it is tangled_rope or snare maintaining Crown supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_accommodation, conceptual, 'Whether partnership accommodates or forecloses rangatiratanga').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wait_tr_t10, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(wait_tr_t20, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(wait_tr_t30, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(wait_tr_t40, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(wait_be_t10, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wait_be_t20, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(wait_be_t30, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(wait_be_t40, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(wait_su_t10, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(wait_su_t20, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(wait_su_t30, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(wait_su_t40, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the waitangi_sovereignty_allocation kernel, alongside crown_sovereignty_reading and rangatiratanga_reading. Each reading instantiates a structurally distinct constraint from the same Treaty text. The partnership reading emerged as a judicial and political compromise between the other two readings, and its dominance changes the legitimacy conditions and institutional configurations of both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
