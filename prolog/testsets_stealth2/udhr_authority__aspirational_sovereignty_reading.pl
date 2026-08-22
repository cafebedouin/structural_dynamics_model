% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR Consent-Gated Moral Guidance Arrangement (Aspirational-Sovereignty Reading)
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   The arrangement under contest: the Universal Declaration of Human Rights
 *   functions as moral guidance, and binding international rights obligation
 *   arises only through state consent — treaty ratification, with
 *   reservations; tribunals exercise no coercive power beyond the states that
 *   have accepted them. This file instantiates ONE reading of the
 *   udhr_authority kernel — the aspirational-sovereignty reading — authored
 *   as a clean, epsilon-invariant constraint. Its epsilon (0.30) assesses the
 *   standing consent-gated arrangement BY THIS READING'S OWN LIGHTS, which
 *   regard consent-gating as legitimate constitutional design rather than
 *   expropriation of state autonomy; the reading concedes a residual
 *   unprotected population but frames it as the agreed price of a system
 *   every state will join. The sibling readings —
 *   binding_universalism_reading and customary_emergence_reading — are
 *   separate files with their own epsilon values, beneficiary structures, and
 *   classifications, linked via network.affects_constraints; the universalism
 *   reading assesses the same standing arrangement as heavily extractive of
 *   individual protection, and the customary reading treats the gate as
 *   transitional scaffolding. This file does not adjudicate among them; the
 *   contest is routed to the omega variables. KEY AGENTS (by structural
 *   relationship): - sovereign_states_collective: Agenda-setter and principal
 *   beneficiary (institutional/arbitrage) — administers the consent gate and
 *   collects retained autonomy - great_powers: Concentrated beneficiary
 *   (powerful/arbitrage) — shape which obligations ever form -
 *   non_ratifying_states: Beneficiary (moderate/mobile) — decline obligations
 *   at no systemic cost - individuals_in_non_ratifying_states: Primary bearer
 *   of costs (powerless/trapped) — unprotected exposure with no international
 *   recourse - individuals_in_reservation_shielding_states: Cost-bearer
 *   (powerless/trapped) — formal coverage, practical denial -
 *   universalist_advocacy_movements: Dually positioned actor
 *   (organized/constrained) — pays in perpetual deflection, draws subsidy
 *   from aspirational prestige - international_human_rights_bodies:
 *   Constrained monitor (institutional/constrained) — operates inside the
 *   gate it cannot open - comparative_international_lawyers: Analytical
 *   observer (analytical/analytical) — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.3).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.22).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR Consent-Gated Moral Guidance Arrangement (Aspirational-Sovereignty Reading)").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/political_philosophy/human_rights_doctrine").

domain_priors:requires_active_enforcement(udhr_authority__aspirational_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, 'f01ea994-3584-490c-8598-297427ff214a').
narrative_ontology:cs_kernel_codification('f01ea994-3584-490c-8598-297427ff214a', fixed_text).
narrative_ontology:cs_authority_grounding('f01ea994-3584-490c-8598-297427ff214a', lineage).
narrative_ontology:cs_interpretation_layer_present('f01ea994-3584-490c-8598-297427ff214a').
narrative_ontology:cs_reading_relation('f01ea994-3584-490c-8598-297427ff214a', udhr_authority__binding_universalism_reading, forecloses).
narrative_ontology:cs_reading_relation('f01ea994-3584-490c-8598-297427ff214a', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('f01ea994-3584-490c-8598-297427ff214a', foundational, binding_obligation_requires_state_consent).
narrative_ontology:cs_axiom_status(binding_obligation_requires_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('f01ea994-3584-490c-8598-297427ff214a', binding_obligation_requires_state_consent, conventional).
narrative_ontology:cs_axiom('f01ea994-3584-490c-8598-297427ff214a', foundational, udhr_moral_guidance_not_justiciable_law).
narrative_ontology:cs_axiom_status(udhr_moral_guidance_not_justiciable_law, holdable).
narrative_ontology:cs_axiom_grounding('f01ea994-3584-490c-8598-297427ff214a', udhr_moral_guidance_not_justiciable_law, conventional).
narrative_ontology:cs_reference_frame('f01ea994-3584-490c-8598-297427ff214a', consent_gated_moral_declaration).
narrative_ontology:cs_drift_state('f01ea994-3584-490c-8598-297427ff214a', contemporary_multilateral_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f01ea994-3584-490c-8598-297427ff214a', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_states_collective).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, great_powers).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, non_ratifying_states).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, individuals_in_non_ratifying_states).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, individuals_in_reservation_shielding_states).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, universalist_advocacy_movements).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, consent_based_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community of states acting through the General Assembly, treaty conferences, and diplomatic practice. It drafted the Declaration in 1948 as a declaration rather than a treaty, writes and opens every rights covenant for signature, and decides collectively what counts as customary practice. Each member retains sole authority to accept or reject any given obligation, and the collective's continuing insistence that consent precedes obligation is the load-bearing wall of the arrangement. Exit for any member means denouncing treaties it has joined — available, costly, rarely used.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_states_collective, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, sovereign_states_collective, beneficiary).

% A small set of militarily and economically dominant states. They decide which proposed norms gain traction, shield allied governments from accountability through Security Council votes, sign selectively, and attach far-reaching reservations when they do sign. Their consent is the scarcest resource in the system, and the system's design gives them the widest menu of ways to withhold it.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, great_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Governments that have declined to join one or more core rights covenants while participating fully in UN deliberations and invoking the Declaration's language when diplomatically useful. Declining costs them no standing, no access, and no penalty; several pair non-ratification with domestic practices the covenants would scrutinize.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, non_ratifying_states, beneficiary,
    moderate, generational, mobile, national).

% People living under governments that have accepted no binding international rights instrument. Their avenues of redress run through domestic institutions that may themselves be the source of the harm. The international layer offers them a vocabulary of rights and periodic review of their government's record, but no body they can petition for enforcement.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individuals_in_non_ratifying_states, payer,
    powerless, biographical, trapped, national).

% People living in states that ratified core covenants while attaching reservations or domestic-effectiveness declarations that prevent the commitments from reaching individual claimants. On paper they are covered by the treaty system; in practice the covering document disclaims the very enforcement it appears to promise.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individuals_in_reservation_shielding_states, payer,
    powerless, biographical, trapped, national).

% Transnational NGOs, jurists, UN special procedure holders, and campaign networks pressing for rights claims to carry legal force. They operate inside multilateral forums their targets dominate, submit shadow reports, litigate where regional systems admit them, and spend much of their effort arguing the Declaration's own authority into being. Their agenda depends on the Declaration remaining a universally invoked reference point, which the arrangement's aspirational character supplies.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, universalist_advocacy_movements, payer,
    organized, generational, constrained, global).

% Treaty committees, special rapporteurs, the Office of the High Commissioner, and the Universal Periodic Review mechanism. They monitor records, issue findings and recommendations, and convene reviews, but execute nothing: their conclusions bind only where the state concerned has accepted the procedure and chooses to comply. Their budgets and mandates are renewed by the states they assess.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, constrained, global).

% Scholars and legal advisers who map how the Declaration's standing is construed across jurisdictions and traditions. They observe the full architecture — the consent gate, the treaty machinery behind it, the populations beyond it — and publish analyses that feed every camp without adjudicating among them.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, comparative_international_lawyers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__aspirational_sovereignty_reading, sovereign_states_collective).
narrative_ontology:fixing_cost_class(udhr_authority__aspirational_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how states with irreconcilable legal orders, ideologies, and records can share one international system: making obligation conditional on each state's own agreement keeps every government at the table, keeps the treaty channel open, and prevents the rights layer from hardening into a rival sovereign that a large fraction of the world would immediately repudiate.
% TRANSFER_FUNCTION: Moves enforceable protection away from individuals in jurisdictions whose governments have withheld consent, and converts the same withholding into retained decision-making autonomy for those governments; in the opposite direction it moves moral standing and rhetorical capital to any state willing to invoke the Declaration, and agenda-setting leverage to the states that control which obligations ever reach the signing table.
% ABSENT_VOICES: Individuals in non-ratifying and reservation-shielding states are the arrangement's declared subjects yet hold no seat anywhere in it: no vote in treaty conferences, no representation in the General Assembly, no standing before committees their government has not accepted. Their interests are voiced secondhand by advocacy movements and sympathetic delegations. Future generations in currently opting-out states are absent in the same way.
% DISAPPEARANCE_RATIONALE: If the consent gate vanished overnight — if Declaration-grade norms became enforceable against all states regardless of agreement — treaty ratification machinery would lose its function, governments currently shielding themselves through non-ratification and reservations would face enforced obligations or open defiance, the Security Council's accountability asymmetries would collide with automatic enforcement, and several governments would likely repudiate the system outright; the map of who enjoys international rights protection would redraw within years.
% FOUNDING_PROBLEM: In 1948 the drafters faced a fork: a binding covenant was unreachable because key states would neither accept supranational enforcement nor expose their domestic practices, yet a purely private statement would carry no weight. The Declaration was issued as a declaration — a common standard of achievement addressed to all peoples — while the binding layer was deferred to treaties each state would join voluntarily. The consent gate is that deferral, institutionalized.
% FOUNDING_PROBLEM_CORROBORATION: Universalist advocacy movements and UN special procedure holders — parties outside the beneficiary set — attest the founding problem is live, arguing in standing submissions that the consent gate still blocks protection where it is most needed. The drafting record corroborates the original compromise: contemporaneous accounts of the 1948 negotiations, preserved in UN archives and drafter memoirs, show the declaration-versus-covenant choice was made expressly because binding force was unattainable. No source outside the beneficiary set attests that the problem is dead.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__aspirational_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__aspirational_sovereignty_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).
:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed type, tangled_rope, comes from structure: the arrangement performs a real coordination service (consent-conditioned obligation keeps every government at the table and the treaty channel open — remove the gate and the system sheds members) AND carries asymmetric incidence through the same structure (individuals in unconsenting jurisdictions bear unprotected exposure while states collect retained autonomy; the same gate that coordinates states denies recourse to persons). The metrics are authored descriptively. Extractiveness 0.30: the reading sees modest residual exclusion, and the historical series shows it declining as ratification coverage widened (covenants in force 1976, post-Cold War accession wave, universal-periodic-review coverage of all states) — the gate excludes less today than in 1948 because more of humanity lives under consented instruments. Suppression 0.22: the gate coerces almost no one — states decline freely, withdrawal is available, and its restrictive force is mostly the quiet foreclosure of a justiciable alternative rather than active punishment. Theater ratio 0.32: commemorative ritual (anniversary declarations, pledge events, rhetorical invocation by non-ratifying governments) has grown while the bindingness frontier stayed static, but the ratification and review machinery remains functional. Accessibility collapse 0.45: alternatives do not fully collapse — bilateral and regional routes, soft-law mobilization, domestic incorporation, and litigation in consenting regional systems all remain workable, which is why resistance stays live. Resistance 0.60: sustained doctrinal and institutional pressure from adherents of the sibling readings keeps the gate permanently contested. Receipt surface: the arrangement's gains — retained veto over obligations — demonstrably accrue to the state collective seat, with great powers taking disproportionate shares inside it, so gain_flow names sovereign_states_collective rather than diffuse. Fixing cost: the only actors who could remove the gate are the states whose veto it is; surrendering it, or staffing an enforcement layer no enforcer exists to fill, is prohibitive relative to the benefit from their seat. All three tracked series run on one shared time grid (1948–2026, seven points): extraction declining and flattening, theater rising slowly, enforcement intensity decaying as the arrangement normalized — the gate needed vigorous defense in the covenant-fight era and routine maintenance now.
 *
 * PERSPECTIVAL GAP:
 *   The state seats and the cost-bearing seats compute differently. From the state seats the arrangement is constitutional design: obligation by agreement, no taxation of autonomy, every government's dignity preserved. From the trapped individual seats the same structure is denial of recourse — an international layer that speaks the language of their protection while withholding enforcement, with their own domestic institutions often the hazard the layer declines to reach. The advocacy seat experiences both faces at once: deflected as a matter of law, subsidized as a matter of rhetoric. The monitoring bodies sit inside the frame they cannot open — constituted, funded, and renewed by the states they assess. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map the three state seats toward the beneficiary end: the collective administers the gate and collects retained autonomy; great powers hold arbitrage-grade exit and shape the obligation menu; non-ratifying states ride free with mobile exit. Victim declarations map the two individual seats to near-full-target directionality — powerless, trapped, bearing the protection deficit directly. The one place the automatic derivation would err is the organized advocacy seat: victim status plus constrained exit reads as near-full target, but the movement draws real operating subsidy from the Declaration's aspirational prestige — its agenda depends on the document remaining a universally citable reference, which only the aspirational frame supplies — so a directionality override places it at d 0.62, nearer symmetric. The monitoring bodies and scholarly observers take observer/analytical treatment and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure directions. Reading the arrangement as pure coordination (rope) would erase the unprotected populations the gate leaves outside consented protection — the absent voices whose interests the arrangement declares itself to serve. Reading it as pure extraction (snare) would erase the genuine service it performs: without consent-gating, the treaty system and the Declaration's own universality of reception would not survive contact with state defection. Tangled rope holds both facts in one structure. Mandatrophy is not resolved: the founding problem — reconciling universal moral standards with sovereign consent — remains live, the coordination function is exercised continuously (new treaties, review cycles, reservation politics), and no sunset clause applies. The R5 mismatch consumer should find founding_problem_status=live consistent with disappearance_verdict=world_rearranges; no zombie flag is expected, and the rising theater series is monitored as the earliest symptom should the coordination function begin to atrophy behind the commemorative ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (aspirational_sovereignty_reading) of the udhr_authority kernel; if authoritative practice durably shifts to a sibling reading''s premises, which structural elements of this story change?',
    'Track authoritative uptake: Security Council practice, ICJ characterization of Declaration-derived norms, ratification trajectories, and doctrinal dominance among foreign-ministry legal advisers; sustained adoption of a sibling reading''s premises reclassifies the referent arrangement.',
    'Under the binding_universalism_reading''s premises the same standing arrangement computes as heavily extractive, with states as targets and individuals as intended beneficiaries; under the customary_emergence_reading the consent gate becomes transitional. This file''s classification holds only while this reading''s premises govern.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this story is one reading of a contested kernel; sibling readings are separate constraints with separate epsilon values.').

omega_variable(
    obligation_source_locus,
    'Where exactly do the three readings diverge — is the disagreement located solely in the source-of-binding-obligation premise (express consent versus inherent validity versus emergent custom), and do bridging positions such as custom-as-tacit-consent exist that would soften the foreclosure edge toward the customary reading?',
    'Doctrinal analysis of hybrid theories (opinio juris reconstructed as tacit consent) and of how states actually evidence assent in treaty and diplomatic practice.',
    'If tacit-consent bridges are doctrinally live, the forecloses relation to the universalism sibling stands but the influences relation to the customary sibling could relax toward coexistence; this reading''s own classification is unchanged, but the family topology changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_source_locus, conceptual, 'Location of the inter-reading disagreement and existence of bridging positions.').

omega_variable(
    unrepresented_cost_visibility,
    'Does the arrangement''s low measured extraction reflect genuine mildness, or successful displacement of costs onto parties holding no seat — individuals in non-consenting jurisdictions, whose protection is the arrangement''s declared subject?',
    'Counterfactual protection accounting: enumerate the recourse individuals would hold under a justiciable regime, compare against the consent-gate baseline, and weight by affected population across non-ratifying and reservation-shielding states.',
    'Full accounting could raise effective extraction at the individual seats sharply, moving computed per-seat types toward snare even while the state-level picture stays coordination-dominant, putting downward pressure on the story-level tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrepresented_cost_visibility, empirical, 'Whether absent-voice costs register in the measured extraction profile.').

omega_variable(
    maintenance_vs_structural_absence,
    'Does the consent gate persist because states actively maintain it — resisting customary crystallization, limiting tribunal competence, attaching reservations — or because no coercive alternative structure exists above them to replace it?',
    'Compare state behavior in regional systems where an enforcement layer exists (European Court of Human Rights) against behavior where it does not; examine whether governments defending the gate globally accept equivalent obligations locally when enforcement is available.',
    'If structural absence dominates, requires_active_enforcement is overstated and the arrangement sits closer to an inertial default than an enforced construct, shifting computed types toward rope or piton at the state seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_vs_structural_absence, conceptual, 'Active maintenance versus structural absence as the persistence mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_aspirational_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t1948, observed).
narrative_ontology:measurement(udhr_aspirational_tr_t1966, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1966, 0.2).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t1966, observed).
narrative_ontology:measurement(udhr_aspirational_tr_t1976, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1976, 0.24).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t1976, observed).
narrative_ontology:measurement(udhr_aspirational_tr_t1993, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1993, 0.27).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t1993, observed).
narrative_ontology:measurement(udhr_aspirational_tr_t2006, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2006, 0.29).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t2006, observed).
narrative_ontology:measurement(udhr_aspirational_tr_t2018, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2018, 0.31).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t2018, observed).
narrative_ontology:measurement(udhr_aspirational_tr_t2026, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2026, 0.32).
narrative_ontology:measurement_basis(udhr_aspirational_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(udhr_aspirational_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.44).
narrative_ontology:measurement_basis(udhr_aspirational_be_t1948, observed).
narrative_ontology:measurement(udhr_aspirational_be_t1966, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1966, 0.4).
narrative_ontology:measurement_basis(udhr_aspirational_be_t1966, observed).
narrative_ontology:measurement(udhr_aspirational_be_t1976, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1976, 0.36).
narrative_ontology:measurement_basis(udhr_aspirational_be_t1976, observed).
narrative_ontology:measurement(udhr_aspirational_be_t1993, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1993, 0.33).
narrative_ontology:measurement_basis(udhr_aspirational_be_t1993, observed).
narrative_ontology:measurement(udhr_aspirational_be_t2006, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2006, 0.31).
narrative_ontology:measurement_basis(udhr_aspirational_be_t2006, observed).
narrative_ontology:measurement(udhr_aspirational_be_t2018, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2018, 0.3).
narrative_ontology:measurement_basis(udhr_aspirational_be_t2018, observed).
narrative_ontology:measurement(udhr_aspirational_be_t2026, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2026, 0.3).
narrative_ontology:measurement_basis(udhr_aspirational_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_aspirational_su_t1948, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1948, 0.38).
narrative_ontology:measurement_basis(udhr_aspirational_su_t1948, observed).
narrative_ontology:measurement(udhr_aspirational_su_t1966, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1966, 0.36).
narrative_ontology:measurement_basis(udhr_aspirational_su_t1966, observed).
narrative_ontology:measurement(udhr_aspirational_su_t1976, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1976, 0.34).
narrative_ontology:measurement_basis(udhr_aspirational_su_t1976, observed).
narrative_ontology:measurement(udhr_aspirational_su_t1993, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1993, 0.3).
narrative_ontology:measurement_basis(udhr_aspirational_su_t1993, observed).
narrative_ontology:measurement(udhr_aspirational_su_t2006, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2006, 0.26).
narrative_ontology:measurement_basis(udhr_aspirational_su_t2006, observed).
narrative_ontology:measurement(udhr_aspirational_su_t2018, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2018, 0.23).
narrative_ontology:measurement_basis(udhr_aspirational_su_t2018, observed).
narrative_ontology:measurement(udhr_aspirational_su_t2026, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2026, 0.22).
narrative_ontology:measurement_basis(udhr_aspirational_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__customary_emergence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'UDHR authority' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — one per reading of the udhr_authority kernel. This file instantiates the aspirational_sovereignty_reading: the Declaration as moral guidance, binding obligation gated on state consent, low extraction on state autonomy. The sibling files instantiate the binding_universalism_reading (justiciable individual rights enforceable regardless of consent — high extraction on state autonomy, individuals as intended beneficiaries) and the customary_emergence_reading (aspiration matured into binding custom through state practice and opinio juris — the gate as transitional). Each story carries its own epsilon, beneficiary/victim structure, and claimed type; they are linked here because the aspirational reading is the historical baseline from which the other two depart, and because the customary reading cites as evidence the very state practice this reading governs. Measuring 'UDHR authority' with one observable collapses these distinct epsilon values into a single unstable number; the family decomposition is the fix.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__aspirational_sovereignty_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
