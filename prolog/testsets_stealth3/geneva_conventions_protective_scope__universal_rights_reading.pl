% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Universal Protective Floor Reading of the Geneva Conventions
 *   domain: legal/international-humanitarian
 *
 * SUMMARY:
 *   This story instantiates the universal-rights reading of the Geneva
 *   protective-scope kernel: the claim that protections attach to persons,
 *   not to combatant status, with the 1949 Common Article and incorporated
 *   human rights law forming a floor that reaches every person in every armed
 *   conflict. Structurally, the reading expands the protected set to include
 *   everyone states would otherwise exclude by status tests, and it prices
 *   that expansion in state operational currency — targeting discipline,
 *   detention registration, interrogation-method prohibition. The
 *   claim/metric gap is deliberate: the reading is CLAIMED as tangled_rope
 *   because it possesses both a genuine coordination function (a universal
 *   humane-treatment floor solving a reciprocity problem) and asymmetric
 *   extraction (state military and intelligence organs pay; non-state actors
 *   and civilians collect), while the metrics describe the arrangement's
 *   actual operation independently of that claim. Sibling readings are
 *   separate constraint files, not positions inside this one.
 *
 * KEY AGENTS:
 *   - state_armed_forces: Primary target (institutional/constrained) — surrenders targeting, detention, and interrogation latitude
 *   - military_intelligence_services: Secondary target (institutional/identity_locked) — tradecraft criminalized by the floor
 *   - civilian_populations_in_conflict_zones: Primary beneficiary (powerless/trapped) — the floor's intended recipients
 *   - irregular_detainees: Expanded-victim-set beneficiary (powerless/trapped) — protected only under this reading
 *   - non_state_armed_groups: Asymmetric beneficiary (organized/mobile) — protection without symmetric obligation
 *   - humanitarian_agencies: Mandate beneficiary (organized/constrained) — operational carrier of the floor
 *   - captured_state_service_members: Reciprocity beneficiary (powerless/trapped) — covered even in irregular hands
 *   - international_criminal_justice_bodies: Agenda setter (institutional/constrained) — adjudicates and enforces scope
 *   - human_rights_monitoring_bodies: Analytical observer (institutional/analytical)
 *   - families_of_enforced_disappeared: Excluded voice (powerless/trapped) — bear the stakes, hold no seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Universal Protective Floor Reading of the Geneva Conventions").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "legal/international-humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '6104c7dc-0191-4d15-a12a-6141038c8d73').
narrative_ontology:cs_kernel_codification('6104c7dc-0191-4d15-a12a-6141038c8d73', fixed_text).
narrative_ontology:cs_authority_grounding('6104c7dc-0191-4d15-a12a-6141038c8d73', expertise).
narrative_ontology:cs_interpretation_layer_present('6104c7dc-0191-4d15-a12a-6141038c8d73').
narrative_ontology:cs_reading_relation('6104c7dc-0191-4d15-a12a-6141038c8d73', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('6104c7dc-0191-4d15-a12a-6141038c8d73', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('6104c7dc-0191-4d15-a12a-6141038c8d73', foundational, protection_status_independent).
narrative_ontology:cs_axiom_status(protection_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('6104c7dc-0191-4d15-a12a-6141038c8d73', protection_status_independent, deontological).
narrative_ontology:cs_axiom('6104c7dc-0191-4d15-a12a-6141038c8d73', secondary, ca3_ihrl_universal_floor_binding).
narrative_ontology:cs_axiom_status(ca3_ihrl_universal_floor_binding, holdable).
narrative_ontology:cs_axiom_grounding('6104c7dc-0191-4d15-a12a-6141038c8d73', ca3_ihrl_universal_floor_binding, conventional).
narrative_ontology:cs_reference_frame('6104c7dc-0191-4d15-a12a-6141038c8d73', universal_protection_floor).
narrative_ontology:cs_drift_state('6104c7dc-0191-4d15-a12a-6141038c8d73', post_2001_status_carveout_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6104c7dc-0191-4d15-a12a-6141038c8d73', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, irregular_detainees).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_agencies).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, captured_state_service_members).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_armed_forces).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, military_intelligence_services).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, customary_ihl_universality_thesis).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, ihl_ihrl_convergence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Field armies and air forces of ratifying states. They surrender targeting latitude (distinction and proportionality duties extend to every strike against every person), accept detention registration and access obligations for every person they hold, and forgo interrogation methods their own doctrine once listed as available. Formal exit exists in treaty denunciation, but customary-core continuity, alliance friction, and reciprocity exposure make it ruinous, so they operate inside the floor while contesting its edges.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_armed_forces, payer,
    institutional, generational, constrained, global).

% State intelligence and military-interrogation organs. The floor removes coercive interrogation methods from their lawful toolkit and requires humane handling of every detainee, including suspected terrorists and insurgents. Their professional identity, career ladders, and institutional memory are built around tradecraft the floor criminalizes; abandoning that identity is not a menu option, so they contest the scope through classification maneuvers, site concealment, and proxy arrangements rather than through exit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, military_intelligence_services, payer,
    institutional, biographical, identity_locked, global).

% Residents of active conflict areas. They hold an enforceable claim to humane treatment, siege relief, and protection from indiscriminate attack regardless of which force controls their area. They cannot leave, cannot arm their claim, and depend on monitors and tribunals to make the claim bite; their protection arrives as a byproduct of rules they never consented to and cannot renegotiate.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, generational, trapped, regional).

% Persons seized in conflict who hold no combatant qualification — suspected insurgents, alleged terrorists, foreign-fighter detainees. Under this reading they acquire the full detention floor: registered custody, humane treatment, access for visiting agencies, no coercive interrogation. Under status-scoped alternatives they would have no treaty channel at all; their protection exists entirely because this reading won interpretive ground, and they have no seat anywhere the scope is contested.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, irregular_detainees, beneficiary,
    powerless, immediate, trapped, regional).

% Armed groups fighting states. They receive protected-person status for their wounded, detained, and civilian auxiliaries without ever signing anything, and they face far weaker enforcement of their own reciprocal obligations than states face. Every restriction the floor places on state operations is a restriction their opponents carry and they largely do not. They cannot exit, because exit would forfeit the protection their own members receive in enemy hands.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, biographical, mobile, regional).

% The ICRC and sister agencies. Their detention-visit mandate, access negotiations, and family-links work are anchored in the floor's universality — every custody situation falls inside their legal remit rather than depending on per-case charity bargains. They pay for the anchor with negotiation labor, staff risk, and the political cost of being the floor's most visible operational carrier.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, humanitarian_agencies, beneficiary,
    organized, generational, constrained, global).

% Soldiers of ratifying states held by irregular forces. Under status-scoped rules their captors would owe them nothing; under the universal floor the Common Article reaches every captor, giving them a claim chain — agency tracing, tribunal accountability — that exists nowhere else. They are individually powerless and wholly dependent on the floor surviving their capture.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, captured_state_service_members, beneficiary,
    powerless, immediate, trapped, local).

% The permanent criminal court, ad hoc tribunals, and universal-jurisdiction courts. They adjudicate the floor's applicability and enforce it; their jurisdictional reach expands as this reading's scope wins, because every person in every conflict becomes a potential protected person and a potential case. They depend on state cooperation for arrests and funding, so they administer a floor they cannot compel.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_justice_bodies, agenda_setter,
    institutional, generational, constrained, global).

% UN treaty bodies, special procedures, and regional human rights courts. They audit state conduct in armed conflict against the fused humanitarian-law and human-rights standard, publish findings, and supply the evidentiary record the tribunals consume. They observe and report; they hold no enforcement instrument of their own.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% Relatives of persons seized in conflict whose fate and whereabouts are concealed. The floor's registration and access duties are the difference between searchable custody and erasure. They lobby, litigate where forums permit, and testify to commissions of inquiry, but they sit outside every negotiating room where the scope is contested, and their objection alters no enforcement priority.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, families_of_enforced_disappeared, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, status-blind minimum standard for treatment of persons in armed conflict — wounded, shipwrecked, detained, civilian — so that every belligerent, state or non-state, faces the same floor and every victim holds the same claim regardless of who seizes them. It solves the collective-action problem in which each side's restraint depends on credible expectations about how its own people will be treated and how third parties will judge conduct.
% TRANSFER_FUNCTION: Moves operational discretion from state military and intelligence organs — targeting latitude, interrogation-method choice, detention-registration freedom — into enforceable protected status for persons without combatant qualification; and moves enforcement attention, reputational cost, and accountability exposure onto states, whose conduct is visible and auditable in ways non-state conduct is not.
% ABSENT_VOICES: Irregular detainees and the families of the disappeared hold the constraint's protections without any seat: they cannot address treaty conferences, cannot litigate before most tribunals, and cannot negotiate access terms. Non-state armed groups were absent from the 1949 codification entirely — the Common Article binds parties that never signed anything. Military legal advisors advocating status-based carve-outs speak in domestic and doctrinal fora, but their position is structurally outside this reading's framework rather than represented within it.
% DISAPPEARANCE_RATIONALE: If the universal floor vanished overnight, every active-conflict detention regime would reorganize around status tests: suspected insurgents and terrorism detainees would lose their only protection channel; access negotiations for visiting agencies would lose their legal anchor and revert to per-case charity bargaining; captured state soldiers held by irregulars would depend on each captor's whim; and the tribunal system would lose the applicability baseline its caseload runs on.
% FOUNDING_PROBLEM: Industrial-scale war left wounded, shipwrecked, captured, and civilian persons wholly outside any rule of protection. The 1949 Diplomatic Conference added the Common Article after watching civil wars — Spain, Greece, China — produce mass atrocity precisely because combatant-status criteria collapse in internal conflict, leaving entire categories of persons with no protecting framework at all.
% FOUNDING_PROBLEM_CORROBORATION: Independent commissions of inquiry (Syria, Ukraine), UN human-rights-office casualty and detention reporting, and scholarly codifications of customary practice attest from outside the benefiting parties that the founding problem persists — protection gaps in contemporary conflicts track exactly the status boundaries this reading exists to erase. The ICRC's access statistics also attest the problem, but the ICRC sits partly inside the beneficiary set, so the load-bearing corroboration is the commission and OHCHR record.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.65 because the costs on states are real but bounded: interrogation-method prohibition strikes at specific tradecraft rather than general capability, proportionality discipline constrains strike selection rather than war-fighting as such, and detention registration is administrable. Suppression is 0.55 and is STRUCTURAL, not internalized — treaty law, tribunal jurisdiction, conditionality, and customary crystallization close exits; no interpersonal-internalization mechanism is in play, so no suppression-mechanism omega is needed. Theater is 0.40: signature ceremonies, protocol anniversaries, and selective compliance reporting have grown faster than substance in some theaters, while core functions (mass detention visits, prisoner exchanges, medical evacuations under the emblems) remain genuinely operative. Accessibility collapse is low (0.30) because the status-scoped alternative remains fully articulated and live — major powers maintain it in doctrine and practice. Resistance is high (0.65): sustained state pushback through detention-policy contests, rejection of human-rights-law applicability in hostilities, and non-ratification of the expansionist protocol. The temporal series run on one shared grid and show a ratchet rather than smooth drift: steps at 1977 (expansion protocols), the 1990s tribunal decade, and post-2001 enforcement hardening against status carve-outs. Claim and metrics were authored independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the state military seat the floor computes as enforced extraction on lawful operational freedom; from the civilian and detainee seats the identical structure computes as subsidy — the same clause is a cost and a lifeline depending on which side of the handcuffs a seat stands. The intelligence seat experiences amplified extraction because its professional identity is fused with the prohibited methods: the floor does not merely constrain what the service may do, it delegitimizes what the service is. That identity lock is institutional — the organization has become its interrogation function — and if the frame broke (through accountability-driven generational turnover), the service's resistance would become negotiable and its experienced extraction would drop toward the ordinary constrained-payer level of the uniformed military. The tribunal seat experiences the floor as jurisdiction: scope expansion reads as mandate growth, not cost. One text, four incompatible experiences — that divergence is the measurement this corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map cleanly onto the derivation. Declared victims (state_armed_forces, military_intelligence_services) derive d near the full-target end, amplified by constrained and identity-locked exit respectively — the two institutional payers share a power atom but separate on exit options, which is what lets the derivation distinguish them without overrides. Declared beneficiaries derive low d: civilians and detainees near zero (trapped but subsidized), captured soldiers near zero, agencies near zero with a small positive component for the costs they bear. Non-state armed groups sit at the extreme beneficiary edge — mobile exit plus protection without enforceable reciprocal obligation puts them at the arbitrage-grade end of the scale. The agenda-setter seat collects jurisdiction and caseload rather than paying costs. No directionality overrides were needed: role plus exit separates every seat the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live — armed conflicts persist, non-international conflicts dominate, and status gaps recur whenever states detain persons outside combatant categories — so there is no mandatrophy to resolve and no sunset logic applies. The tangled_rope classification guards against two symmetrical mislabels. Treating the floor as a mountain (self-executing moral law needing no enforcement) is false: the record shows enforcement machinery being built decade over decade, and the suppression series tracks that build-out. Treating it as a snare (pure imposition on states, coordination story as cover) is equally false: the reciprocity benefit for captured state personnel and the civilian-protection function are genuine and observable, and state coalitions retain latent power to renegotiate — which is exactly why suppression stays moderate rather than ratcheting toward totality. The classification preserves both halves because both are structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the geneva_conventions_protective_scope kernel — what would change structurally if a sibling reading governed instead?',
    'No internal resolution: the question resolves only by which reading parties adopt. The sibling files carry their own structural data; cross-reading comparison happens at the corpus level, not inside this story.',
    'Under the state-centric sibling the victim set shrinks to Article 4-qualified combatants, irregular detainees leave the protected set entirely, and extraction on states collapses toward ceremonial levels. Under the hybrid sibling the victim set scales by conflict classification and proportionality gating, placing this reading''s numbers between the extremes. The disagreement is located at exactly one structural element: whether protected status attaches independently of combatant status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega recording this story''s position as the universal_rights_reading and the structural deltas the sibling readings would introduce.').

omega_variable(
    ihrl_fusion_durability,
    'Will the human-rights-law pillar of the universal floor survive major-power rejection of its applicability in hostilities?',
    'Track treaty-body and regional-court jurisprudence against state practice over the next decade: if monitoring bodies continue issuing armed-conflict findings that states absorb into accountability processes, the fusion holds; if a major-power bloc successfully normalizes a wartime exception, the second pillar erodes.',
    'If the fusion fails, the floor reverts to Common-Article-only scope, epsilon on states drops materially, and this reading converges structurally toward the hybrid sibling''s lower band.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ihrl_fusion_durability, empirical, 'Durability of the human-rights-law incorporation that gives the floor its second pillar.').

omega_variable(
    enforcement_asymmetry_persistence,
    'Are the reciprocal obligations the floor places on non-state parties enforceable at all, or is extraction structurally one-sided against states?',
    'Compare accountability outcomes across actor types over successive conflicts: sanction, trial, and reparations rates for non-state violators versus state violators of comparable conduct.',
    'If non-state enforcement stays negligible, the constraint''s extraction is structurally one-sided and the arrangement drifts toward snare-flavored asymmetry despite its genuine coordination function; if accountability channels mature for non-state actors, the tangled_rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_persistence, empirical, 'Whether the burden asymmetry between state and non-state parties is a fixable enforcement gap or a structural constant.').

omega_variable(
    customary_vs_constructed_binding,
    'Does the floor''s binding force on non-consenting actors reflect genuine customary crystallization, or does it rest entirely on contingent enforcement capacity?',
    'Observe floor persistence through enforcement-capacity decay: if compliance norms hold in conflicts where tribunal access and monitoring collapse, crystallization is real; if conduct reverts immediately to status-scoped practice, the binding was enforcement-contingent.',
    'Genuine crystallization gives the floor mountain-like persistence beneath its extractive superstructure; contingent binding means the entire arrangement is reversible by enforcement attrition, changing every long-horizon projection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_vs_constructed_binding, empirical, 'Whether the universal floor is a crystallized customary norm or an enforcement-dependent construction.').

omega_variable(
    theater_drift_diagnosis,
    'Does the rising theater ratio signal Goodhart drift — compliance performance replacing protection substance — or an artifact of expanded reporting obligations?',
    'Correlate the theater series with outcome indicators the theater cannot fake: detention-visit access rates, casualty trends attributable to targeting discipline, disappearance rates. Divergence between reported compliance and outcomes confirms substitution.',
    'Sustained theater above 0.5 with flat or worsening outcomes would date a transition away from the coordination-function reading of this constraint and toward inertial maintenance — the arrangement persisting as performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_diagnosis, empirical, 'Whether growing performative compliance activity is displacing the floor''s substantive protection function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gcps_universal_rights_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.14).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t1949, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t1958, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1958, 0.16).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t1958, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t1970, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1970, 0.19).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t1970, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.23).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t1977, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t1985, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1985, 0.26).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t1985, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t1995, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t1995, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t2001, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2001, 0.34).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t2001, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t2008, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2008, 0.37).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t2008, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t2015, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t2015, observed).
narrative_ontology:measurement(gcps_universal_rights_tr_t2025, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(gcps_universal_rights_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(gcps_universal_rights_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.32).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t1949, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t1958, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1958, 0.34).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t1958, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t1970, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1970, 0.36).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t1970, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.44).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t1977, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t1985, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1985, 0.46).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t1985, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t1995, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1995, 0.53).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t1995, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t2001, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2001, 0.57).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t2001, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t2008, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2008, 0.61).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t2008, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t2015, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t2015, observed).
narrative_ontology:measurement(gcps_universal_rights_be_t2025, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(gcps_universal_rights_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(gcps_universal_rights_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.18).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t1949, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t1958, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1958, 0.2).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t1958, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t1970, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1970, 0.24).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t1970, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.31).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t1977, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t1985, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1985, 0.34).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t1985, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t1995, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1995, 0.44).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t1995, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t2001, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t2001, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t2008, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t2008, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t2015, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t2015, observed).
narrative_ontology:measurement(gcps_universal_rights_su_t2025, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement_basis(gcps_universal_rights_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Geneva protections' decomposes into three structurally distinct scope claims, written as separate files per the epsilon-invariance principle. The state-centric reading is the treaty-text baseline (highest textual confidence, narrowest victim set). The hybrid reading layers classification-gated scaling on top of that baseline. This universal reading removes the status test entirely by fusing human rights law into the floor — the most expansive victim set and the highest extraction on state operations. Family edges run baseline -> hybrid -> universal in order of increasing scope; this file links to both siblings, and its jurisprudential wins (applicability rulings extending protections across conflict classifications, customary-IHL crystallization studies) exert downstream pressure on how the hybrid sibling's floors are drawn without resolving the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
