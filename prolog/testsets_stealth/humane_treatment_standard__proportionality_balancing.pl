% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing Standard (Detainee Dignity vs. Security Needs)
 *   domain: legal/international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions anchors a minimum
 *   humane-treatment floor for non-international armed conflict. This story
 *   instantiates one reading of that text — proportionality_balancing — under
 *   which the floor's application is not categorical but adjudicated: courts
 *   weigh detainee dignity against asserted security needs case by case, and
 *   treatment permissibility is whatever the balance yields. The arrangement
 *   under assessment is that adjudicative regime itself — courts as
 *   gatekeepers, interrogators under moderate constraint with procedural
 *   safeguards, detainees holding protection whose strength is re-decided in
 *   every case. The ε authored here refers to that standing arrangement,
 *   assessed by this reading's own lights, and never to the arrangements the
 *   sibling readings would install; those are separate constraints (see
 *   kernel_context and network). Claim and metrics are authored
 *   independently: the claim (tangled_rope) asserts that genuine coordination
 *   and real asymmetric extraction coexist in the same structure, while the
 *   metrics describe the arrangement's actual operation — the engine computes
 *   per-seat classifications from the structural data and measures any
 *   divergence.
 *
 * KEY AGENTS:
 *   - detainees_in_custody: primary target (powerless/trapped) — bears contingent protection; dignity weighed and potentially yielded case-by-case
 *   - reviewing_judiciary: agenda-setter and institutional beneficiary (institutional/constrained) — runs the balance, collects adjudicative authority
 *   - state_security_services: operative beneficiary with real cost exposure (powerful/constrained) — receives the lawful latitude the balance preserves, pays in oversight and litigation risk
 *   - executive_governments: secondary beneficiary (institutional/constrained) — gains a compliance path preserving operational flexibility
 *   - human_rights_treaty_bodies: excluded categorical claimants (institutional/constrained) — hold the absolute-floor reading, no procedural seat inside the balance
 *   - detainee_advocacy_organizations: excluded advocates (moderate/mobile) — litigate across fora for the categorical reading their clients cannot themselves reach
 *   - icrc_detention_visitors: observer (organized/mobile) — monitors treatment inside the facilities; leverage is access and its withdrawal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.52).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing Standard (Detainee Dignity vs. Security Needs)").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "legal/international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '76d36cce-97d2-45a7-8287-c4986ea8fa0f').
narrative_ontology:cs_kernel_codification('76d36cce-97d2-45a7-8287-c4986ea8fa0f', fixed_text).
narrative_ontology:cs_authority_grounding('76d36cce-97d2-45a7-8287-c4986ea8fa0f', lineage).
narrative_ontology:cs_interpretation_layer_present('76d36cce-97d2-45a7-8287-c4986ea8fa0f').
narrative_ontology:cs_reading_relation('76d36cce-97d2-45a7-8287-c4986ea8fa0f', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('76d36cce-97d2-45a7-8287-c4986ea8fa0f', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_axiom('76d36cce-97d2-45a7-8287-c4986ea8fa0f', foundational, proportional_balancing_governs_treatment).
narrative_ontology:cs_axiom_status(proportional_balancing_governs_treatment, holdable).
narrative_ontology:cs_axiom_grounding('76d36cce-97d2-45a7-8287-c4986ea8fa0f', proportional_balancing_governs_treatment, conventional).
narrative_ontology:cs_axiom('76d36cce-97d2-45a7-8287-c4986ea8fa0f', foundational, dignity_and_security_both_genuine_weights).
narrative_ontology:cs_axiom_status(dignity_and_security_both_genuine_weights, holdable).
narrative_ontology:cs_axiom_grounding('76d36cce-97d2-45a7-8287-c4986ea8fa0f', dignity_and_security_both_genuine_weights, deontological).
narrative_ontology:cs_reference_frame('76d36cce-97d2-45a7-8287-c4986ea8fa0f', judicial_proportionality_gatekeeping).
narrative_ontology:cs_drift_state('76d36cce-97d2-45a7-8287-c4986ea8fa0f', contemporary_absolute_norm_hardening, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('76d36cce-97d2-45a7-8287-c4986ea8fa0f', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, reviewing_judiciary).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_security_services).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, executive_governments).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detainees_in_custody).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, state_security_services).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, executive_governments).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, ihl_proportionality_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__proportionality_balancing, judicial_gatekeeping_over_interrogation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons held in custody in a non-international armed conflict whose treatment falls to be assessed under the standard. What flows to them is protection whose strength is re-decided in every adjudication: their dignity is weighed against asserted security needs, and where security is judged weightier their protection yields for that case. They cannot leave custody to escape the arrangement, and many cannot effectively appear in it — incommunicado detention, restricted counsel access, and classified evidence keep the people being weighed away from the proceedings that weigh them.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainees_in_custody, payer,
    powerless, immediate, trapped, global).

% The courts that conduct the weighing. Case by case they decide whether specific treatment practices survive the balance between dignity and security, and their precedents supply the standard's content over time. The arrangement concentrates interrogation governance in their hands — a substantial institutional acquisition — while they carry its docket, its disclosure fights, and the political friction of second-guessing security judgments. Their alternative is to abandon weighing for categorical rules, which would surrender the gatekeeping position they occupy.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, reviewing_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, reviewing_judiciary, beneficiary).

% The services that conduct interrogations under the standard. For them the arrangement preserves a lawful margin: methods a categorical floor would erase remain available where the balance permits, which is the arrangement's operative product. The price is procedural — oversight, disclosure obligations, and case-by-case litigation in which any method's legality can be relitigated — plus the standing uncertainty that today's lawful method is tomorrow's violation. Pressing for a necessity-override regime instead is available only through political channels, not unilateral action.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_services, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, state_security_services, payer).

% The states party to the conventions, whose officials defend practices in court and absorb adverse rulings. The arrangement gives them a compliance path that keeps operational flexibility in security emergencies — a position between categorical prohibition and open self-judgment — at the cost of permanent contestability: every practice can be challenged before their own courts and international bodies, with reputational and legal exposure on each adverse outcome. Leaving the arrangement would mean denouncing or openly violating norms widely treated as customary, at prohibitive standing cost.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, executive_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, executive_governments, payer).

% International monitoring bodies that read the humane-treatment floor categorically: certain protections are non-derogable in every circumstance, and weighing them against security is itself the error. Inside a framework built on weighing, their premise has no procedural seat — a balance presupposes that both sides can be traded, which their categorical claim denies. They review state reports, issue pronouncements, and press for absolute floors, but they do not decide the cases.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, human_rights_treaty_bodies, excluded,
    institutional, generational, constrained, global).

% Litigating and campaigning organizations acting for detainees. They hold more mobility than their clients — able to shift between domestic courts, regional human rights courts, treaty bodies, and public advocacy — but in any forum built on weighing they must argue their clients' dignity as a weight rather than a floor, a framing they regard as the harm itself.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detainee_advocacy_organizations, excluded,
    moderate, biographical, mobile, global).

% ICRC delegates who visit places of detention, register detainees, and report confidentially on conditions and treatment. They see the arrangement's operation from inside the facilities it governs, with access negotiated state by state; their instruments are private remonstration and the credible threat of withdrawn visits, not adjudication.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, icrc_detention_visitors, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, state_security_services).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the recurring conflict between detainee dignity and state security needs through a single adjudicable decision procedure: courts weigh both values case by case, producing permissibility determinations that states can anticipate and detainees can contest, keeping interrogation inside a common legal framework instead of executive self-judgment or categorical prohibition.
% TRANSFER_FUNCTION: Moves decision authority over interrogation conduct from the executive and security services to the judiciary, one case at a time; and moves a margin of detainee protection from categorical guarantee to contingent, case-determined status. The operational latitude thereby preserved flows to the security services, priced in procedural oversight and litigation exposure.
% ABSENT_VOICES: Absolute-prohibition claimants — human rights treaty bodies and a substantial body of doctrine holding dignity non-negotiable — have no seat inside a weighing framework: their categorical premise is incommensurable with the procedure by design. Detainees themselves are frequently absent from the adjudication that decides them: incommunicado detention, classified evidence, and standing barriers keep the people the balance weighs from appearing before it.
% DISAPPEARANCE_RATIONALE: If the balancing arrangement vanished overnight, treatment governance would reorganize around one of the sibling readings: courts would either apply categorical floors (states would resist, driving practice into unadjudicated gray zones) or defer to security necessity (detainee protection would become wholly executive-determined). The adjudicative infrastructure — precedent, procedural safeguards, the gatekeeping role — would dissolve, and the judiciary would lose the interrogation-governance function the arrangement confers.
% FOUNDING_PROBLEM: Common Article 3 was drafted to set a minimum humanitarian floor for non-international armed conflict, where the wounded, detained, and captured had previously stood outside any legal protection. The proportionality reading specifically addresses the problem the categorical floor created in practice: states facing genuine security emergencies found the absolute reading unadministrable and either ignored it or sought escape routes; this reading was built to make the floor enforceable by giving courts a decision procedure both values could live inside.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC commentary on the 1949 Diplomatic Conference attests the founding protection problem from outside the benefiting parties. On the specific administrability premise — that the absolute floor is unworkable in security emergencies — corroboration splits: human rights treaty bodies, outside the benefiting set, attest the opposite, holding the categorical floor workable and rejecting the balancing premise outright. No source outside the benefiting parties attests that the administrability problem specifically (as distinct from the general protection problem) remains live.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (the interval's end state): the arrangement's operative effect is that detainee protection is contingent — dignity enters adjudication as a weight that security need can outweigh — and the class bearing that contingency is the least powerful party while the weighing is done by others. Suppression is authored at 0.52 as a raw structural property, unscaled by power or scope in the engine's arithmetic: detainees cannot exit custody, categorical objections are rejected by the framework's own design, and participation presupposes access many detainees lack. Theater_ratio is 0.28 — the balancing is mostly real adjudication with binding outcomes, but a growing share of procedural activity legitimates practice more than it constrains it. Accessibility_collapse is 0.45: the categorical alternative stays live in other fora and doctrines; the arrangement forecloses it domestically without erasing it. Resistance is 0.6: treaty bodies, advocacy organizations, and parts of the academy actively contest the weighing premise, while states resist the oversight it imposes. The three temporal series share one grid (1949/1964/1979/1994/2009/2024): base_extractiveness rises as judicial activation makes the arrangement operative — before activation it governed little and extracted little; suppression_requirement is authored because this story specifically tracks enforcement-capacity change — the machinery (review jurisdiction, disclosure, procedural safeguards) built up through the 1990s-2000s litigation wave and eased slightly as absolute-norm hardening moved the torture core out of the balance's reach. Receipt surface: the value the balance preserves — lawful operational margin — accrues to the services that exercise it, so gain_flow names state_security_services. Fixing cost: the seat that could replace weighing with categorical rules is the judiciary, and doing so would cost it the gatekeeping function the arrangement confers while driving state practice into unadjudicated gray zones — cost to the fixer exceeds what the fixer bears, hence prohibitive. Coalition note: the victim class cannot aggregate internally — custody itself prevents coordination among detainees — and external aggregation runs through advocacy organizations that are mobile but hold no seat in the adjudication.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the bench, the arrangement is the judiciary's own craft — a doctrine it built, staffs, and benefits from; coordination-forward, low effective extraction. From the cell, the same procedure is the machinery that prices dignity: protection that can be outweighed is protection on approval; extraction-forward, near-full target. From the interrogation unit, it is managed latitude — oversight accepted as the price of methods a categorical floor would erase. From the treaty bodies' seat, the arrangement is not a balance at all but the institutionalization of commensurability, which they deny exists. Same-level divergence: the reviewing judiciary and the security services are both state institutions at comparable formal standing, yet they experience the arrangement oppositely because it assigns them different structural positions — the bench decides, the services are decided about; the bench collects authority, the services collect margin and pay exposure. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (reviewing_judiciary, state_security_services, executive_governments) derive low directionality for those seats; the victim declaration (detainees_in_custody) combined with trapped exit derives directionality near the full-target end. One override: the powerful atom — held in this story only by state_security_services — is set to 0.28 because structural derivation would read a beneficiary at that power and exit profile near the pure-beneficiary end (~0.15), while the services bear real procedural, disclosure, and litigation costs that make them partly paying; the override separates an operative beneficiary-with-cost-exposure from a pure collector. The judiciary's beneficiary position is left to derivation: its gain (adjudicative authority) is real but it also carries the arrangement's operating burden, netting near the mild-beneficiary range. Detainee directionality is not overridden: trapped exit and victim position already place it at the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an absolute humanitarian floor that states in genuine security emergencies found unadministrable — is contested rather than dead: prohibition advocates hold the floor administrable and non-negotiable, and the absolute-norm hardening of recent decades is their position gaining terrain. The tangled_rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure coordination would erase the detainees' contingent-protection cost: the price the balance charges is paid by the one party with no vote in the weighing. Reading it as pure extraction would erase the genuine coordination function: an adjudicable decision procedure is a real good that both domestic branches and the treaty system can inhabit, and the categorical alternative remains live elsewhere rather than suppressed out of existence. The classification also keeps the drift question open: if the balancing_constraint_vs_ratification omega resolves against the reading — courts ratifying rather than constraining — theater_ratio rises and the structure drifts toward pure extraction with the judiciary as legitimation cover; the omegas track exactly that contingency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is proportionality_balancing the correct instantiation of the humane_treatment_standard kernel, or does Common Article 3''s categorical floor language (''violence ... prohibited at any time and in any place whatsoever'') commit the kernel to the absolute_prohibition reading?',
    'Drafting-history analysis of the 1949 Diplomatic Conference plus comparative adjudication: if the floor language is held non-derogable and non-balancable, this reading fails; if treatment questions are held inherently case-relative, it stands.',
    'If absolute_prohibition is the true reading, this constraint''s structure dissolves — courts lose the balancing seat, detainees'' protection becomes categorical, and the extraction measured here relocates to whatever enforcement gap remains. If contextual_necessity, gatekeeping leaves the courts and the contingency becomes executive-determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading or a sibling correctly instantiates the Common Article 3 kernel.').

omega_variable(
    commensurability_premise_contest,
    'The entire disagreement between this reading and its siblings is located in one premise: that detainee dignity and security needs are commensurable — weighable on a common scale by a court. Is that premise sound, or is dignity incommensurable such that balancing is a category error for the core cases?',
    'Doctrinal and philosophical adjudication: the absolute-norm hardening (CAT Article 2(2), absolute Article 3 jurisprudence) is in effect a ruling that commensurability fails for the torture core; the open question is where the commensurable margin ends.',
    'If incommensurability wins for the core, this reading''s scope contracts to the margin and its extraction structure persists only there; if commensurability holds, the arrangement stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commensurability_premise_contest, conceptual, 'Whether the commensurability premise that makes balancing coherent is sound.').

omega_variable(
    balancing_constraint_vs_ratification,
    'In adjudicated cases, does the proportionality balance actually constrain interrogation practice, or does it predominantly ratify security-service determinations under procedural cover?',
    'Cross-jurisdictional case-outcome analysis: the rate at which courts find treatment impermissible once security needs are asserted, and whether outcomes track the services'' own ex ante assessments.',
    'If ratification dominates, theater_ratio is understated and the structure drifts toward pure extraction with the judiciary as legitimation cover; if constraint is genuine, the coordination function is real and the tangled_rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_constraint_vs_ratification, empirical, 'Whether judicial balancing constrains interrogation practice or ratifies it.').

omega_variable(
    detainee_litigation_access,
    'Do detainees in practice have the access — counsel, unclassified evidence, standing — that case-by-case adjudication presupposes, or is the balancing seat structurally unavailable to the people it decides?',
    'Empirical audit of counsel access and evidentiary disclosure rates across adjudicated treatment cases, including incommunicado detention rates at time of adjudication.',
    'If access is systematically denied, the procedural safeguards are cover and suppression is understated — detainees'' effective directionality rises and the measured extraction understates the real one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detainee_litigation_access, empirical, 'Whether detainees can actually occupy the adjudicative seat the arrangement assigns them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__proportionality_balancing, theater_ratio, 1949, 0.08).
narrative_ontology:measurement_basis(huma_tr_t1949, observed).
narrative_ontology:measurement(huma_tr_t1964, humane_treatment_standard__proportionality_balancing, theater_ratio, 1964, 0.12).
narrative_ontology:measurement_basis(huma_tr_t1964, observed).
narrative_ontology:measurement(huma_tr_t1979, humane_treatment_standard__proportionality_balancing, theater_ratio, 1979, 0.18).
narrative_ontology:measurement_basis(huma_tr_t1979, observed).
narrative_ontology:measurement(huma_tr_t1994, humane_treatment_standard__proportionality_balancing, theater_ratio, 1994, 0.22).
narrative_ontology:measurement_basis(huma_tr_t1994, observed).
narrative_ontology:measurement(huma_tr_t2009, humane_treatment_standard__proportionality_balancing, theater_ratio, 2009, 0.27).
narrative_ontology:measurement_basis(huma_tr_t2009, observed).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__proportionality_balancing, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(huma_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1949, 0.18).
narrative_ontology:measurement_basis(huma_be_t1949, observed).
narrative_ontology:measurement(huma_be_t1964, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1964, 0.26).
narrative_ontology:measurement_basis(huma_be_t1964, observed).
narrative_ontology:measurement(huma_be_t1979, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1979, 0.36).
narrative_ontology:measurement_basis(huma_be_t1979, observed).
narrative_ontology:measurement(huma_be_t1994, humane_treatment_standard__proportionality_balancing, base_extractiveness, 1994, 0.47).
narrative_ontology:measurement_basis(huma_be_t1994, observed).
narrative_ontology:measurement(huma_be_t2009, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement_basis(huma_be_t2009, observed).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__proportionality_balancing, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(huma_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1949, 0.1).
narrative_ontology:measurement_basis(huma_su_t1949, observed).
narrative_ontology:measurement(huma_su_t1964, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1964, 0.15).
narrative_ontology:measurement_basis(huma_su_t1964, observed).
narrative_ontology:measurement(huma_su_t1979, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1979, 0.26).
narrative_ontology:measurement_basis(huma_su_t1979, observed).
narrative_ontology:measurement(huma_su_t1994, humane_treatment_standard__proportionality_balancing, suppression_requirement, 1994, 0.38).
narrative_ontology:measurement_basis(huma_su_t1994, observed).
narrative_ontology:measurement(huma_su_t2009, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2009, 0.55).
narrative_ontology:measurement_basis(huma_su_t2009, observed).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__proportionality_balancing, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(huma_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Common Article 3 humane treatment' decomposes into three structurally distinct readings of the humane_treatment_standard kernel — absolute_prohibition, contextual_necessity, and this proportionality_balancing reading — because each instantiates a different decision procedure with a different victim structure and a different ε. This story's ε refers only to the adjudicated-balance arrangement; the siblings are separate files linked here. The upstream treaty text is cited by all three as authority; the absolute-norm hardening downstream of the prohibition reading is eroding this reading's reference frame (see cs_structure.drift_state).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__proportionality_balancing, powerful, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
