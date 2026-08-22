% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Cultural Imposition Mandate (Hybrid Scaffolding Reading)
 *   domain: political_history/state_formation
 *
 * SUMMARY:
 *   A consolidating state mandates a civil dress register and backs the
 *   decree with two scaffolds: visible elite modeling (ministers, officers,
 *   and urban notables wearing the register) and an ideological campaign
 *   (press, schools, civilizationist rhetoric) that reframes adoption as
 *   chosen modernity rather than obedience. Displacement is partial: urban
 *   adoption becomes durable and partly self-sustaining, while rural
 *   populations comply minimally and performatively, producing hybrid
 *   practices. This file instantiates ONE reading — the
 *   hybrid_scaffolding_reading — of the contested kernel
 *   legitimacy_of_imposed_practice; the exogenous_override_reading and
 *   endogenous_climb_reading are separate constraints in separate files with
 *   their own epsilon values, not positions inside this one. Epsilon's
 *   referent is the standing scaffolded-imposition arrangement as this
 *   reading assesses it — never the arrangement this reading would endorse
 *   instead. The claimed_type (tangled_rope, from this reading's seat) and
 *   the metrics are independent authored facts; the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - central_state_administration: agenda-setter (institutional/arbitrage) — authors the mandate, runs the ideological campaign, collects fines and recognition
 *   - urban_westernized_elites: primary beneficiary (powerful/mobile) — early adopters whose compliance is cheap and whose gains are status and access
 *   - rural_peasant_households: primary target (powerless/trapped) — bear compliance costs and fines with little access to the scaffolding that softens it elsewhere
 *   - provincial_inspection_officers: enforcement administrators (organized/constrained) — dual-positioned: administer the mandate and skim petty rents from it
 *   - western_goods_merchants: secondary beneficiary (organized/mobile) — sell into a legally manufactured captive market
 *   - traditional_textile_producers: secondary target (moderate/constrained) — lose the customary market the register displaces
 *   - village_notables_and_clergy: excluded voice (organized/trapped) — would contest cost and framing, never seated in the drafting councils
 *   - foreign_recognition_auditors: analytical observer (institutional/analytical) — external graders whose recognition is the mandate's prize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.64).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.55).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Cultural Imposition Mandate (Hybrid Scaffolding Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political_history/state_formation").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, '26575f2c-9002-416f-b903-1e75ea2cb672').
narrative_ontology:cs_kernel_codification('26575f2c-9002-416f-b903-1e75ea2cb672', distributed).
narrative_ontology:cs_authority_grounding('26575f2c-9002-416f-b903-1e75ea2cb672', expertise).
narrative_ontology:cs_interpretation_layer_present('26575f2c-9002-416f-b903-1e75ea2cb672').
narrative_ontology:cs_reading_relation('26575f2c-9002-416f-b903-1e75ea2cb672', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('26575f2c-9002-416f-b903-1e75ea2cb672', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_axiom('26575f2c-9002-416f-b903-1e75ea2cb672', foundational, scaffolded_pull_beats_decree_alone).
narrative_ontology:cs_axiom_status(scaffolded_pull_beats_decree_alone, holdable).
narrative_ontology:cs_axiom_grounding('26575f2c-9002-416f-b903-1e75ea2cb672', scaffolded_pull_beats_decree_alone, empirically_contingent).
narrative_ontology:cs_axiom('26575f2c-9002-416f-b903-1e75ea2cb672', foundational, partial_hybrid_displacement_is_success_condition).
narrative_ontology:cs_axiom_status(partial_hybrid_displacement_is_success_condition, holdable).
narrative_ontology:cs_axiom_grounding('26575f2c-9002-416f-b903-1e75ea2cb672', partial_hybrid_displacement_is_success_condition, empirically_contingent).
narrative_ontology:cs_reference_frame('26575f2c-9002-416f-b903-1e75ea2cb672', scaffolded_transition_reference).
narrative_ontology:cs_drift_state('26575f2c-9002-416f-b903-1e75ea2cb672', comparative_case_synthesis_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('26575f2c-9002-416f-b903-1e75ea2cb672', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernized_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, central_state_administration).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, western_goods_merchants).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_peasant_households).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_textile_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_inspection_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the dress mandate, funds the press and school campaign that frames it as civilization, and deploys provincial inspectors to fine non-compliance. Collects fine revenue and the legibility and recognition gains the mandate was built to produce. Can recalibrate or relax enforcement from the center at will.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, central_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt the mandated dress early and visibly, staff the modeling roles in ministries and parade grounds, and gain diplomatic access, commercial credit, and distinction from rural compatriots. Compliance is cheap for them: goods and tailors are local, and the ideological framing matches their self-description.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_westernized_elites, beneficiary,
    powerful, biographical, mobile, national).

% Owe compliance they can barely fund: a mandated garment set can consume a season's surplus, and fines compound the burden. They receive little of the newspaper and school messaging that makes compliance feel chosen elsewhere, and they manage inspections by keeping one mandated item to wear on market and inspection days.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_peasant_households, payer,
    powerless, generational, trapped, local).

% Administer the mandate locally: conduct inspections, levy fines, and file compliance statistics upward. Fine quotas and promotion criteria tie their careers to reported success, and petty fee-taking at inspections supplements salaries.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_inspection_officers, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, provincial_inspection_officers, beneficiary).

% Supply the mandated garments and accessories. Demand is legally manufactured — every subject needs the registered items — so they sell into a captive market at premium prices and lobby against any relaxation of the register.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, western_goods_merchants, beneficiary,
    organized, biographical, mobile, national).

% Lose their customary market as demand shifts to the mandated register. Some retool to produce hybrid or licensed items; the rest watch orders decline and workshops close across the weaving districts.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, traditional_textile_producers, payer,
    moderate, generational, constrained, regional).

% Would contest both the cost and the framing — that inherited dress is shame to be discarded — but were never seated in the drafting councils. Their objections reach the center only as disorder reports filed by the inspectors policing them.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, village_notables_and_clergy, excluded,
    organized, biographical, trapped, regional).

% Treaty partners, consuls, and diplomatic corps whose recognition is the prize the mandate pursues. They observe compliance externally, grade the polity's civility in dispatches, and their verdicts feed back into the state's own narrative of success.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, foreign_recognition_auditors, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, central_state_administration).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the polity's public presentation around one civil dress register, giving administration a single legible classificatory surface and giving treaty partners a recognizable marker of membership; solves the recognition-legibility problem once, centrally, instead of region by region.
% TRANSFER_FUNCTION: Moves compliance costs — garment purchases, fines, habit-change labor — from the general population (disproportionately rural households) into urban garment markets and the state treasury; moves status, administrative legibility, and international recognition upward to the state and Western-facing elites.
% ABSENT_VOICES: Village notables, clergy, and rural women — whose household budgets absorb the garment costs — were absent from the drafting councils; their objection (that the mandate taxes subsistence and brands inherited practice as shame) entered the record only as resistance to be managed, filtered through the inspectors who reported it.
% DISAPPEARANCE_RATIONALE: Enforcement stops, fines cease, and village dress drifts back toward prior practice within a generation; urban elites keep the adopted markers voluntarily because their pull has become partly endogenous; the state loses its legibility register and must rebuild recognition claims; the urban garment sector contracts; the national-modernity narrative loses its daily wearable proof.
% FOUNDING_PROBLEM: A newly consolidating state needed visible, uniform marks of membership in the recognized family of civilized nations and a single administrative classificatory surface; inherited regional dress read as foreign or backward to treaty partners and resisted centralized enumeration.
% FOUNDING_PROBLEM_CORROBORATION: Foreign diplomatic archives and treaty ratification records — outside the benefiting parties — attest that recognition and legibility were substantially secured within the first decade; rural petition files and contemporaneous consular and missionary reports attest that enforcement outlasted that payoff. Only the state's own press attests a continuing live need.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.64 because compliance costs are decoupled from rural capacity: a mandated garment set consumes a season's surplus where the scaffolding (tailors, wages, messaging) is thinnest, and fines compound the burden. Suppression is 0.55 — real enforcement (inspectors, fines, public shaming) but materially lower than a pure-decree regime would need, because the ideological campaign carries part of the load; the suppression_requirement series is authored precisely because this story tracks enforcement-capacity change: coercive demand falls (0.74 to 0.55) as the scaffolding matures, the substitution dynamic this reading is about. Theater is 0.31 and rising: elite modeling and the press campaign are functional scaffolding, but a growing share of activity is inspection-day compliance performance and staged modernity display. Accessibility_collapse is 0.45 — alternatives (full inherited dress) survive privately and in hybrid forms; the register closes public space, not private practice. Resistance is 0.55 — evasion, petition, occasional riot over enforcement excesses. All three series run on one shared time grid ({0,8,16,24,32,40}) so every metric is authored at every examined point; the trajectories are monotonic, not cyclical, so no cycle documentation applies.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is nation-building coordination succeeding: legibility achieved, recognition won, adoption spreading. From the elite beneficiary seat it is voluntary modernity — the framing matches their self-description, so the mandate reads as confirmation, not constraint. From the trapped rural seat the same structure is a punitive tax on poverty administered by strangers. The inspector seat experiences it as quota metrics and salary supplementation. The foreign auditor seat sees only the external surface — compliance rates and dispatch-grade civility — and cannot see the hybrid masking underneath. The engine derives these divergences from power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: urban_westernized_elites (mobile exit, direct status gains) sit nearest the beneficiary end; central_state_administration designs, enforces, and collects, placing it beneficiary-side despite its enforcement labor; western_goods_merchants collect legally manufactured demand. Victim declarations drive high directionality: rural_peasant_households (trapped, local, bearing the transfer) sit nearest the full-target end; traditional_textile_producers (constrained, losing their market) sit high as well. Provincial_inspection_officers are dual-positioned — the secondary beneficiary role captures their fine-skimming, so the derivation lands them mid-range without an override. No directionality_overrides are authored: every seat's derived d follows from its declarations plus exit options, and no seat exhibits the capture or indirect-benefit distortion that would warrant overriding the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents symmetric mislabelings. Reading the arrangement as pure snare would erase its genuine coordination function — the legibility register and recognition channel are real, elite uptake is partly sincere, and the ideological scaffolding does reduce coercive demand (visible in the falling suppression series). Reading it as pure rope would erase the asymmetric extraction — the rural majority bears the transfer while excluded from the scaffolding infrastructure that makes compliance affordable elsewhere, and persistence depends on active enforcement. On the genealogy: the founding problem (recognition plus legibility) was substantially solved within the first decade per outside attestation, yet the arrangement persists with rising theater (0.14 to 0.31) — the classic drift signature. Authored status is contested rather than dead because the state still cites live unity needs and the parties dispute the record; the mismatch consumer should watch this pair: if status resolves to dead while the verdict stays world_rearranges, the zombie flag fires, and the theater trajectory indicates the piton path runs through ceremonial enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'Which reading of the legitimacy_of_imposed_practice kernel correctly locates the causal weight behind the observed partial displacement — decree force, ideological scaffolding, or endogenous adoption?',
    'Paired cross-domain comparison holding state capacity and timing constant: the calendar reform run as pure decree versus the dress reform run with scaffolding isolates scaffolding as the varying factor; further policy-domain replicates tighten the estimate.',
    'If scaffolding adds nothing over decree, this story collapses toward the exogenous_override sibling; if decree adds nothing over organic diffusion, toward the endogenous_climb sibling; epsilon and classification follow whichever reading survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'Committer-frame omega: this story instantiates one reading of a contested kernel; the sibling readings are separate constraints, and reading assignment is the open variable.').

omega_variable(
    quasi_endogenous_pull_authenticity,
    'How much urban-elite adoption survives when enforcement lapses — is the pull genuinely endogenized or merely sanction-maintained?',
    'Adoption-persistence measurement across administrative-slack windows when inspections lapse; comparison of stated preference in elite memoirs and correspondence against revealed wardrobe behavior.',
    'If adoption is sanction-maintained, effective suppression is understated and the arrangement sits nearer the snare end; if durable, the coordination component is real and the rope-side weighting rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quasi_endogenous_pull_authenticity, empirical, 'Whether the quasi-endogenous pull this reading posits is authentic internalization or latent coercion.').

omega_variable(
    rural_cost_incidence,
    'What share of disposable income do mandated-garment costs and fines consume in rural households versus urban adopters?',
    'Household budget inventories and regional price and wage series for the mandated garment set, compared across urban and rural districts.',
    'Higher rural incidence raises effective extraction on the trapped seat and pushes the computed classification toward snare; rough parity supports the tangled_rope reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rural_cost_incidence, empirical, 'Distributional incidence of compliance costs across the beneficiary/target divide.').

omega_variable(
    hybrid_practice_interpretation,
    'Do hybrid practices indicate successful synthesis (the register absorbing prior practice) or failed displacement masked by inspection-cycle compliance?',
    'Longitudinal observation away from inspection cycles: wardrobe-continuity studies in villages between enforcement visits, and merchant ledgers distinguishing everyday from inspection-day sales.',
    'The synthesis reading lowers theater_ratio and strengthens the coordination function; the masking reading raises theater and signals piton drift through ceremonial enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_practice_interpretation, empirical, 'Interpretation of hybrid practices: synthesis versus performed compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(legi_tr_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(legi_tr_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(legi_tr_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 8, 0.57).
narrative_ontology:measurement(legi_be_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(legi_be_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement(legi_su_t8, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(legi_su_t16, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(legi_su_t32, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: the colloquial label 'did the imposed practice take?' covers three structurally distinct claims with distinct epsilon values and distinct beneficiary/victim structures, so the kernel decomposes into three stories. This (hybrid_scaffolding) story links to both siblings via affects_constraints. Direction of influence: the calendar case (pure decree, failed) is the exogenous_override sibling's home instance and is cited BY this reading as evidence that decree alone fails; the dress case (scaffolded, partial success) is this reading's home instance and pressures the endogenous_climb sibling by demonstrating manufactured quasi-internalization. Each file documents the decomposition in its own dual_formulation_note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
