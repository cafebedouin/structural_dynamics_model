% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition Reading (Non-Derogable Humane Treatment Floor)
 *   domain: legal/humanitarian/state-security
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions — the same words in all
 *   four conventions — sets minimum treatment requirements for persons hors
 *   de combat in every armed conflict, with no derogation clause: no
 *   emergency, necessity, or reciprocity condition may license crossing the
 *   threshold. This story authors ONE reading of that kernel: the
 *   absolute_prohibition reading, under which detainees enter the full
 *   rights-holder set, state interrogation methods are constrained
 *   absolutely, and no security exception exists. The sibling readings
 *   (contextual_necessity, proportionality_balancing) are separate
 *   constraints in separate files; the disagreement between them lives in
 *   derogability, and this file prices only this reading's structure. KEY
 *   AGENTS (by structural relationship): - detainees_hors_de_combat: Primary
 *   beneficiary (powerless/trapped) — protection flows to them directly -
 *   capturing_state_detention_authorities: Primary target
 *   (institutional/constrained) — bear compliance, inspection, and
 *   prosecution burdens; reciprocity gains thin in asymmetric conflict -
 *   military_intelligence_interrogators: Secondary target
 *   (organized/constrained) — method sets bounded, careers shaped by the
 *   boundary - nonstate_armed_groups: Nominal target, effective light bearer
 *   (moderate/constrained) — bound without consent, enforcement episodic -
 *   humanitarian_monitoring_bodies: Beneficiary-administrator
 *   (organized/mobile) — access rights embedded in the architecture; receipt
 *   seat for institutional gains - international_accountability_bodies:
 *   Agenda-setter (institutional/analytical) — define operative meaning,
 *   indict violators - human_rights_advocacy_networks: Secondary beneficiary
 *   (organized/mobile) — standing and casework flow from a litigable absolute
 *   line - ihl_legal_academy: Analytical observer — sees the full structure
 *   and the inter-reading contest
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.4).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.8).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.4).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition Reading (Non-Derogable Humane Treatment Floor)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "legal/humanitarian/state-security").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, 'd5768208-4e50-442f-a301-bcb6706a1d23').
narrative_ontology:cs_kernel_codification('d5768208-4e50-442f-a301-bcb6706a1d23', fixed_text).
narrative_ontology:cs_authority_grounding('d5768208-4e50-442f-a301-bcb6706a1d23', lineage).
narrative_ontology:cs_interpretation_layer_present('d5768208-4e50-442f-a301-bcb6706a1d23').
narrative_ontology:cs_reading_relation('d5768208-4e50-442f-a301-bcb6706a1d23', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('d5768208-4e50-442f-a301-bcb6706a1d23', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('d5768208-4e50-442f-a301-bcb6706a1d23', foundational, prohibition_without_exception).
narrative_ontology:cs_axiom_status(prohibition_without_exception, holdable).
narrative_ontology:cs_axiom_grounding('d5768208-4e50-442f-a301-bcb6706a1d23', prohibition_without_exception, deontological).
narrative_ontology:cs_axiom('d5768208-4e50-442f-a301-bcb6706a1d23', foundational, detainee_full_rightsholder_status).
narrative_ontology:cs_axiom_status(detainee_full_rightsholder_status, holdable).
narrative_ontology:cs_axiom_grounding('d5768208-4e50-442f-a301-bcb6706a1d23', detainee_full_rightsholder_status, deontological).
narrative_ontology:cs_reference_frame('d5768208-4e50-442f-a301-bcb6706a1d23', nonderogable_absolute_floor).
narrative_ontology:cs_drift_state('d5768208-4e50-442f-a301-bcb6706a1d23', post_war_on_terror_contest, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('d5768208-4e50-442f-a301-bcb6706a1d23', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees_hors_de_combat).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, humanitarian_monitoring_bodies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, capturing_state_detention_authorities).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, military_intelligence_interrogators).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, nonstate_armed_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, nonstate_armed_groups).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, equal_application_of_ihl_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogable_minimum_standard_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fighters rendered incapable of fighting and civilians interned in armed conflict, in whose hands the minimum standard lands as direct protection: food, water, humane quarters, trial guarantees, freedom from coercive questioning. They consented to nothing and chose nothing; the standard reaches them wherever they are held, and their only leverage is whatever visiting monitors and courts can project on their behalf. Leaving captivity is not available to them; the floor beneath their treatment holds until release.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees_hors_de_combat, beneficiary,
    powerless, biographical, trapped, global).

% Ministries, commands, and prison administrations that run detention operations under the standard's jurisdiction. They carry inspection obligations, prosecution duties, training costs, and the operational loss of coercive questioning techniques. Their own soldiers and citizens are also the population the standard protects when captured by others, though in recent decades they capture far more than they are captured. Formal withdrawal from the conventions is legally open on one year's notice and politically unusable; their realistic maneuver room is interpretive — arguing about what the text covers — not physical exit.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, capturing_state_detention_authorities, payer,
    institutional, generational, constrained, global).

% Professionals whose methods the standard bounds. They hold operational judgments that coercive questioning yields time-critical intelligence, and the prohibition prices those judgments out of the lawful toolkit regardless of circumstance. Career risk runs both directions: using forbidden methods invites prosecution; refusing field demands creates friction with commanders. Their preferred escape is doctrinal rather than physical — shifting the governing interpretation toward necessity or balancing.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, military_intelligence_interrogators, payer,
    organized, immediate, constrained, national).

% Insurgent and militia organizations bound by the common standard without ever having signed anything — the text reaches them through its identical presence in every convention and its customary status. When their members are captured they hold protection claims they never purchased; when they detain others they owe duties many disavow. Enforcement against them is sporadic and usually deferred to post-conflict processes, so the routine cost of compliance sits lighter on them than on uniformed services, while battlefield reciprocity still shapes how their detainees fare.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, nonstate_armed_groups, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, nonstate_armed_groups, beneficiary).

% The ICRC and peer organizations whose access rights, mandate language, and funding relevance are written into the standard's architecture. They visit places of detention, register grievances confidentially, publish findings when access fails, and disseminate the rules to armed forces. They can withdraw from countries that bar them — a lever used sparingly because presence is their product.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, humanitarian_monitoring_bodies, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, humanitarian_monitoring_bodies, agenda_setter).

% Treaty-body committees, ad hoc tribunals, the ICC, and UN inquiry commissions that define what the minimum standard requires, prosecute outrages, and issue findings naming violators. They set the operative meaning of the text through jurisprudence; states respond with cooperation, defiance, or forum-shifting. They hold no territory and no force — their instruments are interpretation, indictment, and publicity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_accountability_bodies, agenda_setter,
    institutional, generational, analytical, global).

% NGOs, bar associations, and documentation projects whose casework, reporting, and litigation agendas draw standing and resources from an absolute, litigable floor. They document violations, brief prosecutors, and campaign against interpretive retreat; a softer, negotiable standard would shrink the clear line around which they organize.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_advocacy_networks, beneficiary,
    organized, biographical, mobile, global).

% Scholars, military lawyers, and commentators who map the standard's reach, argue its interpretation in journals and doctrine manuals, and train the officers who implement it. They see the whole structure — text, case law, state practice, and the contest between readings — and their analyses feed both accountability bodies and the governments testing the standard's limits.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, ihl_legal_academy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, humanitarian_monitoring_bodies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared minimum floor for treatment of persons hors de combat in every armed conflict — international or internal, declared or not — so that each party knows in advance how its own captured personnel and civilians will be treated and how it must treat those it captures; removes treatment severity from the bargaining space between adversaries; and gives neutral intermediaries a fixed textual anchor for access and complaint.
% TRANSFER_FUNCTION: Moves no money or goods. It converts discretionary power over captive bodies into bounded duty: permitted interrogation technique sets are narrowed, detention conditions are specified, and prosecutorial exposure is created for crossings. In practice the burden of honoring the duty concentrates on parties with disciplined chains of command and treaty exposure, while the protection it purchases extends to every detained person regardless of which side holds them.
% ABSENT_VOICES: Detained persons themselves never sat at the diplomatic conferences where the standard was drafted and revised — their interests arrive through ICRC delegates and protecting powers, filtered twice. Non-state armed group leadership was never consulted despite being bound; some reject the binding outright as imposition by states. Frontline interrogators' operational knowledge reaches negotiation only through state delegations that historically argued the security side. Victims of violations in closed conflicts (Syrian detention facilities, besieged-city internments) have no forum unless an ad hoc mechanism is created for them.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would reprice captivity immediately: treatment of detainees would track each holder's incentives — intelligence demand, revenge cycles, logistics of neglect — with nothing anchoring a floor. The access-and-reporting machinery (visits, registrations, confidential representations) loses its textual warrant; tribunals lose the anchor offense category; reciprocal expectations between adversaries collapse into tit-for-tat experimentation. Commanders would regain a technique set whose boundaries are currently criminal.
% FOUNDING_PROBLEM: After the Second World War, existing protections covered only declared international wars between recognized states; in civil wars like Spain, detainees fell to whatever the local victor allowed. The 1949 drafters needed a minimum every party to ANY armed conflict — including unrecognized rebels — could be held to, applicable in each case without conditions on reciprocity, recognition, or emergency, so that no conflict anywhere would be legally floor-less.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the 1949 diplomatic conference records and the ICRC's published commentary attest the drafters' stated intent; independent historians of the Spanish Civil War document the pre-1949 protection gap; UN commissions of inquiry on Syria, Ethiopia, and Sudan keep invoking the missing floor in precisely the internal-conflict settings that motivated it — investigative seats with no stake in the standard's institutional budget.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).
:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. CLAIMED TYPE: tangled_rope, from structure alone — the reading possesses a genuine, externally corroborated coordination function (a reciprocal restraint floor solving the civil-war floorlessness problem, whose founding motivation is attested by 1949 conference records and independent historiography) AND asymmetric burden-bearing through the same structure (compliance costs concentrate on parties with treaty exposure and disciplined chains of command while protection flows to all detainees and enforcement reaches non-state violators only episodically) AND mandatory active enforcement (criminal liability, tribunals, inspections — the norm survives nowhere without machinery). It is not a snare: the coordination story is not cover, it is the documented reason the drafters built it. It is not a rope: coercive overhead is far from minimal and alternatives are suppressed by design. METRICS, descriptively: epsilon 0.40 — moderate, because the arrangement takes real costs from governed parties (bounded interrogation, self-inspection, prosecution exposure) while delivering protection goods and conferring nothing material on any collector; not negligible because burden concentration is real. Suppression 0.80 — high BY DESIGN: non-derogability is this reading's defining architecture; the text formally forecloses the necessity and balancing alternatives. Accessibility collapse 0.38 — LOW, and honestly so: the alternatives have not collapsed, they are the live kernel contest; that residual accessibility is precisely what distinguishes a constructed norm from a natural limit. Resistance 0.72 — sustained: reservation games, black-site programs, 'unlawful combatant' reframing, and open violation by several states. Theater ratio 0.42 — the enforcement mix is genuinely functional (ICRC visits measurably affect treatment; convictions occur) but carries a heavy proclamation-without-consequence component (condemnation resolutions over ongoing violations). Temporal series run on one shared seven-point grid (interval units: t=0 is 1949, t=75 is 2024); theater shows one visible dip (the 1990s ad hoc tribunal era, when enforcement was unusually real) inside an otherwise rising arc; extraction rose with enforcement maturation and eased slightly as enforcement decayed in recent theaters. Suppression_requirement rises monotonically — an enforcement ratchet: each era layered machinery (ad hoc tribunals, ICC, universal jurisdiction, sanctions) onto a norm that began as a paper commitment.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme and structurally driven. From the detainee seat the arrangement is pure protection — a floor arriving from outside their own agency. From the interrogation-professional seat the same text is a career-shaped prohibition that prices out their core craft regardless of circumstance. From the capturing-power seat it is an asymmetric burden: they inspect themselves, prosecute themselves, and finance compliance while many adversaries defect at low cost. From the accountability-body seat the text is jurisdiction-generating raw material. The engine computes these divergent per-seat classifications from the power/exit data; the divergence is the measurement, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. detainees_hors_de_combat: beneficiary + powerless + trapped -> d near 0.0 (fully subsidized; the arrangement protects them at no price they pay). humanitarian_monitoring_bodies and human_rights_advocacy_networks: beneficiaries with mobile exit -> d low (0.05–0.15); they collect institutional goods (mandate, access, standing) without bearing compliance costs. capturing_state_detention_authorities: victim + institutional + constrained -> d high (~0.8); their paper exit (denunciation with one-year notice) is unusable, so they sit near the target end despite the reciprocity argument. military_intelligence_interrogators: victim + organized + constrained -> d ~0.8+. international_accountability_bodies: agenda-setting with analytical exit -> near-symmetric administrative position. OVERRIDE (nonstate_armed_groups, power_atom moderate, d 0.35): the structural derivation would read their victim listing plus constrained exit as target-position (~0.65), which is wrong for this constraint — enforcement against them is episodic and post-conflict, their members collect protection when captured, and their routine compliance burden is nominal; the honest d is well below symmetric, nearer beneficiary than target. Receipt surface: the constraint's gains demonstrably accrue to humanitarian_monitoring_bodies — access rights and mandate scope are written into the architecture itself, making them the seat the arrangement's operation feeds. Fixing cost: weakening or removing the floor is prohibitive for the only actors who could (the contracting states collectively) — reciprocity collapse, escalation exposure, and reputational catastrophe outweigh any compliance savings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — civil-war floorlessness — is still LIVE: new internal conflicts erupt continually and the floor's absence is invoked exactly where it was meant to bind. Mandatrophy is therefore NOT resolved; no mandatrophy_resolved flag is declared, and the R5 mismatch consumer finds status=live x verdict=world_rearranges — coherent, no zombie flag. What this classification guards against in both directions: (1) mistaking the norm's moral prestige for natural law — a mountain claim would erase the enforcement dependence, the resistance, and the live sibling contest that the accessibility_collapse and resistance values document; (2) mistaking its selective, contested enforcement for pure cover — a snare claim would erase the externally corroborated founding function that the receipts (1949 records, independent historiography, commission reports) attest. The tangled-rope placement holds both facts. Watch item: the theater series is trending upward; if the founding problem died while proclamation activity kept growing, this would drift toward piton territory — the measurements exist to catch that turn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governing_reading_contest,
    'Which reading of the humane_treatment_standard kernel governs a given detention operation — this story''s absolute_prohibition, or one of its siblings (contextual_necessity, proportionality_balancing)?',
    'Authoritative interpretation: domestic court rulings, treaty-body pronouncements, or a diplomatic conference amending the text. Absent adjudication, the operative reading is whichever the acting power''s chain of command adopts — which is exactly the contest this story documents.',
    'Adopting contextual_necessity moves detainees near the exception threshold into the harmed set and converts this reading''s structure toward enforced exclusion for them; adopting proportionality_balancing reintroduces case-by-case variance and dissolves the absolute floor this story prices. This story''s classification is valid only under the absolute reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governing_reading_contest, conceptual, 'Kernel-level dispute: which reading of the humane-treatment kernel governs, and therefore whose constraint this is.').

omega_variable(
    reciprocity_realization_gap,
    'Does the reciprocal-protection benefit actually realize for major detaining powers, or has asymmetric conflict made the protection-for-restraint exchange structurally one-way?',
    'Paired-conflict dataset: capture volumes and treatment outcomes for both sides across recent non-international armed conflicts; compare realized reciprocal protection against compliance expenditure by each party.',
    'Realized reciprocity pulls the capturing-power seat back toward symmetric coordination-cost classification; persistent one-wayness pushes it toward the full-target end and strengthens extraction readings at the state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_realization_gap, empirical, 'Whether the protection-for-restraint exchange is symmetric in practice or one-way under asymmetric conflict.').

omega_variable(
    enforcement_selectivity_structure,
    'Is enforcement''s concentration on states with treaty exposure and functioning courts a capacity limitation, or a built-in selectivity that systematically spares non-state violators?',
    'Cross-conflict accountability audit: prosecution and sanction rates for equivalent violations committed by state versus non-state perpetrators over a fixed window.',
    'Built-in selectivity would recast the arrangement as an instrument that binds the compliant and exempts the defiant — sharpening burden at compliant seats and dead-letter status elsewhere; capacity limits would support the tangled-rope reading with an enforcement-lag explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity_structure, empirical, 'Whether enforcement asymmetry is structural design or capacity shortfall.').

omega_variable(
    natural_law_or_construct,
    'Is the absolute prohibition an emergent moral constant (jus cogens expressing universal conscience) or a constructed treaty artifact whose survival depends on enforcement machinery?',
    'Persistence test under enforcement collapse: examine adherence trajectories in theaters where monitoring withdrew entirely and accountability lapsed; survey elite and popular attitudes across cultures where no enforcement reaches.',
    'Emergent-constant findings push classification toward mountain-like treatment and undermine beneficiary-based reclassification; construct findings confirm the enforcement-dependent tangled-rope structure this story authors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_or_construct, conceptual, 'Naturality versus construction of the prohibition — the classic false-summit ambiguity, documented here even though this story does not claim mountain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t12, humane_treatment_standard__absolute_prohibition, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(huma_tr_t12, observed).
narrative_ontology:measurement(huma_tr_t25, humane_treatment_standard__absolute_prohibition, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(huma_tr_t25, observed).
narrative_ontology:measurement(huma_tr_t37, humane_treatment_standard__absolute_prohibition, theater_ratio, 37, 0.2).
narrative_ontology:measurement_basis(huma_tr_t37, observed).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__absolute_prohibition, theater_ratio, 50, 0.38).
narrative_ontology:measurement_basis(huma_tr_t50, observed).
narrative_ontology:measurement(huma_tr_t62, humane_treatment_standard__absolute_prohibition, theater_ratio, 62, 0.45).
narrative_ontology:measurement_basis(huma_tr_t62, observed).
narrative_ontology:measurement(huma_tr_t75, humane_treatment_standard__absolute_prohibition, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(huma_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t12, humane_treatment_standard__absolute_prohibition, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(huma_be_t12, observed).
narrative_ontology:measurement(huma_be_t25, humane_treatment_standard__absolute_prohibition, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(huma_be_t25, observed).
narrative_ontology:measurement(huma_be_t37, humane_treatment_standard__absolute_prohibition, base_extractiveness, 37, 0.36).
narrative_ontology:measurement_basis(huma_be_t37, observed).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__absolute_prohibition, base_extractiveness, 50, 0.44).
narrative_ontology:measurement_basis(huma_be_t50, observed).
narrative_ontology:measurement(huma_be_t62, humane_treatment_standard__absolute_prohibition, base_extractiveness, 62, 0.41).
narrative_ontology:measurement_basis(huma_be_t62, observed).
narrative_ontology:measurement(huma_be_t75, humane_treatment_standard__absolute_prohibition, base_extractiveness, 75, 0.4).
narrative_ontology:measurement_basis(huma_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t12, humane_treatment_standard__absolute_prohibition, suppression_requirement, 12, 0.14).
narrative_ontology:measurement_basis(huma_su_t12, observed).
narrative_ontology:measurement(huma_su_t25, humane_treatment_standard__absolute_prohibition, suppression_requirement, 25, 0.3).
narrative_ontology:measurement_basis(huma_su_t25, observed).
narrative_ontology:measurement(huma_su_t37, humane_treatment_standard__absolute_prohibition, suppression_requirement, 37, 0.48).
narrative_ontology:measurement_basis(huma_su_t37, observed).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__absolute_prohibition, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(huma_su_t50, observed).
narrative_ontology:measurement(huma_su_t62, humane_treatment_standard__absolute_prohibition, suppression_requirement, 62, 0.72).
narrative_ontology:measurement_basis(huma_su_t62, observed).
narrative_ontology:measurement(huma_su_t75, humane_treatment_standard__absolute_prohibition, suppression_requirement, 75, 0.8).
narrative_ontology:measurement_basis(huma_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the humane treatment standard of Common Article 3' decomposes into three structurally distinct constraints sharing one kernel text. This file authors the absolute_prohibition member (no derogation; detainees as full rights-holders; epsilon 0.40, tangled_rope). The contextual_necessity member authors the same text as an overridable baseline — under it, detainees near the exception threshold enter the victim set and the epsilon profile shifts materially upward for them. The proportionality_balancing member authors a weighting scheme with no absolutes — intermediate victim structure and case-indexed epsilon. Per the epsilon-invariance principle these are three stories, not one story with a measurement parameter: averaging across readings would fabricate a single epsilon for a label that conflates three arrangements. Upstream/downstream: the absolute reading anchors the other two (both define themselves relative to whether the floor bends), hence this file links to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__absolute_prohibition, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
