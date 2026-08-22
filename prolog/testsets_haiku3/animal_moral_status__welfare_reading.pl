% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Constraint (Sentience + Regulated Use Reading)
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   The welfare reading of animal moral status instantiates a constraint that
 *   recognizes animals as sentient beings whose suffering should be
 *   minimized, but permits their use within regulated systems designed to
 *   prevent cruelty. This reading sits between the abolitionist claim (use is
 *   inherently wrong) and the property claim (animals have no independent
 *   moral standing). The constraint's operation legitimates animal use
 *   through welfare certification while extracting compliance costs from
 *   regulated industries and diffuse moral costs from the animals themselves.
 *   The core claim/metric gap is intentional: the reading claims tangled_rope
 *   structure (genuine coordination of safety standards + asymmetric
 *   extraction of use-legitimacy), while the authored metrics show moderate
 *   extractiveness (0.52) and high theater (0.58) — the engine measures
 *   whether the coordination story stands under scrutiny.
 *
 * KEY AGENTS:
 *   - Animal welfare organizations: set standards, collect legitimacy, coordinate enforcement
 *   - Regulated use industries: pay compliance costs, collect use-legitimacy benefit
 *   - Animals under regulated systems: trapped victims bearing suffering and death
 *   - Abolitionist advocates: structurally excluded, contest permissibility premise
 *   - Regulatory bodies: operationalize welfare/cruelty boundary
 *   - Consumers: benefit from moral permission to use without guilt
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.52).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.48).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Constraint (Sentience + Regulated Use Reading)").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, 'edb5493a-95bc-4c16-a696-a6550ae41e24').
narrative_ontology:cs_kernel_codification('edb5493a-95bc-4c16-a696-a6550ae41e24', distributed).
narrative_ontology:cs_authority_grounding('edb5493a-95bc-4c16-a696-a6550ae41e24', extraction).
narrative_ontology:cs_interpretation_layer_present('edb5493a-95bc-4c16-a696-a6550ae41e24').
narrative_ontology:cs_reading_relation('edb5493a-95bc-4c16-a696-a6550ae41e24', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('edb5493a-95bc-4c16-a696-a6550ae41e24', animal_moral_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('edb5493a-95bc-4c16-a696-a6550ae41e24', foundational, sentience_grounds_moral_consideration).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('edb5493a-95bc-4c16-a696-a6550ae41e24', sentience_grounds_moral_consideration, deontological).
narrative_ontology:cs_axiom('edb5493a-95bc-4c16-a696-a6550ae41e24', foundational, regulated_use_permissible_with_suffering_minimization).
narrative_ontology:cs_axiom_status(regulated_use_permissible_with_suffering_minimization, holdable).
narrative_ontology:cs_axiom_grounding('edb5493a-95bc-4c16-a696-a6550ae41e24', regulated_use_permissible_with_suffering_minimization, conventional).
narrative_ontology:cs_reference_frame('edb5493a-95bc-4c16-a696-a6550ae41e24', sentience_centered_use_constraint).
narrative_ontology:cs_drift_state('edb5493a-95bc-4c16-a696-a6550ae41e24', contemporary_abolitionist_pressure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('edb5493a-95bc-4c16-a696-a6550ae41e24', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, animal_welfare_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_use_industries).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_under_regulated_systems).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, small_operators_excluded_from_standards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumers_of_animal_products).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_use_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and certify 'humane use' standards; conduct inspections and enforcement; collect legitimacy and funding through the welfare frame. Their existence depends on the constraint maintaining the distinction between cruelty (wrong) and regulated use (permissible). They set agendas by writing standards, negotiating with industry, and framing public discourse.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animal_welfare_organizations, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, animal_welfare_organizations, beneficiary).

% Operate industrial animal agriculture, biomedical research, entertainment, and other extractive uses under welfare-certified practices. They benefit from the reading because it legitimates their core operation (use is permissible) while shifting the regulatory target from use-itself to method-only. They pay compliance costs (humane housing, veterinary care, reduced stocking density) but avoid the catastrophic cost of abolishing use.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_use_industries, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_use_industries, payer).

% Subject to the regulated systems justified by welfare reasoning. They experience confinement, separation from kin, procedures that cause pain (even if minimized), and early death for human benefit. The welfare constraint protects them from the worst forms of suffering but does not stop the use itself. They cannot exit, refuse consent, or seek remedies.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_under_regulated_systems, payer,
    powerless, biographical, trapped, local).

% Small farms, laboratories, and entertainment operators that cannot afford the compliance infrastructure (certified housing, veterinary oversight, documentation systems) required to demonstrate 'humane use.' The welfare constraint de facto pushes them out of the market or into illegality, while large standardized operators with compliance economies of scale consolidate control.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, small_operators_excluded_from_standards, payer,
    moderate, biographical, constrained, local).

% Argue that the welfare reading is a false resolution that legitimates use by dressing it in minimization language. They contest the foundational premise that regulated use is permissible and would argue for property status abolition. They are structurally excluded from standard-setting bodies and industry negotiations that enforce this reading.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, mobile, national).

% Legal and philosophical authorities who maintain that animals are property with no independent moral standing. They contest the sentience-based reading and argue the welfare constraint imposes unjustified costs on human use. They are partially excluded from modern regulatory discourse but retain institutional authority in property law and some jurisdictions.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, property_doctrine_proponents, excluded,
    institutional, generational, analytical, national).

% Benefit from access to meat, dairy, research-enabled medicines, and animal-derived products at market prices. The welfare reading permits them to consume with reduced moral friction: they can purchase 'humane' products and feel that cruelty is being prevented. They typically do not see the constraint as coercive because their preferred outcome (use + some protection) is precisely what it permits.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumers_of_animal_products, beneficiary,
    organized, biographical, mobile, global).

% Enforce animal welfare standards, audit compliance, and adjudicate violations. They operationalize the welfare reading's distinction between cruelty and regulated use. Their authority and budget depend on the constraint maintaining the permissibility of use while certifying methods.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the coherence and implications of the welfare reading. Some defend it as a pragmatic compromise; others argue it masks structural victimization. They observe the constraint's operation without directly collecting or paying.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, philosophers_and_ethicists, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of defining and enforcing a boundary between acceptable and unacceptable treatment of animals: without welfare standards, individual producers and consumers would make uncoordinated choices; the constraint establishes a shared definition of 'cruelty' and certifies systems that stay within it.
% TRANSFER_FUNCTION: Moves legitimacy and moral authority from animals themselves to welfare organizations and regulated industries. Use of animal bodies is transferred from presumed-wrong to permissible-if-certified. Payment flows from regulated industries to welfare organizations (for certification) and to consumers (moral relief/branded reassurance). Suffering is transferred from prevention-as-goal to minimization-within-use.
% ABSENT_VOICES: Abolitionist advocates and rights-based philosophers are structurally excluded from standard-setting and industry compliance bodies. They would contest the foundational premise (use is permissible) but are not seated in the constraint's enforcement architecture. Property doctrine proponents are partially excluded from modern discourse but retain institutional authority in law.
% DISAPPEARANCE_RATIONALE: If this welfare constraint disappeared overnight, the distinction between cruelty and regulated use would collapse. Regulatory bodies would lose enforcement authority; welfare organizations would lose legitimacy and funding; industries would face either complete abolition pressure or unregulated expansion; consumers would lose the moral framework that permits their consumption. The animal ethics landscape would reorganize around competing readings (abolitionist or property-based).
% FOUNDING_PROBLEM: Early industrial animal use involved egregious suffering with no external oversight: animals confined in unlivable conditions, subjected to painful procedures without pain relief, killed in ways designed for efficiency rather than minimization of suffering. Public revulsion and moral intuition demanded something better.
% FOUNDING_PROBLEM_CORROBORATION: Welfare organizations and regulated industries attest the problem is live and being addressed through their standards. Abolitionist scholars and animal rights advocates attest the founding problem persists despite welfare framing — suffering is reduced but not eliminated, and the constraint's core function (legitimating use) prevents solving the problem at its root. Historical animal welfare documentation supports reduction in egregious cases; empirical studies of industrial practice show persistent suffering within 'certified humane' systems. The contest is not about whether early conditions were bad (settled) but whether welfare certification solves it (contested).
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.38) when welfare standards are first implemented and public perception shifts from 'animals suffer badly' to 'animals are now protected.' As time passes and audits reveal persistent suffering within 'certified humane' systems, extractiveness drifts upward (stabilizing at 0.52) — the constraint's primary function becomes increasingly visible as legitimation of use rather than prevention of harm. Theater ratio rises steeply (0.35→0.58 by t=25, plateauing) because welfare certification activity — inspections, audits, certification labels, marketing — becomes disproportionate to the actual reduction in suffering. The suppression requirement rises from 0.32 to 0.48 as abolitionist pressure increases and the constraint must actively suppress the question 'why is use permissible at all?' The plateau at t=25–40 indicates a steady state: the constraint has matured, abolitionist challenges are continuous but contained, and no major shift occurs. Measurements reflect a single shared time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The welfare reading's primary structural asymmetry is between the regulatory/industry seats and the animal/abolitionist seats. From the agenda-setter and industry perspective, the constraint is genuine coordination: it establishes a shared standard that prevents the worst harms and enables operations to scale predictably. From the animal and abolitionist perspective, the same structure operates as coordinated legitimation of use — the welfare frame converts a moral question ('should we use animals?') into a technical question ('are we doing it humanely?'), which is the core extraction mechanism. The gap is not about metric disagreement but about baseline assumptions: one set of seats accepts that regulated use is permissible (and therefore minimization is good); the other set rejects that premise. The engine computes this as per-seat type divergence from the structural data — the constraint's coordination function is real (all seats agree on the problem of preventing egregious suffering), but the extraction of use-legitimacy flows asymmetrically to the industries and away from alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare organizations and regulated industries face low directionality (d near 0.2–0.3, beneficiary end): they shape the rules, collect legitimacy and profit, and can exit if the constraint changes by shifting to different business models. Animals under regulated systems face high directionality (d near 0.9, target end): they are identity-locked into captivity and bear the extraction directly (confinement, pain, death). Consumers sit near symmetric (d ≈ 0.5): they get moral relief and access to cheap animal products, but also bear diffuse cost of knowing suffering persists. Abolitionist advocates face high directionality (d near 0.85) as their core message is actively suppressed by the constraint's framing — their exit is to accept the welfare reading or retreat to marginal status. The per-seat computation should show the payer and agenda-setter seats diverging sharply: the operator seats (welfare orgs, industries, regulators) experience the constraint as low-extractiveness coordination, while the payer seats (animals, abolitionists) experience it as high-extractiveness suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The welfare reading avoids one classic mandatrophy trap (Piton misclassification) by maintaining its founding problem as live and contested: the question 'how much suffering is acceptable in animal use?' remains open, and the constraint's enforcement continues to address it (even if inadequately). However, a secondary mandatrophy signal appears in the theater ratio's rise: as welfare certification becomes increasingly ritualized and divorced from actual suffering reduction, the constraint approaches the Piton threshold. The commentary documents this drift explicitly — theater rising from 0.35 to 0.58 indicates that a growing share of activity is performative rather than functional. The classification lands at tangled_rope rather than piton because: (1) genuine coordination of standards still exists, (2) industries still pay real compliance costs, (3) beneficiaries (welfare orgs) still depend on the constraint for legitimacy. But the omega variable addresses whether the constraint will eventually degrade into pure theater (Piton), with welfare organizations maintaining the apparatus because it collects donations and moral authority, while actual animal suffering persists unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_certification_sufficiency,
    'Does welfare certification in practice reduce animal suffering to an acceptable level, or does it legitimize continued use while suffering persists?',
    'Long-term empirical study comparing certified vs. non-certified systems, cross-sectional pain/mortality/behavior data from animals in regulated use, and post-exit animal behavior (do rescued animals show post-traumatic patterns consistent with abuse despite ''humane'' conditions?).',
    'If certification substantially reduces suffering toward acceptable levels, the tangled_rope classification holds and the coordination function is real. If suffering persists at near-baseline levels despite certification, the constraint is reclassifiable as snare (pure extraction masked as coordination), and the ε jumps significantly (0.52 → 0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_certification_sufficiency, empirical, 'Whether welfare certification achieves actual suffering reduction or legitimates use without harm prevention.').

omega_variable(
    permissibility_premise_contestation,
    'Is the foundational premise that regulated use is permissible held by a genuine constituency, or does it derive its authority primarily from the constraint''s own enforcement and industry support?',
    'Public opinion research separating responses to ''how should we treat animals in systems we use?'' (where welfare-preference is high) from ''should we use animals at all?'' (where abolition-preference is rising in some demographics). Polling across jurisdictions with different constraint strengths.',
    'If permissibility is held as a genuine live conviction across stakeholder groups, the reading is legitimately coexisting with alternatives. If permissibility is enforced by constraint-machinery rather than widespread conviction, the suppression metric is understated and the reading risks reclassification as snare with high theater-ratio (Piton candidate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissibility_premise_contestation, empirical, 'Whether the welfare reading''s permissibility premise is genuinely held or constraint-enforced.').

omega_variable(
    kernel_reading_foreclosure_risk,
    'Does the welfare reading''s institutionalization foreclose the abolitionist reading by making rights-based arguments structurally invisible in regulatory discourse?',
    'Track whether abolitionist arguments appear in regulatory hearings, standard-setting bodies, and philosophical journals; measure citation rates and public salience before/after welfare-certification becomes dominant institutional framework.',
    'If the welfare reading forecloses abolitionist discourse within regulatory institutions (even if abolitionism survives in philosophy), the reading_relations type should shift from coexists_with to forecloses for the abolitionist sibling. This would mean the readings do not actually coexist at the institutional level — one has suppressed the other from the relevant decision-making spaces.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_risk, conceptual, 'Whether welfare institutionalization structurally forecloses abolitionist framing in practice despite theoretical coexistence.').

omega_variable(
    identity_lock_in_welfare_organizations,
    'Are welfare organizations identity-locked into defending the welfare reading, such that a shift toward abolition or property-doctrine would require organizational dissolution?',
    'Historical analysis of welfare organizations'' responses to abolitionist critiques; tracking of mission-drift and organizational mergers/dissolutions when animal ethics shifts; interviews with leadership about counterfactual scenarios.',
    'If welfare organizations are identity-locked, their role as beneficiary and agenda-setter is structurally dependent on the constraint persisting. This amplifies the theater-ratio risk: as actual welfare progress plateaus, the organizations must increase visibility of their activity to justify funding, which increases theater independent of functional need. A high identity-lock would support reclassification toward piton if theater continues rising.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_welfare_organizations, empirical, 'Whether welfare organizations depend on the welfare reading for institutional identity.').

omega_variable(
    small_operator_exclusion_mechanism,
    'Do welfare standards de facto exclude small operators because compliance is expensive, or could small operators meet standards at proportional cost if they chose?',
    'Cost-structure analysis comparing compliance cost per animal between large standardized operations and small farms; tracking of small operator exit rates post-regulation; pilot programs testing scalable welfare standards.',
    'If exclusion is a side effect of standardization rather than deliberate design, the constraint''s asymmetry favors scale but is not intentionally extractive. If small operators are priced out deliberately by standard-setting, the constraint has a secondary extraction function (consolidating use into large regulated operations) beyond the primary function (minimizing suffering).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_operator_exclusion_mechanism, empirical, 'Whether welfare standards create intentional or incidental barriers for small operators.').

omega_variable(
    kernel_reading_alternative_framings,
    'Are there coherent framings of the animal moral status kernel that would produce different ε values or type classifications than the welfare reading instantiates?',
    'Thought experiment: an alternative welfare reading that rejects permissibility but accepts regulated-system minimization (e.g., ''regulated use is a temporary evil we minimize while transitioning away'') would produce different beneficiary/victim structure and different ε. Map the logical space of readings and identify which produce classification-grade differences.',
    'If a coherent alternative welfare-framing exists that produces different ε (e.g., higher suppression due to active transition pressure, lower theater due to honesty about impermanence), the constraint-family may need decomposition. The welfare_reading as authored here assumes permissibility; a ''welfare-with-transition'' reading would be a sibling, not an alternative interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether the welfare reading''s ε-invariance is robust or depends on a specific framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(animal_welfare_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(animal_welfare_tr_t0, observed).
narrative_ontology:measurement(animal_welfare_tr_t5, animal_moral_status__welfare_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(animal_welfare_tr_t5, observed).
narrative_ontology:measurement(animal_welfare_tr_t10, animal_moral_status__welfare_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement_basis(animal_welfare_tr_t10, observed).
narrative_ontology:measurement(animal_welfare_tr_t15, animal_moral_status__welfare_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(animal_welfare_tr_t15, observed).
narrative_ontology:measurement(animal_welfare_tr_t20, animal_moral_status__welfare_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(animal_welfare_tr_t20, observed).
narrative_ontology:measurement(animal_welfare_tr_t25, animal_moral_status__welfare_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(animal_welfare_tr_t25, observed).
narrative_ontology:measurement(animal_welfare_tr_t30, animal_moral_status__welfare_reading, theater_ratio, 30, 0.59).
narrative_ontology:measurement_basis(animal_welfare_tr_t30, observed).
narrative_ontology:measurement(animal_welfare_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(animal_welfare_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(animal_welfare_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(animal_welfare_be_t0, observed).
narrative_ontology:measurement(animal_welfare_be_t5, animal_moral_status__welfare_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(animal_welfare_be_t5, observed).
narrative_ontology:measurement(animal_welfare_be_t10, animal_moral_status__welfare_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(animal_welfare_be_t10, observed).
narrative_ontology:measurement(animal_welfare_be_t15, animal_moral_status__welfare_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(animal_welfare_be_t15, observed).
narrative_ontology:measurement(animal_welfare_be_t20, animal_moral_status__welfare_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(animal_welfare_be_t20, observed).
narrative_ontology:measurement(animal_welfare_be_t25, animal_moral_status__welfare_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(animal_welfare_be_t25, observed).
narrative_ontology:measurement(animal_welfare_be_t30, animal_moral_status__welfare_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(animal_welfare_be_t30, observed).
narrative_ontology:measurement(animal_welfare_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(animal_welfare_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(animal_welfare_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(animal_welfare_su_t0, observed).
narrative_ontology:measurement(animal_welfare_su_t5, animal_moral_status__welfare_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(animal_welfare_su_t5, observed).
narrative_ontology:measurement(animal_welfare_su_t10, animal_moral_status__welfare_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(animal_welfare_su_t10, observed).
narrative_ontology:measurement(animal_welfare_su_t15, animal_moral_status__welfare_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement_basis(animal_welfare_su_t15, observed).
narrative_ontology:measurement(animal_welfare_su_t20, animal_moral_status__welfare_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(animal_welfare_su_t20, observed).
narrative_ontology:measurement(animal_welfare_su_t25, animal_moral_status__welfare_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(animal_welfare_su_t25, observed).
narrative_ontology:measurement(animal_welfare_su_t30, animal_moral_status__welfare_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(animal_welfare_su_t30, observed).
narrative_ontology:measurement(animal_welfare_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(animal_welfare_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(animal_moral_status__welfare_reading, 0.18).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_moral_status kernel decomposes into three constraint stories, one per major reading. Each reading produces a different ε-invariant constraint with different beneficiary/victim structure. The welfare_reading (this story) treats animals as sentient and use as permissible-if-regulated. The abolitionist_reading treats use itself as the violation. The property_reading denies independent moral standing. These are not observational variants of one constraint; they are structurally distinct constraints derived from competing interpretations of a single kernel. Network edges link all three; the upstream story (welfare_reading, institutionally dominant) influences both siblings by establishing regulatory baselines; the downstream stories (abolitionist and property) contest those baselines from outside the institutional structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_moral_status__welfare_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
