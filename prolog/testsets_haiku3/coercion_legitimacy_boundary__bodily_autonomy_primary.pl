% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primacy Reading: Medical Intervention Without Consent is Categorically Impermissible
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the bodily_autonomy_primary reading of
 *   the contested kernel 'coercion_legitimacy_boundary.' The kernel asks:
 *   Under what circumstances may the state compel medical intervention? The
 *   bodily_autonomy_primary reading answers: Never, regardless of collective
 *   benefit. This is one reading among three live alternatives
 *   (public_health_primary, proportionality_reading), each held by different
 *   institutional and professional communities. The reading claims to ground
 *   itself in natural law (bodily integrity as irreducible), yet it exists in
 *   active dispute with siblings that reject or qualify that claim. The story
 *   is authored as a constraint story for THIS reading only: what
 *   extractiveness, suppression, and beneficiary/victim structure
 *   characterizes the bodily_autonomy_primary reading when it is
 *   operationalized as the legitimacy framework? The measurement series
 *   tracks the rising cost of non-enforcement (extractiveness 0.15 to 0.42
 *   across 1990–2026, particularly sharp 2019–2022 COVID period) and the
 *   increasing suppressive pressure needed to maintain the reading against
 *   public_health_primary alternatives (suppression 0.15 to 0.28 over the
 *   same span). Theater is low (0.12 at endpoint), indicating the
 *   constraint's operation is mostly functional (maintaining the boundary
 *   against coercion) rather than performative.
 *
 * KEY AGENTS:
 *   - individual_consent_bearers: Those protected by bodily autonomy framework (powerless to organized institutional seats dependent on the reading remaining in force)
 *   - bodily_autonomy_advocates: Organized constituencies (bioethicists, civil libertarians, human rights bodies) that instantiate and defend the reading (organized/powerful)
 *   - mandate_enforcers: Public health authorities, governments, employers, schools forced to operate under non-enforcement cost (institutional, trapped)
 *   - immunocompromised_unvaccinated_adjacent: Vulnerable populations exposed to disease spread by non-enforcement (powerless, trapped, identity-locked via biological vulnerability)
 *   - unvaccinated_refusers: Beneficiaries of the reading's protection (protected from coercion; their compliance remains voluntary)
 *   - public_health_primary_reading_proponents: Competing reading institutionalized in some jurisdictions (excluded from bodily_autonomy_primary framing)
 *   - proportionality_reading_proponents: Middle-ground reading balancing autonomy and harm-prevention (excluded, coexisting)
 *   - constitutional_adjudicators: Neutral observer seats interpreting the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.28).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy Primacy Reading: Medical Intervention Without Consent is Categorically Impermissible").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e502039d-bfd7-49f3-b960-f7fee95c4a9d').
narrative_ontology:cs_kernel_codification('e502039d-bfd7-49f3-b960-f7fee95c4a9d', formalized).
narrative_ontology:cs_authority_grounding('e502039d-bfd7-49f3-b960-f7fee95c4a9d', lineage).
narrative_ontology:cs_interpretation_layer_present('e502039d-bfd7-49f3-b960-f7fee95c4a9d').
narrative_ontology:cs_reading_relation('e502039d-bfd7-49f3-b960-f7fee95c4a9d', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('e502039d-bfd7-49f3-b960-f7fee95c4a9d', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('e502039d-bfd7-49f3-b960-f7fee95c4a9d', foundational, bodily_integrity_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('e502039d-bfd7-49f3-b960-f7fee95c4a9d', bodily_integrity_inviolable, deontological).
narrative_ontology:cs_axiom('e502039d-bfd7-49f3-b960-f7fee95c4a9d', foundational, coercion_never_legitimate_medical_context).
narrative_ontology:cs_axiom_status(coercion_never_legitimate_medical_context, holdable).
narrative_ontology:cs_axiom_grounding('e502039d-bfd7-49f3-b960-f7fee95c4a9d', coercion_never_legitimate_medical_context, deontological).
narrative_ontology:cs_reference_frame('e502039d-bfd7-49f3-b960-f7fee95c4a9d', bodily_autonomy_doctrine_post_1945).
narrative_ontology:cs_drift_state('e502039d-bfd7-49f3-b960-f7fee95c4a9d', covid_19_pandemic_2019_2022, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e502039d-bfd7-49f3-b960-f7fee95c4a9d', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_consent_bearers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_unvaccinated_adjacent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, unvaccinated_refusers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons whose bodily autonomy is protected by the reading: the constraint secures their right to refuse medical intervention regardless of public health pressure. The protection is framed as a fundamental, inalienable right. They do not run this constraint; it is a framework they inhabit and rely on.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_consent_bearers, beneficiary,
    moderate, biographical, mobile, national).

% Professional and activist constituencies — bioethicists, civil libertarians, patient rights organizations, human rights bodies — that have institutionalized the reading and defend it as foundational. They collect no rents; they vindicate a proposition.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocates, beneficiary,
    organized, generational, arbitrage, global).

% State public health authorities, medical institutions, employers, and schools that under the public_health_primary or proportionality_reading would be empowered to mandate medical interventions. Under bodily_autonomy_primary, they are structurally forbidden from coercing consent. They bear the cost of non-enforcement: disease spread, uncontrolled outbreak response, loss of policy leverage.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, payer,
    institutional, generational, constrained, national).

% Individuals who cannot be vaccinated or are severely immunocompromised, whose vulnerability depends on population immunity. Under bodily_autonomy_primary without mandate enforcement, they bear the exposure cost when others refuse medical intervention. They cannot exit the adjacent population; their protection depends on others' voluntary compliance, which the reading cannot compel.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_unvaccinated_adjacent, payer,
    powerless, biographical, trapped, local).

% Individuals who refuse medical intervention (vaccination, treatment) on grounds of bodily autonomy, religious belief, medical skepticism, or autonomy principle. The reading protects their right to refuse. They do not set the constraint; they are its protected actors.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, unvaccinated_refusers, beneficiary,
    moderate, biographical, constrained, local).

% Jurisdictions and authorities that adopt the public_health_primary reading and operate under mandated medical intervention. They would argue that collective harm-prevention justifies coercion in specific contexts. Their reading is logically foreclosed by bodily_autonomy_primary's categorical rejection of coercion regardless of benefit.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_primary_reading_proponents, excluded,
    institutional, generational, trapped, national).

% Jurisdictions and authorities adopting proportionality_reading: they would scale coercion legitimacy with disease severity and transmission dynamics, permitting mandates for measles but not flu. This middle ground coexists with bodily_autonomy_primary in different jurisdictions but is rejected as a framework choice by autonomy-primary advocates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_reading_proponents, excluded,
    institutional, generational, constrained, national).

% Courts and constitutional bodies that interpret the legitimacy boundary between individual autonomy and state coercion. They do not set the constraint but receive testimony and render decisions that affect its enforceability.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, constitutional_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This is a boundary-setting reading, not a coordination mechanism. It establishes a normative rule about the legitimacy of coercion, not a solution to a collective-action problem. The 'coordination' is purely at the level of commitment-system interpretation: establishing what autonomy means in relation to state power.
% TRANSFER_FUNCTION: No direct transfer. The reading prevents a transfer that would otherwise occur under public_health_primary or proportionality_reading: medical intervention (compliance) moved from refusers to the state via coercion. Under bodily_autonomy_primary, that transfer is categorically forbidden; immunocompromised persons instead depend on voluntary (non-coerced) compliance, which the reading cannot compel.
% ABSENT_VOICES: Individuals harmed by disease spread from unvaccinated populations (immunocompromised, neonates, those with medical contraindications) are harmed by the reading's structure but are not parties to the consent-autonomy debate — they enter the victim set as exposed to unvaccinated rather than as voice in the legitimacy contest. Public health authorities that depend on mandate enforcement are excluded from the autonomy-primary framing, though they appear as payersunder the measurement of non-enforcement cost.
% DISAPPEARANCE_RATIONALE: Under bodily_autonomy_primary framing, disappearance of the constraint would be a catastrophe — the reading's core claim is that medical coercion is never legitimate. Under public_health_primary or proportionality readings, disappearance would enable uncontrolled disease spread and loss of policy tools. The contest is about whether the constraint should exist at all, not whether arrangements depend on it.
% FOUNDING_PROBLEM: The founding problem this reading addresses is the legitimacy of medical coercion and state power over the body: the question 'Under what circumstances, if any, may the state compel medical intervention?' This reading's answer is 'Never, regardless of collective benefit.' It is grounded in Enlightenment autonomy theory, bodily integrity doctrine, and post-WWII medical ethics (Nuremberg Code, Declaration of Helsinki).
% FOUNDING_PROBLEM_CORROBORATION: The International Covenant on Civil and Political Rights, numerous constitutional courts (Canada, South Africa, India in specific rulings), and bioethics consensus documents (UNESCO Declaration on Bioethics, major medical association ethics guidelines) affirm bodily autonomy as a foundational principle. However, the same bodies permit narrow exceptions (emergency, imminent danger, least-restrictive-means) that the public_health_primary and proportionality readings exploit. Competing readings are actively endorsed by public health authorities and courts in jurisdictions that permit vaccine mandates. No single source 'corroborates' the founding problem outside the autonomy-advocate tradition; the problem is essentially contested.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.42 at interval end, rising from 0.15 in 1990): The core cost to public health authorities is the inability to compel medical intervention during disease outbreaks. This is highest during high-transmission-disease crises (COVID-19 in 2019–2022 saw extractiveness rise sharply to 0.42 as vaccination rates fell below herd-immunity thresholds and enforcement remained prohibited). The reading extracts compliance capacity and policy leverage from mandate enforcers while protecting individual refusers. SUPPRESSION (0.28 at endpoint, rising from 0.15): The mechanism that holds the reading in place is institutional and legal (constitutional protection of medical autonomy, international human rights instruments, court rulings), not coercive force applied against individuals. The suppression is primarily structural: the reading is operationalized through law, institutional policy, and professional norms that foreclose mandate-enforcement as a policy option, regardless of public health pressure. The suppression is not directed at individuals; it is directed at institutions trying to override autonomy. THEATER (0.12, flat): The reading's operation is mostly functional — it genuinely prevents coercion as claimed. The small theater component arises from the reading's institutional maintenance activities: bioethics committees affirming autonomy, professional bodies reaffirming consent doctrine, advocacy organizations defending the reading in policy disputes. These are real functions (not pure theater), but they are increasingly maintenance-heavy as siblings offer competing interpretations. ACCESSIBILITY_COLLAPSE (0.65): Alternatives to the bodily_autonomy_primary reading exist and are live in actual policy (proportionality_reading permits mandates in measles contexts; public_health_primary permits harm-prevention mandates). For individual refusers under bodily_autonomy_primary, alternatives collapse substantially — they cannot legally be coerced regardless of their knowledge or circumstances. For mandate enforcers, alternatives do exist in some jurisdictions (public_health_primary adopted in some contexts), so the collapse is moderate. RESISTANCE (0.78): Strong resistance from public health authorities, epidemiologists, and mandate-supporting constituencies during the COVID pandemic showed the reading faces substantial institutional and expert opposition. Refusers face low resistance; advocates and constitutional defenders face organized opposition from proportionality and public_health proponents.
 *
 * PERSPECTIVAL GAP:
 *   The bodily_autonomy_primary reading should compute VERY DIFFERENTLY across seats. From the individual_consent_bearer's and bodily_autonomy_advocates' seats, this is a protective mountain: it secures a fundamental boundary against coercion and computes as a natural law (the engine would classify it as mountain-type, low extraction, high accessibility collapse because alternatives are foreclosed by the reading's logic). From the mandate_enforcer's seat (institutional, trapped), the same reading computes as extractive and suppressive: it prevents policy leverage and confiscates their authority to respond to collective harm. From the immunocompromised_unvaccinated_adjacent seat (powerless, trapped), it computes as extractive in a different direction — it exposes them to harm they cannot escape. The engine derives these per-seat classifications from the power atoms, exit options, and directionality declarations. The claim (mountain) and the metrics (extractiveness=0.42, suppression=0.28) intentionally diverge because different seats experience different types.
 *
 * DIRECTIONALITY LOGIC:
 *   BENEFICIARIES: individual_consent_bearers and bodily_autonomy_advocates collect the benefit of non-coercion and autonomy protection. These are genuine beneficiaries in the structural sense: the reading's existence benefits them. bodily_autonomy_advocates are not an agent that collects rents (they are vindicated by the reading's operation, not enriched by it), but they are listed as beneficiaries to flag the reading's vindicating function. VICTIMS: mandate_enforcers are payersunder non-enforcement cost; immunocompromised_unvaccinated_adjacent are victimized by disease exposure under non-enforcement. These are distinct victim mechanisms. DIRECTIONALITY: mandate_enforcers have d near 1.0 (full target of the constraint's limitation on their authority); individual_consent_bearers have d near 0.0 (full beneficiaries of protection); immunocompromised_unvaccinated_adjacent have d~0.7 (victims by exposure, but not primary targets of the reading's intent). The reading's structural purpose is to protect autonomy, not to harm immunocompromised populations — they are collateral damage of the reading's operation, not the constraint's target. This distinction would be captured by directionality_overrides if needed, but the structural derivation (powerless + trapped + identity_locked via biology) already pushes them toward the target end appropriately.
 *
 * MANDATROPHY ANALYSIS:
 *   The bodily_autonomy_primary reading is NOT mandatrophic in the classical sense (a founding problem that has been solved and the constraint persists by inertia). The founding problem — the legitimacy boundary between individual autonomy and state coercion — remains actively contested and arguably MORE salient in 2026 than in 1990 (COVID-19, monkeypox preparedness, gain-of-function research debates). However, a secondary mandatrophy appears if the founding problem STATUS is dead in a specific reading's terms: if public_health_primary reading operates in a jurisdiction and demonstrates successful disease control via enforcement (the founding problem of 'state coercion necessity' is demonstrated true), but bodily_autonomy_primary reading persists in institutional doctrine (courts, bioethics boards, international law) without re-evaluating the constraint. This would be a doctrine-level mandatrophy: the reading persists because institutional actors are identity-locked to autonomy doctrine, not because the reading's founding problem remains live. The measurement series show extractiveness and suppression rose sharply 2019–2022 (COVID crisis) and then plateaued at 0.42/0.28, suggesting the reading has reached a stable operating point under regime competition: it neither dissolved (as a pure mandate-enforcement regime would expect) nor accumulated indefinitely. The plateau suggests equilibrium under coexistence with public_health_primary and proportionality readings in different jurisdictions — classical mandatrophy (inert persistence) is not the story; contested equilibrium is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is bodily autonomy a natural law (fundamental, pre-political, irreducible) or a constructed normative reading of bodily integrity (historically specific, institutionally maintained, contingent on legal recognition)?',
    'Comparative constitutional and legal history across jurisdictions with and without explicit autonomy protections; analysis of whether the principle persists absent legal instantiation or requires institutional maintenance.',
    'If natural law: the constraint''s emergences_naturally=true is secure and extraction claims are mischaracterized. If constructed: the constraint is a doctrine maintained by advocacy coalitions, the beneficiary declarations are accurate, and FSM (false summit detection) flags the reading as non-mountain despite claimed naturalness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether bodily autonomy is a discovered principle or an institutionalized reading.').

omega_variable(
    reading_versus_natural_law_boundary,
    'Is this constraint a reading of the kernel ''coercion_legitimacy_boundary'' (a contested interpretation), or is it claiming to express a pure natural law of bodily integrity independent of reading-contests?',
    'Examination of the claim narrative: does the constraint frame itself as ''what bodily autonomy fundamentally requires'' (natural law, no kernel reading involved) or as ''one interpretation of the legitimacy boundary when coercion arises'' (kernel reading, sibling alternatives)? The KERNEL CONTEXT declaration answers this: this is a kernel reading, not a standalone mountain.',
    'This omega documents the potential FSM candidate: the story claims mountain (emerges_naturally=true) while instantiating a kernel reading with active siblings (public_health_primary, proportionality_reading). If the reading is a reading, it is not a mountain; if it is a mountain, it has no siblings. The contradiction itself is the measure: bodily autonomy may be a natural law, but the dispute over coercion legitimacy at different disease/benefit thresholds is fundamentally a reading contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_natural_law_boundary, conceptual, 'Is this a natural-law mountain or a contested kernel reading?').

omega_variable(
    immunity_cascade_gap,
    'What is the actual epidemiological impact of non-enforcement on immunocompromised populations in different disease contexts (measles transmission rate ~90% vs. flu ~25%)?',
    'Epidemiological modeling and observational data from jurisdictions with and without vaccine mandates, stratified by disease and immunocompromise status.',
    'High immunity-cascade impact (measles) would establish immunocompromised populations as substantially victimized by non-enforcement; low impact (flu) would reduce the victim set''s empirical claim. This affects whether the victim declaration stands or requires contextual qualification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunity_cascade_gap, empirical, 'Magnitude of harm to unvaccinated-adjacent immunocompromised populations under different diseases.').

omega_variable(
    suppression_of_debate_mechanism,
    'Does the bodily_autonomy_primary reading suppress alternative public health framings (proportionality, harm-benefit), or do sibling readings coexist as genuine policy alternatives?',
    'Analysis of institutional venues where alternative readings are permitted, funded, and staffed as live options vs. venues where bodily_autonomy_primary is treated as settling doctrine and alternatives are marginalized.',
    'High suppression of proportionality and public_health readings would indicate bodily_autonomy_primary functions as doctrinal enforcement (snare-like mechanism) despite mountain claims. Low suppression would indicate genuine coexistence. This affects whether the reading should be reclassified as a constraint that constrains other readings, not a natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_debate_mechanism, conceptual, 'Whether bodily_autonomy_primary suppresses or coexists with sibling readings in institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement_basis(coer_tr_t1990, observed).
narrative_ontology:measurement(coer_tr_t2000, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2000, 0.09).
narrative_ontology:measurement_basis(coer_tr_t2000, observed).
narrative_ontology:measurement(coer_tr_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2010, 0.1).
narrative_ontology:measurement_basis(coer_tr_t2010, observed).
narrative_ontology:measurement(coer_tr_t2019, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2019, 0.11).
narrative_ontology:measurement_basis(coer_tr_t2019, observed).
narrative_ontology:measurement(coer_tr_t2022, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2022, 0.12).
narrative_ontology:measurement_basis(coer_tr_t2022, observed).
narrative_ontology:measurement(coer_tr_t2026, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(coer_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(coer_be_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement_basis(coer_be_t1990, observed).
narrative_ontology:measurement(coer_be_t2000, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement_basis(coer_be_t2000, observed).
narrative_ontology:measurement(coer_be_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement_basis(coer_be_t2010, observed).
narrative_ontology:measurement(coer_be_t2019, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2019, 0.38).
narrative_ontology:measurement_basis(coer_be_t2019, observed).
narrative_ontology:measurement(coer_be_t2022, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement_basis(coer_be_t2022, observed).
narrative_ontology:measurement(coer_be_t2026, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2026, 0.42).
narrative_ontology:measurement_basis(coer_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement_basis(coer_su_t1990, observed).
narrative_ontology:measurement(coer_su_t2000, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement_basis(coer_su_t2000, observed).
narrative_ontology:measurement(coer_su_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2010, 0.22).
narrative_ontology:measurement_basis(coer_su_t2010, observed).
narrative_ontology:measurement(coer_su_t2019, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2019, 0.26).
narrative_ontology:measurement_basis(coer_su_t2019, observed).
narrative_ontology:measurement(coer_su_t2022, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2022, 0.28).
narrative_ontology:measurement_basis(coer_su_t2022, observed).
narrative_ontology:measurement(coer_su_t2026, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2026, 0.28).
narrative_ontology:measurement_basis(coer_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% The kernel 'coercion_legitimacy_boundary' decomposes into three constraint stories, each representing one live reading. The bodily_autonomy_primary reading (this story) forecloses public_health_primary (both cannot be true in a single framework) and influences proportionality_reading (by raising the legitimacy bar). Public_health_primary and proportionality_reading coexist with each other across different institutional venues. All three stories link via network.affects_constraints to document the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coercion_legitimacy_boundary__bodily_autonomy_primary, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
