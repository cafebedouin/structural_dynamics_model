% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3 — Positive Entitlement Reading (State Duty to Provide Material Conditions for Life/Security)
 *   domain: constitutional/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint story captures the positive entitlement reading of UDHR
 *   Article 3 — the interpretation that 'everyone has the right to life,
 *   liberty and security of person' imposes on states a duty to provide the
 *   material conditions (welfare, healthcare, housing, income floor)
 *   necessary for that right to be real. This reading has expanded from
 *   aspirational declaration (1948) to justiciable constitutional entitlement
 *   in multiple jurisdictions (South Africa, Colombia, India, European social
 *   charter systems). The constraint operates as a tangled rope: it solves a
 *   genuine coordination problem (pooling risk to guarantee existence
 *   minimum) while extracting asymmetrically from property holders and
 *   expression-rights holders through redistributive taxation and hate speech
 *   regulation. The engine will compute per-seat classifications from the
 *   structural data authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.55).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3 — Positive Entitlement Reading (State Duty to Provide Material Conditions for Life/Security)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, '5d1d640a-3282-45b2-88a8-0ded22f104ca').
narrative_ontology:cs_kernel_codification('5d1d640a-3282-45b2-88a8-0ded22f104ca', fixed_text).
narrative_ontology:cs_authority_grounding('5d1d640a-3282-45b2-88a8-0ded22f104ca', lineage).
narrative_ontology:cs_interpretation_layer_present('5d1d640a-3282-45b2-88a8-0ded22f104ca').
narrative_ontology:cs_reading_relation('5d1d640a-3282-45b2-88a8-0ded22f104ca', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d1d640a-3282-45b2-88a8-0ded22f104ca', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('5d1d640a-3282-45b2-88a8-0ded22f104ca', foundational, state_has_positive_obligation_to_provide_existence_minimum).
narrative_ontology:cs_axiom_status(state_has_positive_obligation_to_provide_existence_minimum, holdable).
narrative_ontology:cs_axiom_grounding('5d1d640a-3282-45b2-88a8-0ded22f104ca', state_has_positive_obligation_to_provide_existence_minimum, deontological).
narrative_ontology:cs_axiom('5d1d640a-3282-45b2-88a8-0ded22f104ca', foundational, dignity_requires_material_substrate_not_merely_non_interference).
narrative_ontology:cs_axiom_status(dignity_requires_material_substrate_not_merely_non_interference, holdable).
narrative_ontology:cs_axiom_grounding('5d1d640a-3282-45b2-88a8-0ded22f104ca', dignity_requires_material_substrate_not_merely_non_interference, deontological).
narrative_ontology:cs_axiom('5d1d640a-3282-45b2-88a8-0ded22f104ca', secondary, social_rights_are_justiciable_not_merely_aspirational).
narrative_ontology:cs_axiom_status(social_rights_are_justiciable_not_merely_aspirational, holdable).
narrative_ontology:cs_axiom_grounding('5d1d640a-3282-45b2-88a8-0ded22f104ca', social_rights_are_justiciable_not_merely_aspirational, conventional).
narrative_ontology:cs_reference_frame('5d1d640a-3282-45b2-88a8-0ded22f104ca', udhr_1948_original_understanding).
narrative_ontology:cs_drift_state('5d1d640a-3282-45b2-88a8-0ded22f104ca', contemporary_constitutional_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5d1d640a-3282-45b2-88a8-0ded22f104ca', '').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, economically_vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, persons_with_disabilities).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, elderly_without_adequate_pensions).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, homeless_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, refugees_and_stateless_persons).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_holders_subject_to_redistributive_taxation).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_rights_holders_under_hate_speech_regimes).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpayers_in_high_brackets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, civil_society_ngos_rights_advocates).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, legislature_and_executive).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, social_rights_are_justiciable_constitutional_entitlements).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, state_has_positive_obligations_to_satisfy_existence_minimum).
narrative_ontology:constraint_vindicates(udhr_article_3__positive_entitlement_reading, dignity_requires_material_substrate_not_merely_non_interference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interprets Article 3 as imposing positive obligations; issues structural interdicts ordering legislative and executive branches to fund and deliver housing, healthcare, and social assistance. Its rulings create the enforcement architecture. The court itself does not pay for the entitlements it orders; it commands the purse of the state.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, constitutional_court, observer).

% Must appropriate funds, design programs, and administer delivery to satisfy court-ordered positive obligations. They bear the fiscal cost and political accountability for implementation failures. Their exit is constrained by constitutional supremacy and electoral consequences of non-compliance.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, legislature_and_executive, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, legislature_and_executive, payer).

% Depend on state-provided welfare, healthcare, and housing for survival. The constraint's enforcement directly determines whether they receive life-sustaining resources. They have no alternative provider and cannot exit the relationship; their survival is structurally bound to the constraint's operation.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, economically_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Require specialized healthcare, assistive technologies, and supported housing that only state provision delivers at scale. Market alternatives are inaccessible or unaffordable. The positive entitlement reading is the only structural guarantee of their inclusion.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, persons_with_disabilities, beneficiary,
    powerless, biographical, trapped, national).

% Rely on state pensions, long-term care, and subsidized housing. Some have family or private savings as partial buffer, but the constraint's guarantee is the floor. Exit is constrained by age and health; they cannot re-enter the labor market.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, elderly_without_adequate_pensions, beneficiary,
    moderate, biographical, constrained, national).

% The constraint's housing guarantee is their only legal claim to shelter. They are the most immediate beneficiaries of enforcement — and the most harmed by its non-enforcement. No exit exists; they are constitutively inside the constraint's target population.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, homeless_populations, beneficiary,
    powerless, immediate, trapped, local).

% Often excluded from contributory social insurance; the positive entitlement reading extends Article 3's protection to them as persons within the jurisdiction. Their exit options are structurally null — they cannot return to origin states and have no other jurisdiction.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, refugees_and_stateless_persons, beneficiary,
    powerless, biographical, trapped, national).

% Bear the primary fiscal incidence of funding positive entitlements through progressive taxation, wealth taxes, and property-based levies. They have significant exit options: capital mobility, jurisdictional arbitrage, political lobbying to narrow the entitlement's scope. Their structural position is payer with high exit.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, property_holders_subject_to_redistributive_taxation, payer,
    powerful, biographical, mobile, global).

% The positive entitlement reading's dignity substrate logic extends to hate speech restrictions and compelled speech doctrines that limit expression to protect vulnerable groups' security. They bear the cost of narrowed expressive space. Exit is constrained — emigration is possible but costly; domestic advocacy is the primary path.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, expression_rights_holders_under_hate_speech_regimes, payer,
    moderate, biographical, constrained, national).

% Organized through professional associations, business federations, and political parties to resist the fiscal incidence of positive entitlements. They do not bear the full cost individually but coordinate to shape the constraint's scope. Their exit is jurisdictional arbitrage and political capture.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, taxpayers_in_high_brackets, payer,
    organized, biographical, mobile, national).

% Litigate to expand the positive entitlement's scope; monitor state compliance; provide shadow reporting to treaty bodies. They benefit institutionally from the constraint's enforceability (funding, mandate, relevance) but also serve as the constraint's enforcement infrastructure. Their exit is analytical — they can shift focus to other rights frameworks.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, civil_society_ngos_rights_advocates, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__positive_entitlement_reading, civil_society_ngos_rights_advocates, observer).

% Treat the positive entitlement reading as the authoritative interpretation of Article 3 (General Comment No. 36, CESCR jurisprudence). They do not enforce directly but legitimize the reading across jurisdictions. Their seat is purely analytical — they neither pay nor collect.
narrative_ontology:constraint_stakeholder(udhr_article_3__positive_entitlement_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of providing existence-minimum goods (healthcare, housing, income floor) that no individual can secure alone and markets undersupply due to inability to pay. The state pools risk and resources across the population to guarantee a dignified life for all.
% TRANSFER_FUNCTION: Moves resources from property holders and high-income taxpayers (via progressive taxation, wealth taxes, social contributions) to vulnerable populations (via universal healthcare, social housing, minimum income, disability support, refugee assistance). The transfer is mediated by the state's administrative apparatus and constitutionally mandated by judicial enforcement.
% ABSENT_VOICES: Future generations who will bear the fiscal legacy of current entitlement levels; non-citizen residents in jurisdictions where the positive entitlement is limited to citizens; persons in states that have not incorporated the reading into domestic law — they are excluded from the conversation that defines the constraint's scope.
% DISAPPEARANCE_RATIONALE: If the positive entitlement reading vanished overnight, constitutional courts would lose their primary basis for ordering structural relief; legislatures would face no enforceable duty to fund existence-minimum goods; vulnerable populations would lose their justiciable claim to survival resources; the fiscal architecture of the welfare state would be politically contestable without constitutional anchor. The world would rearrange — not necessarily collapse, but the legal guarantee of material conditions would dissolve into legislative discretion.
% FOUNDING_PROBLEM: Post-WWII constitutional orders confronted the reality that negative liberty alone (freedom from state interference) could not secure life and dignity for populations devastated by war, displacement, and poverty. The founding problem was: how to make the state's obligation to protect life meaningful when the threats to life are structural (starvation, disease, homelessness) rather than intentional state violence?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the drafting history of the UDHR (Cassin, Roosevelt, Malik debates), the ICESCR travaux préparatoires, and the early CESCR General Comments — all outside the current beneficiary set. However, the status is contested: the original drafters framed social rights as progressive realization, not immediate justiciable entitlements; contemporary originalists argue the founding problem has been redefined by judicial activism. No single corroborating source outside the beneficiary coalition settles the dispute.
narrative_ontology:disappearance_verdict(udhr_article_3__positive_entitlement_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__positive_entitlement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__positive_entitlement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(udhr_article_3__positive_entitlement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__positive_entitlement_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint moves substantial resources from payer groups to beneficiary groups through compulsory taxation and regulatory mandates — the transfer is not voluntary and the rate is set by political-judicial process, not market exchange. Suppression (0.55) is moderate: the constraint does not primarily operate by silencing dissent, but it does structurally exclude rival interpretations (negative liberty reading) from constitutional authority and uses hate speech laws to suppress expression deemed threatening to vulnerable groups' security. Theater ratio (0.22) is low: the coordination function (guaranteeing survival) is real and the enforcement machinery (courts, bureaucracies, budgets) delivers material outcomes, not just performances. Accessibility collapse (0.45) is moderate: alternatives (charity, family, market) exist but are structurally inadequate at scale — the constraint does not fully collapse them but makes them supplementary. Resistance (0.62) is high: payer groups mobilize politically, judicially, and through capital flight to constrain the entitlement's scope.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (trapped, powerless), the constraint is a lifeline — a rope that coordinates survival. From the payer seats (powerful, mobile), the same constraint is a snare — enforced extraction with suppressed exit via constitutional supremacy. From the agenda_setter seats (institutional, analytical), the constraint is a coordination mechanism they administer but whose costs they externalize. The engine will compute these as different types per seat — the claimed type (tangled_rope) is the author's structural judgment of the constraint's overall character, not a per-seat prediction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (vulnerable populations) are structurally trapped or identity-locked — their survival depends on the constraint, exit is non-existent or extremely costly. This places them at d ≈ 0.0 (full beneficiary). Payer groups (property holders, high-income taxpayers) have mobile or arbitrage-grade exit — they can move capital, emigrate, or capture the political process — placing them at d ≈ 0.8-1.0 (full target). Expression-rights holders under hate speech regimes have constrained exit (emigration possible but costly) — d ≈ 0.6. The constitutional court and legislature are agenda_setters with analytical or constrained exit — they administer the constraint but do not bear its extraction directly. International bodies are pure observers (d = 0.5 analytical). The engine derives these from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making life protection meaningful against structural threats) remains live — poverty, homelessness, and health inequity persist. But the constraint's scope has expanded beyond the founding problem's core (existence minimum) into contested territory (hate speech as security threat, wealth redistribution as dignity requirement). This scope creep without sunset clause is a mandatrophy signal: the arrangement persists and intensifies after the original coordination problem has been partially solved in many jurisdictions. The theater ratio's rise (0.05→0.22) tracks this — a growing share of enforcement energy defends the expanded perimeter rather than the core guarantee.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_membership,
    'Does this constraint''s classification depend on its status as one reading of a contested kernel, or would it classify identically as a standalone constraint?',
    'Generate the negative_liberty_reading and procedural_hybrid_reading as separate constraint stories; compare their computed types, ε values, and beneficiary/victim structures. If the positive reading''s classification changes when the kernel frame is removed, the committer structure is classification-relevant.',
    'If kernel membership is classification-relevant, the corpus must model kernel-reading families as a first-class structure, not merely as linked files. If not, the kernel frame is analytical overlay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_membership, conceptual, 'Whether the kernel-reading relationship is structurally constitutive of the constraint''s classification.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (pooling risk for existence minimum) end and the asymmetric extraction (redistribution beyond survival, speech regulation as security) begin?',
    'Comparative study of jurisdictions that constitutionalize only the core existence minimum (healthcare, shelter, food) versus those that extend to relative poverty thresholds, cultural participation, and expressive restrictions. Measure ε and suppression in each.',
    'If the boundary is sharp, the constraint may be two constraints (a rope for core, a snare for extensions) — decomposition per ε-invariance. If the boundary is gradient, the tangled_rope classification holds as a single structure with internal variation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the positive entitlement reading is one constraint or a family of constraints with different extraction profiles.').

omega_variable(
    identity_lock_of_vulnerable_populations,
    'Is the exit_option ''trapped'' for vulnerable populations a structural fact (no alternative provider exists) or an identity-fusion mechanism (the constraint constitutes their political subjectivity)?',
    'Track post-exit trajectories: if a jurisdiction retracts the positive entitlement, do vulnerable populations experience only material deprivation, or also a collapse of political identity and claim-making capacity? The latter signals identity-lock beyond structural trapping.',
    'If identity-locked, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression internally. This would amplify χ for beneficiary seats in a way the current derivation chain does not capture (identity_locked reverts to power atom fallback when no structural data exists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_vulnerable_populations, conceptual, 'Whether vulnerable populations'' trapped exit is purely structural or includes identity-fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_art3_pos_tr_t1948, udhr_article_3__positive_entitlement_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(udhr_art3_pos_tr_t1966, udhr_article_3__positive_entitlement_reading, theater_ratio, 1966, 0.08).
narrative_ontology:measurement(udhr_art3_pos_tr_t1976, udhr_article_3__positive_entitlement_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(udhr_art3_pos_tr_t1990, udhr_article_3__positive_entitlement_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(udhr_art3_pos_tr_t2000, udhr_article_3__positive_entitlement_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(udhr_art3_pos_tr_t2010, udhr_article_3__positive_entitlement_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(udhr_art3_pos_tr_t2024, udhr_article_3__positive_entitlement_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(udhr_art3_pos_be_t1948, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(udhr_art3_pos_be_t1966, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1966, 0.25).
narrative_ontology:measurement(udhr_art3_pos_be_t1976, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1976, 0.35).
narrative_ontology:measurement(udhr_art3_pos_be_t1990, udhr_article_3__positive_entitlement_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(udhr_art3_pos_be_t2000, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(udhr_art3_pos_be_t2010, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(udhr_art3_pos_be_t2024, udhr_article_3__positive_entitlement_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_art3_pos_su_t1948, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1948, 0.1).
narrative_ontology:measurement(udhr_art3_pos_su_t1966, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1966, 0.2).
narrative_ontology:measurement(udhr_art3_pos_su_t1976, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1976, 0.3).
narrative_ontology:measurement(udhr_art3_pos_su_t1990, udhr_article_3__positive_entitlement_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(udhr_art3_pos_su_t2000, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(udhr_art3_pos_su_t2010, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(udhr_art3_pos_su_t2024, udhr_article_3__positive_entitlement_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(udhr_article_3__positive_entitlement_reading, 0.15).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, icescr_article_11_housing).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, icescr_article_12_health).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, national_constitutional_social_rights).

% DUAL FORMULATION NOTE:
% This reading and its siblings form the UDHR Article 3 constraint family. The positive entitlement reading (this story) has ε=0.68 and type tangled_rope. The negative liberty reading would have low ε (~0.15) and type mountain or rope. The procedural hybrid reading would have moderate ε (~0.35) and type scaffold. Their ε values differ because they instantiate different structural claims from the same kernel text — this is the ε-invariance principle in action. Each reading is a separate constraint with its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, institutional, 0.35).
constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
