% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty Model of Border Control Legitimacy
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'jurisdictional_sovereignty'
 *   reading of the contested kernel 'border_control_legitimacy'. The reading
 *   holds that sovereignty is jurisdictional authority (power to regulate
 *   rights and obligations within territory) but does not entail border
 *   closure authority; legitimacy requires balancing protection obligations
 *   (human rights, non-refoulement) with labor needs and public consent. The
 *   constraint is the operating legitimacy framework for border control in
 *   liberal democratic states since the 1990s — post-Cold War, post-Schengen,
 *   post-human-rights-regime maturation. It claims to be a coordination
 *   mechanism (jurisdictional regulation replacing arbitrary power) but
 *   operates with dual extraction: excluded migrants bear exclusion costs,
 *   displaced citizens bear labor market costs. The engine will compute
 *   per-seat classifications from the structural data; the claimed_type
 *   (tangled_rope) and metrics are authored independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.52).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty Model of Border Control Legitimacy").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '82b9cd51-65a9-4152-8f04-2ddce3879c5f').
narrative_ontology:cs_kernel_codification('82b9cd51-65a9-4152-8f04-2ddce3879c5f', distributed).
narrative_ontology:cs_authority_grounding('82b9cd51-65a9-4152-8f04-2ddce3879c5f', distributed).
narrative_ontology:cs_reading_relation('82b9cd51-65a9-4152-8f04-2ddce3879c5f', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('82b9cd51-65a9-4152-8f04-2ddce3879c5f', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('82b9cd51-65a9-4152-8f04-2ddce3879c5f', foundational, jurisdictional_authority_not_closure).
narrative_ontology:cs_axiom_status(jurisdictional_authority_not_closure, holdable).
narrative_ontology:cs_axiom_grounding('82b9cd51-65a9-4152-8f04-2ddce3879c5f', jurisdictional_authority_not_closure, deontological).
narrative_ontology:cs_axiom('82b9cd51-65a9-4152-8f04-2ddce3879c5f', foundational, legitimacy_requires_balancing).
narrative_ontology:cs_axiom_status(legitimacy_requires_balancing, holdable).
narrative_ontology:cs_axiom_grounding('82b9cd51-65a9-4152-8f04-2ddce3879c5f', legitimacy_requires_balancing, conventional).
narrative_ontology:cs_reference_frame('82b9cd51-65a9-4152-8f04-2ddce3879c5f', jurisdictional_sovereignty_framework).
narrative_ontology:cs_drift_state('82b9cd51-65a9-4152-8f04-2ddce3879c5f', contemporary_migration_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('82b9cd51-65a9-4152-8f04-2ddce3879c5f', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_jurisdictional_authority).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, employers_labor_markets).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, proportionality_principle).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, necessity_test).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, human_rights_framework).
narrative_ontology:constraint_vindicates(border_control_legitimacy__jurisdictional_sovereignty, public_consent_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims authority to regulate rights and obligations within territory, including admission and exclusion decisions. Justifies enforcement through jurisdictional sovereignty rather than absolute border closure. Bears legitimacy costs when enforcement violates proportionality or when admission undermines public consent. Can shift policy frameworks but cannot exit the legitimacy requirement itself.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_jurisdictional_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% Bear the full costs of exclusion: denied access to territory, labor markets, and protection. Subject to enforcement apparatus (detention, removal, deterrence) constrained only by proportionality and necessity tests that are unevenly applied. No meaningful exit from the constraint — cannot access the jurisdiction that excludes them, and return to origin may be dangerous or impossible.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Experience labor market displacement, wage pressure, and public service strain attributed to migration flows. Benefit from jurisdictional protections and public consent mechanisms that the constraint's legitimacy balancing purports to protect. Exit options limited to internal migration or political mobilization; cannot exit the territorial jurisdiction that defines their displacement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, beneficiary).

% Gain access to flexible, often lower-cost labor through regulated admission channels. Benefit from the constraint's coordination function: predictable regulatory framework for labor recruitment rather than chaotic or closed borders. Can relocate capital and operations across jurisdictions — exit is arbitrage-grade for capital, constrained for specific labor needs.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, employers_labor_markets, beneficiary,
    powerful, biographical, mobile, global).

% Monitor and adjudicate proportionality and necessity of enforcement actions. Issue rulings, recommendations, and pressure but lack direct enforcement power over sovereign jurisdictions. Their authority is epistemic and normative — they observe whether the constraint's own legitimacy conditions (human rights compliance) are met.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, human_rights_institutions, observer,
    institutional, generational, analytical, universal).

% Advocate for expanded admission, rights for excluded migrants, and stricter proportionality enforcement. Structurally excluded from border policy decision-making — their arguments are heard in courts and public discourse but not in the agenda-setting rooms where admission quotas and enforcement priorities are set. Exit from advocacy role is possible but costly (professional identity, organizational survival).
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, migration_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates rights and obligations within territory through jurisdictional authority — determining who may enter, remain, work, and access protections — providing a predictable legal framework that replaces arbitrary exclusion or chaotic openness.
% TRANSFER_FUNCTION: Moves exclusion costs (denied entry, detention, removal) to excluded migrants; moves displacement costs (wage pressure, service strain) to displaced citizens; moves labor access benefits to employers; moves legitimacy capital to state authority when balancing succeeds, extracts legitimacy when it fails.
% ABSENT_VOICES: Would-be migrants not yet at the border (future applicants, displaced persons in transit); future generations affected by demographic and fiscal trajectories of admission policies; stateless persons with no origin state to return to — all are structurally absent from the legitimacy calculus.
% DISAPPEARANCE_RATIONALE: If the jurisdictional sovereignty model vanished overnight, the legitimacy framework balancing protection, labor needs, and public consent would collapse. Either absolute border closure (sovereignty_primary reading) or open borders (freedom_of_movement_primary reading) would fill the vacuum, rearranging migration flows, labor markets, rights regimes, and state legitimacy claims globally.
% FOUNDING_PROBLEM: How to legitimate territorial authority over movement after the collapse of absolute sovereignty claims — reconciling the state's jurisdictional power to regulate with the moral claims of non-citizens and the economic demands of labor markets, without resorting to either absolute exclusion or uncontrolled admission.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars (e.g., Vienna Convention on the Law of Treaties drafting history, UNHCR executive committee conclusions) attest the founding problem is live — jurisdiction and human rights remain in tension. Labor economists (ILO, OECD migration studies) corroborate labor needs as structural, not cyclical. No corroboration from outside state beneficiaries for the claim that current balancing achieves legitimacy.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects dual extraction: migrants pay with denied access and enforcement trauma; citizens pay with wage/service pressure. The coordination function (jurisdictional regulation) is genuine but partial — the constraint does not fully coordinate labor needs (hence displaced citizens) nor protection obligations (hence excluded migrants). Suppression (0.52) is moderate because enforcement is constrained by proportionality/necessity tests — but those tests are unevenly applied and have eroded over time (rising suppression_requirement series). Theater ratio (0.32) is low-moderate: the proportionality framework is real but increasingly performative as enforcement intensity grows. Accessibility collapse (0.45) is moderate: alternatives (regional free movement, humanitarian visas, labor agreements) exist but are narrow. Resistance (0.55) is significant: courts, advocates, and political movements contest from both directions.
 *
 * PERSPECTIVAL GAP:
 *   From the state authority seat, the constraint appears as genuine coordination — it replaced arbitrary exclusion with rule-based jurisdiction. From the excluded migrant seat, it is extraction enforced by detention and deterrence. From the displaced citizen seat, it is a failed coordination that admits labor competition without adequate compensation. From the employer seat, it is a functional but imperfect labor regulation. The engine computes these divergences from power/exit/beneficiary/victim declarations; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority is agenda_setter with arbitrage exit — it sets rules but bears legitimacy costs (d ~0.35, not pure beneficiary). Excluded migrants are powerless/trapped — full targets (d ~0.95). Displaced citizens are moderate/constrained — significant targets but with some political voice (d ~0.70). Employers are powerful/mobile — beneficiaries with exit (d ~0.15). Human rights institutions are analytical observers (d ~0.05). Migration advocates are organized/constrained excluded — they bear advocacy costs without agenda power (d ~0.60). The dual victim structure creates a legitimacy trap: the constraint extracts from both migrants and citizens while claiming to balance their interests.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating territorial authority post-absolute-sovereignty) remains contested — not dead, not fully live. The arrangement persists because no alternative has achieved consensus, not because it solves the problem. Mandatrophy is unresolved: the constraint's mandate (balancing protection, labor, consent) has outlived its Cold War/post-Cold War consensus but no replacement mandate has been legitimated. The rising extractiveness and suppression series suggest the constraint is drifting toward snare as proportionality constraints erode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct structural arrangement or merely a rhetorical position within the border_control_legitimacy kernel?',
    'Compare enforcement patterns, admission outcomes, and legitimacy crises across states claiming this reading vs. states claiming sovereignty_primary or freedom_of_movement_primary. If material outcomes differ systematically, the reading instantiates a distinct constraint.',
    'If distinct, it warrants its own ε and classification. If rhetorical, it should merge with the dominant operational constraint (likely sovereignty_primary in practice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the jurisdictional_sovereignty reading has independent structural force or is a cover for sovereignty_primary operation.').

omega_variable(
    sibling_reading_delta_sovereignty_primary,
    'How does this reading''s structural relationship to sovereignty_primary differ in practice — does proportionality constrain enforcement, or is it absorbed?',
    'Track court rulings on proportionality in expulsion/detention cases across jurisdictions. Measure divergence between stated proportionality standard and actual enforcement outcomes.',
    'If proportionality is systematically absorbed, this reading collapses into sovereignty_primary operationally. If it constrains, the dual victim set and legitimacy balancing are structurally real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_delta_sovereignty_primary, empirical, 'Whether the proportionality/necessity tests in this reading materially constrain enforcement or are theatrical.').

omega_variable(
    sibling_reading_delta_freedom_of_movement,
    'Does this reading''s acknowledgment of protection obligations create a structural floor for migrant rights that freedom_of_movement_primary would extend but not create?',
    'Compare non-refoulement compliance, asylum grant rates, and family reunification outcomes in jurisdictions operating under this reading vs. those rejecting any movement right.',
    'If a structural floor exists, this reading is a genuine coordination constraint with extraction, not a snare. If no floor, the protection obligations are rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_freedom_of_movement, empirical, 'Whether the protection obligation half of the balancing creates enforceable rights or aspirational rhetoric.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (detention, removal, deterrence infrastructure) or partially internalized (migrants self-excluding, citizens accepting displacement as inevitable)?',
    'Post-policy-change suppression trajectory: if suppression persists after enforcement intensity decreases (e.g., after regularization programs), reclassify as partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — targets carry the constraint with them. This would increase χ for payer seats beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in dual-victim border control constraint.').

omega_variable(
    proportionality_test_effectiveness,
    'Do proportionality and necessity tests function as genuine enforcement constraints or as legitimation theater for the jurisdictional_sovereignty reading?',
    'Longitudinal analysis of court deferral rates, executive compliance with adverse rulings, and policy changes following proportionality findings.',
    'If theater, theater_ratio is understated and the constraint drifts toward snare. If genuine, the coordination function is structurally real and tangled_rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_test_effectiveness, conceptual, 'Whether the reading''s own legitimacy conditions constrain its enforcement apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_js_tr_t1990, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(bcl_js_tr_t2000, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(bcl_js_tr_t2010, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(bcl_js_tr_t2015, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(bcl_js_tr_t2020, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2020, 0.31).
narrative_ontology:measurement(bcl_js_tr_t2024, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(bcl_js_be_t1990, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(bcl_js_be_t2000, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(bcl_js_be_t2010, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(bcl_js_be_t2015, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(bcl_js_be_t2020, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(bcl_js_be_t2024, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bcl_js_su_t1990, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(bcl_js_su_t2000, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(bcl_js_su_t2010, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(bcl_js_su_t2015, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(bcl_js_su_t2020, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement(bcl_js_su_t2024, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__jurisdictional_sovereignty, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy__freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one member of the border_control_legitimacy kernel family. The three readings (jurisdictional_sovereignty, sovereignty_primary, freedom_of_movement_primary) decompose the natural-language concept 'border control legitimacy' into structurally distinct constraints with different ε values, victim sets, and coordination functions. This reading has moderate ε (0.58) with dual victims; sovereignty_primary has lower ε for citizens but higher for migrants (absolute exclusion); freedom_of_movement_primary has near-zero ε for migrants but higher for citizens (uncontrolled admission costs). They are linked via affects_constraints because each reading's legitimacy claims cite the others as counterpositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
