% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Commerce Clause - Expansive Federal Authority Reading
 *   domain: constitutional/federalism/economic
 *
 * SUMMARY:
 *   The expansive federal reading of the Commerce Clause treats 'interstate
 *   commerce' to include all economic activity with a substantial aggregate
 *   effect on national markets. This reading, entrenched since the New Deal
 *   shift of 1937 and crystallized in Wickard v. Filburn (1942) and Gonzales
 *   v. Raich (2005), grants the federal government regulatory authority over
 *   vast domains—labor, environment, food safety, telecommunications,
 *   financial regulation. The reading coordinates national economic policy
 *   but extracts state autonomy and forecloses local regulatory
 *   experimentation. This is one of three coexisting readings of the same
 *   constitutional text; the constraint story models this specific reading's
 *   internal structure, beneficiaries, and victims without reconciling it to
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - Federal regulatory agencies (agenda-setters): EPA, OSHA, FDA, SEC, FCC, NLRB, enforcing federal floors nationwide
 *   - State legislatures (payers): subordinated in regulatory authority, constrained in local variation
 *   - National coordination advocates (beneficiaries): environmental groups, consumer protection advocates, labor unions benefiting from unified standards
 *   - Local variant regimes (victims): regional regulatory experiments (California emissions, Vermont dairy, state labor rules) subject to federal preemption
 *   - Supreme Court (enforcer/agenda-setter): interprets text, applies precedent, can shift the reading via doctrinal overhaul
 *   - Interstate commerce beneficiaries (beneficiaries): multinational firms, national supply chains benefiting from unified regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.68).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.52).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Commerce Clause - Expansive Federal Authority Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional/federalism/economic").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '8308affe-dae9-4112-b1f2-45ba641dab24').
narrative_ontology:cs_kernel_codification('8308affe-dae9-4112-b1f2-45ba641dab24', fixed_text).
narrative_ontology:cs_authority_grounding('8308affe-dae9-4112-b1f2-45ba641dab24', lineage).
narrative_ontology:cs_interpretation_layer_present('8308affe-dae9-4112-b1f2-45ba641dab24').
narrative_ontology:cs_reading_relation('8308affe-dae9-4112-b1f2-45ba641dab24', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('8308affe-dae9-4112-b1f2-45ba641dab24', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('8308affe-dae9-4112-b1f2-45ba641dab24', foundational, substantial_effects_doctrine_jurisdictional).
narrative_ontology:cs_axiom_status(substantial_effects_doctrine_jurisdictional, holdable).
narrative_ontology:cs_axiom_grounding('8308affe-dae9-4112-b1f2-45ba641dab24', substantial_effects_doctrine_jurisdictional, instrumental).
narrative_ontology:cs_axiom('8308affe-dae9-4112-b1f2-45ba641dab24', foundational, federal_regulatory_supremacy_modern_economy).
narrative_ontology:cs_axiom_status(federal_regulatory_supremacy_modern_economy, holdable).
narrative_ontology:cs_axiom_grounding('8308affe-dae9-4112-b1f2-45ba641dab24', federal_regulatory_supremacy_modern_economy, empirically_contingent).
narrative_ontology:cs_reference_frame('8308affe-dae9-4112-b1f2-45ba641dab24', cooperative_federalism_post_new_deal).
narrative_ontology:cs_drift_state('8308affe-dae9-4112-b1f2-45ba641dab24', contemporary_originalist_resurgence, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('8308affe-dae9-4112-b1f2-45ba641dab24', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_variant_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, interstate_commerce_beneficiaries).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, regulatory_commerce_nexus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the Commerce Clause as granting federal authority over any economic activity with substantial aggregate effects on interstate markets. Set regulatory floors, environmental standards, labor protections, and consumer rules nationwide. Their power to set the agenda derives from the reading's success in federal courts; they defend the reading by citing precedent and economic interconnection. The expansion of federal reach increases their jurisdictional scope and administrative resource flow.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from unified national standards in areas where coordination failure or races-to-the-bottom would otherwise occur: environmental protection, worker safety, food and drug standards, financial regulation. They argue the reading prevents regulatory arbitrage where firms relocate to low-regulation states. Their benefit is diffuse but real: cleaner air and water, safer workplaces, reduced systemic financial risk.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, generational, mobile, national).

% Bear the cost of federal preemption of their regulatory authority. Under the expansive reading, state power over local economic activity is subordinated to federal authority whenever the federal government asserts a substantial-effects nexus. States lose the ability to set distinct labor rules, environmental standards, or consumer protections; they must comply with federal floors or face preemption. Their exit option—asserting state sovereignty—is constrained by the reading's entrenchment in constitutional doctrine.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_legislatures, payer,
    organized, generational, constrained, regional).

% Regional and local regulatory experiments—California's environmental rules, Vermont's agricultural protections, state-level healthcare innovation—are continuously subject to federal preemption challenge under the expansive reading. Communities that see their regulatory identity as tied to their economic model (agricultural regions, environmental stewards, labor-protective communities) find that identity constrained by federal doctrine. Exit would mean abandoning the regional identity itself.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_variant_regimes, payer,
    moderate, biographical, identity_locked, local).

% Would argue for a narrower reading tied to the text's original meaning: 'commerce' limited to trade and its instrumentalities, not all economic activity affecting interstate commerce. They contest the reading from outside the institutional decision-making structure; their arguments circulate in academic discourse and amicus briefs but do not set the agenda.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_constitutional_scholars, excluded,
    analytical, generational, analytical, national).

% Interprets the Commerce Clause text and, in recent decades, has established precedent affirming the expansive reading (Wickard, Gonzales v. Raich). The Court can shift the reading by overruling precedent, but doing so would require substantial pressure from originalist arguments or federalism doctrine. The Court sits as both an enforcer of the reading (applying it in cases) and as an agenda-setter (deciding whether the reading stands).
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, supreme_court, observer).

% Businesses operating across state lines and internationally benefit from unified national regulatory standards: they do not face 50 different labor regimes, environmental regimes, or consumer protection rules. A single national floor reduces compliance costs and enables efficient supply chains. They support federal preemption when state regulation would fragment their markets.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, interstate_commerce_beneficiaries, beneficiary,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of regulatory races-to-the-bottom and prevents state regulatory arbitrage that would undermine national economic coherence: where states could lower labor, environmental, or safety standards to attract firms, federal authority establishes unified floors.
% TRANSFER_FUNCTION: Transfers regulatory authority from state governments to federal agencies; transfers economic benefits from firms seeking regulatory arbitrage to consumers and workers benefiting from unified protection standards; transfers identity and autonomy from localities committed to distinct regulatory models to federal administrative uniformity.
% ABSENT_VOICES: Originalist legal scholars and federalism-protection advocates are structurally excluded from setting the regulatory agenda; their voices appear in constitutional argument and dissent but do not determine how courts apply the reading. Rural and regional communities whose economic identity depends on regulatory distinctiveness are not represented in federal standard-setting processes.
% DISAPPEARANCE_RATIONALE: If the expansive reading collapsed—if courts reverted to narrow Commerce Clause authority—the federal government would lose regulatory power over vast economic domains (labor, environment, food safety, financial regulation, telecommunications). States would resume separate regulatory regimes; firms would relocate to low-regulation states; environmental and safety standards would fragment; the administrative state would contract. Entire departments and agencies would lose jurisdiction.
% FOUNDING_PROBLEM: Early commerce regulation under a narrow reading left states able to erect tariff barriers and regulatory obstacles to interstate trade, fragmenting national markets and creating coordination failures in interstate commerce.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and Court assert the founding problem remains live: without expansive federal authority, states would erect regulatory barriers and races-to-the-bottom would occur. Originalist scholars and federalism advocates argue the founding problem is largely historical and has been solved by constitutional amendment and federal-state cooperation; they point to modern state-federal coordination in environmental and labor law as evidence the problem no longer requires such broad federal preemption.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and rising over the interval because the reading's scope expands: each affirmation in case law extends federal reach deeper into traditionally local domains (agriculture → drug regulation → health insurance → environmental standard-setting). Suppression is moderate-to-moderate-rising (0.38→0.52) because the reading's enforcement requires continuous doctrinal maintenance—courts must repeatedly reject state preemption challenges and novel federalism arguments. Theater ratio rises modestly (0.12→0.31) because federal agencies increasingly deploy 'substantial effects' language theatrically: justifying interventions by invoking market interconnection that may be attenuated but doctrinally valid. The measurement grid captures the post-1937 entrenchment of the reading, with inflection points at Wickard (1942), the Civil Rights Act (1964), and Gonzales (2005). Theater rise reflects growing reliance on rhetorical substantial-effects framing as originalist challenges mount.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency seat, the reading is genuine coordination: it prevents races-to-the-bottom and solves collective-action problems in national markets. From the state legislature seat, the same reading operates as jurisdictional extraction: federal authority preempts local decisions. From the originalist scholar seat (excluded), the reading is an interpretive overreach that violates the text's original narrower scope. The engine computes these divergent classifications from the structural data: agenda-setter vs. payer vs. observer seats experience different directionalities despite sharing the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal agencies (d ≈ 0.1, beneficiary end): they set the agenda, defend the reading, expand their jurisdiction, and face no exit. State legislatures (d ≈ 0.85, target end): they are subordinated, constrained in local variation, and exit would require constitutional amendment or sustained doctrinal reversal. National coherence advocates (d ≈ 0.35): they benefit from unified standards but have mobile exit and lower power than agencies. Interstate commerce beneficiaries (d ≈ 0.2): they benefit substantially but are not the reading's designers. Originalist scholars (d ≈ 0.95 as targets, but excluded from the agenda): they bear the cost of marginalization but lack institutional standing to set policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits latent mandatrophy risk: the founding problem (state tariff barriers, regulatory balkanization) was substantially solved by the 1960s, yet the reading's scope continued to expand through Gonzales and beyond. Federal agencies continue to justify novel interventions by invoking 'substantial effects' even where the market nexus is attenuated. Theater ratio rise and theater cost reflect this drift—the coordination story persists while the coordination problem has matured into solved-and-managed. However, the reading's institutional entrenchment (Supreme Court precedent, administrative infrastructure, 90+ years of doctrine) means reversal would be catastrophic for the payer seats, so even a mandatropic constraint persists. This is the structure of a Tangled Rope straining toward Piton: coordination function weakening but enforcement machinery strengthening to maintain the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_effects_indeterminacy,
    'At what threshold of economic interconnection does an intrastate activity acquire a ''substantial effect'' on interstate commerce sufficient to trigger federal authority under this reading?',
    'Examine case law boundaries: activities held to have substantial effects (Wickard wheat, Gonzales marijuana, Heart of Atlanta hotels) vs. activities held not to (Lopez guns, Morrison violence). Identify the rule''s outer boundary empirically.',
    'If the threshold is operationally indeterminate, the reading collapses into a doctrine of federal authority over any economic activity the federal government asserts affects interstate commerce—which converts the reading into pure jurisdictional extraction with minimal constraint. If a boundary can be articulated, the reading retains genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_effects_indeterminacy, empirical, 'Whether ''substantial effects'' has a stable, bounded meaning or functions as rhetorical justification for limitless federal reach.').

omega_variable(
    originalist_textual_contestation,
    'Can the Commerce Clause text, read according to its eighteenth-century original public meaning, bear the expansive reading, or does the reading require departure from original meaning?',
    'Originalist constitutional scholarship (Scalia, Thomas, Barnett, Epstein) and historical analysis of eighteenth-century commercial usage. If the text''s original scope was narrower, the reading is a constructed doctrine, not a discovery.',
    'If the reading departs from original meaning, it is best understood as a deliberate choice to prioritize modern economic coordination over textual constraint—the Tangled Rope structure becomes stark: coordination benefit justified departing from the text''s original limits. If the text can bear both readings with equal historical plausibility, both remain interpretively valid and the contest is genuinely irresolvable within constitutional law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_textual_contestation, conceptual, 'Whether the expansive reading is textually faithful or represents a constructed doctrine that prioritizes coordination over original meaning.').

omega_variable(
    state_autonomy_value_framework,
    'What normative weight should be assigned to state regulatory autonomy as a constitutional good, independent of coordination outcomes?',
    'Constitutional theory debate: federalism as an intrinsic structural good (subsidiarity, local knowledge, preference heterogeneity) vs. federalism as instrumental only to preventing races-to-the-bottom. No empirical resolution; outcome depends on foundational commitments.',
    'If state autonomy is intrinsically valuable, the reading''s extraction cost is higher than its coordination benefit can justify. If autonomy is instrumental only, the reading''s arrangement is defensible. This is the framing omega for the Tangled Rope structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_autonomy_value_framework, preference, 'Whether federalism has intrinsic normative weight beyond instrumental coordination benefits.').

omega_variable(
    sibling_reading_foreclosure,
    'Does this expansive reading logically foreclose the originalist narrow reading within a single constitutional framework, or do both remain live interpretive options?',
    'Examine whether the readings share a single kernel (they do—the Commerce Clause text) and whether acceptance of one requires denial of the other''s internal logic. Both can be internally coherent; they differ in their chosen reading strategy (original vs. modern-effect).',
    'The readings coexist rather than foreclose: this reading dominates institutional authority, but the narrow reading remains available to constitutional challengers and originalist scholars. The structure is coexistence, not logical foreclosure. This shapes the network relationship: influences, not forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'The logical relationship between this reading and originalist alternatives within constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__expansive_federal_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_text__expansive_federal_reading, theater_ratio, 1964, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1964, observed).
narrative_ontology:measurement(comm_tr_t1985, commerce_clause_text__expansive_federal_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(comm_tr_t1985, observed).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__expansive_federal_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__expansive_federal_reading, theater_ratio, 2024, 0.31).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1964, 0.52).
narrative_ontology:measurement_basis(comm_be_t1964, observed).
narrative_ontology:measurement(comm_be_t1985, commerce_clause_text__expansive_federal_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement_basis(comm_be_t1985, observed).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__expansive_federal_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1937, 0.38).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1964, 0.45).
narrative_ontology:measurement_basis(comm_su_t1964, observed).
narrative_ontology:measurement(comm_su_t1985, commerce_clause_text__expansive_federal_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement_basis(comm_su_t1985, observed).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement_basis(comm_su_t2005, observed).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__expansive_federal_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.18).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, regulatory_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, environmental_protection_agency_authority).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, interstate_commerce_beneficiary_lock).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Commerce Clause kernel. The expansive federal reading interprets 'interstate commerce' to encompass all economic activity with substantial aggregate effects on national markets. Sibling readings include originalist_narrow_reading (trade and instrumentalities only) and substantial_effects_limited_reading (federal power to intrastate activity with explicit nexus, excluding pretext). The three readings share the same constitutional text but yield different beneficiary/victim structures, different federal-state power distributions, and different extractiveness profiles. Each reading is a separate constraint story with its own ε value. They are linked via network.affects_constraints to enable constraint-family analysis. The expandive reading drives the most extraction and the broadest federal authority; the originalist reading yields the least extraction and the most state autonomy; the limited reading sits between them, requiring jurisdictional nexus. No single reading is true; all remain live in constitutional discourse and judicial practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, analytical, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
