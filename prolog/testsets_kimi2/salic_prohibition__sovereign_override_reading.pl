% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Prohibition (Sovereign Override Reading)
 *   domain: constitutional/dynastic
 *
 * SUMMARY:
 *   The Salic prohibition on female succession is read here not as an
 *   immutable natural or divine mandate, but as revocable positive law
 *   subject to sovereign legislative authority. Under this reading, the
 *   reigning sovereign retains the constitutional power to issue Pragmatic
 *   Sanctions permitting female succession, and challengers to such sovereign
 *   settlements are treated as rebels against legitimate authority rather
 *   than legitimate competitors. The prohibition thus operates as a default
 *   coordination mechanism for dynastic continuity, enforced by the military
 *   and legal apparatus of the state, while concentrating discretionary
 *   authority in the sovereign.
 *
 * KEY AGENTS:
 *   - reigning_sovereign_authority: agenda-setter with dynastic legislative supremacy; controls revocation and enforcement
 *   - male_dynastic_heirs: primary beneficiaries of the default male-preference rule
 *   - dynastic_nobility: secondary beneficiaries supplying enforcement and enjoying stability
 *   - female_succession_claimants: payers excluded by default; can only enter via sovereign exception
 *   - cognatic_claimants: payers with claims through female lines treated as rebels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.74).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Prohibition (Sovereign Override Reading)").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/dynastic").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, 'c51a0ef1-397b-4495-855c-d5c7e3b299fd').
narrative_ontology:cs_kernel_codification('c51a0ef1-397b-4495-855c-d5c7e3b299fd', formalized).
narrative_ontology:cs_authority_grounding('c51a0ef1-397b-4495-855c-d5c7e3b299fd', lineage).
narrative_ontology:cs_reading_relation('c51a0ef1-397b-4495-855c-d5c7e3b299fd', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('c51a0ef1-397b-4495-855c-d5c7e3b299fd', salic_prohibition__cognatic_reversion_reading, influences).
narrative_ontology:cs_axiom('c51a0ef1-397b-4495-855c-d5c7e3b299fd', foundational, dynastic_legislative_supremacy).
narrative_ontology:cs_axiom_status(dynastic_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('c51a0ef1-397b-4495-855c-d5c7e3b299fd', dynastic_legislative_supremacy, conventional).
narrative_ontology:cs_axiom('c51a0ef1-397b-4495-855c-d5c7e3b299fd', foundational, male_preference_positive_default).
narrative_ontology:cs_axiom_status(male_preference_positive_default, holdable).
narrative_ontology:cs_axiom_grounding('c51a0ef1-397b-4495-855c-d5c7e3b299fd', male_preference_positive_default, conventional).
narrative_ontology:cs_reference_frame('c51a0ef1-397b-4495-855c-d5c7e3b299fd', dynastic_legislative_supremacy).
narrative_ontology:cs_drift_state('c51a0ef1-397b-4495-855c-d5c7e3b299fd', pragmatic_sanction_prevalence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c51a0ef1-397b-4495-855c-d5c7e3b299fd', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_sovereign_authority).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, male_dynastic_heirs).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, dynastic_nobility).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, female_succession_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, cognatic_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds and asserts the power to bind and unbind succession rules by legislative act. Issues Pragmatic Sanctions to override the default male-preference prohibition when politically necessary. Commands the military and legal apparatus that treats unauthorized claimants as rebels.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_sovereign_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, reigning_sovereign_authority, beneficiary).

% Stand in the default line of succession ahead of female relatives. Their claim is presumptively valid under the prohibition and is defended by state force unless the sovereign explicitly alters the succession by legislative exception.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, male_dynastic_heirs, beneficiary,
    powerful, biographical, constrained, national).

% Supply military force and political support to enforce the sovereign's succession settlements. Receive land, titles, and relative stability in exchange for backing the male-line default and suppressing unauthorized claimants.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dynastic_nobility, beneficiary,
    organized, generational, constrained, national).

% Are excluded from the default order of succession by the prohibition. May only inherit if the sovereign grants a specific legislative override. If they assert claims without such authorization, they are branded rebels and face military action.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, female_succession_claimants, payer,
    moderate, biographical, trapped, national).

% Claims that run through female ancestors are structurally disqualified under the default rule. Must rely on sovereign grace or accept exclusion; pressing a claim independently triggers the constraint's defensive war mechanism.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, cognatic_claimants, payer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents dynastic civil war by establishing a clear, presumptive male-only succession rule that eliminates ambiguity about who inherits the throne; the sovereign retains override authority to handle exceptional cases without collapsing the default framework.
% TRANSFER_FUNCTION: Transfers presumptive right of succession from female-line and cognatic claimants to male-line heirs; transfers discretionary authority over legitimacy from customary dynastic law to the reigning sovereign's legislative will.
% ABSENT_VOICES: Cognatic reversion advocates who see the Salic rule as a Frankish import illegitimate on non-Frankish soil; female claimants and their lineages who are structurally excluded from the succession conversation unless the sovereign personally admits them.
% DISAPPEARANCE_RATIONALE: If the prohibition and its sovereign-override framework vanished, succession would revert to cognatic or customary alternatives, the sovereign's gatekeeping authority over dynastic legitimacy would collapse, and the political-military structure that treats challengers as rebels would lose its constitutional grounding.
% FOUNDING_PROBLEM: The fragmentation of Carolingian and post-Roman successor kingdoms produced endemic succession warfare; a clear, defensible rule was needed to consolidate male-line dynastic continuity and prevent competing claims from collapsing the realm into civil conflict.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign and male dynastic beneficiaries attest the problem remains live (civil war risk). Cognatic reversion readings and female claimants attest the problem was always territorially specific and that the current arrangement persists to concentrate sovereign authority, not to prevent civil war. External historians and political theorists outside the beneficiary set corroborate that succession crises continued under Salic rules, suggesting the coordination function is partial.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the systematic exclusion of half the dynastic pool from default succession rights, tempered only by exceptional sovereign override. Suppression (0.74) is high because the constraint's persistence requires treating unauthorized claimants as rebels and waging defensive war against them. Theater ratio (0.35) captures the performative invocation of 'ancient custom' to legitimize what functions as a positive-law tool of sovereign discretion. Accessibility collapse (0.68) is high because cognatic alternatives are actively delegitimized; resistance (0.52) is moderate because challengers often have foreign backing or provincial support.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign and male heirs experience the constraint as a stabilizing framework that prevents civil war; excluded claimants experience it as an enforced deprivation of birthright backed by state violence. The engine computes this divergence from shared structural data â the same prohibition reads as coordination from one seat and extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereign and male dynastic heirs are structural beneficiaries (low directionality); their costs are minimal and their authority or presumptive rights are enhanced. Female and cognatic claimants are structural targets (high directionality); they bear the extraction of exclusion and face violent suppression if they resist. The nobility sits nearer symmetric: they pay in blood for enforcement but benefit from reduced civil war risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a pure rope because identifiable victims bear asymmetric costs (exclusion from sovereignty, risk of death as rebels). It is not a pure snare because a genuine coordination function exists: clear default rules reduce dynastic civil war frequency compared to fully contested succession. The Tangled Rope classification captures the hybrid structure: coordination for the male-line polity, extraction for excluded claimants, held together by active military enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_foreclosure,
    'This reading treats the Salic prohibition as revocable positive law; the immutable_mandate reading treats it as irrevocable divine dynastic law. Which structural classification follows if both readings are held by factions with equal military capacity?',
    'Historical case study of succession wars where both readings were backed by armed factions (e.g., War of Spanish Succession, Austrian Pragmatic Sanction contestations).',
    'If revocability consistently produces civil war when contested by immutable-mandate holders, the coordination function is weaker than claimed and extraction (sovereign authority consolidation) dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_foreclosure, empirical, 'Contest between revocable and immutable readings in armed succession crises').

omega_variable(
    territorial_origin_ambiguity,
    'Is the Salic prohibition''s authority derived from Frankish custom that was never properly transferred to non-Frankish dynastic territories, making its application a constructive false summit?',
    'Paleographic and constitutional historical analysis of the prohibition''s transmission into each territorial legal system.',
    'If the prohibition lacks legitimate transmission, the sovereign_override reading becomes a post-hoc rationalization for territorial aggrandizement by male lines, shifting the constraint toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_origin_ambiguity, conceptual, 'Whether Salic Law''s territorial extension is legitimate or constructed').

omega_variable(
    sovereign_discretion_scope,
    'Does sovereign override authority extend to any succession alteration, or only to specific exceptions that preserve the default male-preference structure?',
    'Comparative analysis of Pragmatic Sanctions and similar sovereign acts across European dynastic history.',
    'If override is unlimited, the prohibition is merely symbolic and the constraint is closer to sovereign whim (snare-like); if limited, the coordination function is genuine and the constraint remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_discretion_scope, empirical, 'Scope of sovereign legislative override authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sali_tr_t15, salic_prohibition__sovereign_override_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(sali_tr_t30, salic_prohibition__sovereign_override_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(sali_tr_t45, salic_prohibition__sovereign_override_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(sali_tr_t60, salic_prohibition__sovereign_override_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(sali_tr_t75, salic_prohibition__sovereign_override_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sali_be_t15, salic_prohibition__sovereign_override_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(sali_be_t30, salic_prohibition__sovereign_override_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(sali_be_t45, salic_prohibition__sovereign_override_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(sali_be_t60, salic_prohibition__sovereign_override_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(sali_be_t75, salic_prohibition__sovereign_override_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sali_su_t15, salic_prohibition__sovereign_override_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(sali_su_t30, salic_prohibition__sovereign_override_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(sali_su_t45, salic_prohibition__sovereign_override_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(sali_su_t60, salic_prohibition__sovereign_override_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(sali_su_t75, salic_prohibition__sovereign_override_reading, suppression_requirement, 75, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, cognatic_reversion_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the salic_prohibition kernel. The kernel decomposes into three structurally distinct constraints per the Îµ-invariance principle: sovereign_override_reading (revocable positive law), immutable_mandate_reading (irrevocable divine law), and cognatic_reversion_reading (territorially inapplicable Frankish custom). Each reading carries a distinct Îµ, beneficiary/victim structure, and authority grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
