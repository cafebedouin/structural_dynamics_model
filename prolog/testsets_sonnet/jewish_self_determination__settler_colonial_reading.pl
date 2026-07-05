% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionism as European Settler-Colonial Dispossession (Settler-Colonial Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates the settler-colonial reading of the
 *   jewish_self_determination kernel: Zionism as a project organized by
 *   European Jewish settlement institutions that, in establishing Jewish
 *   sovereignty over Mandatory Palestine, dispossessed the indigenous
 *   Palestinian Arab population through displacement (1948 and after),
 *   military occupation (1967 and after), and asymmetric legal status (the
 *   Law of Return granting automatic citizenship to Jewish immigrants while
 *   barring Palestinian refugee return). This is ONE of five sibling readings
 *   of the same kernel — liberal_nationalist_reading,
 *   indigenous_return_reading, religious_covenant_reading, and
 *   diasporist_reading are separate constraint stories with their own ε,
 *   beneficiary/victim structures, and classifications. This reading's ε is
 *   high and stable-to-rising because the reading holds that extraction
 *   (land, water, citizenship privilege) continues and compounds through
 *   ongoing settlement expansion; the liberal_nationalist_reading and
 *   indigenous_return_reading siblings would author substantially lower ε for
 *   the same historical events because they locate the coordination function
 *   (national self-determination, indigenous return) as dominant rather than
 *   the extraction function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionism as European Settler-Colonial Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '319d6cc1-aa6b-4b43-9530-d2c1d62b01d4').
narrative_ontology:cs_kernel_codification('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', distributed).
narrative_ontology:cs_authority_grounding('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', distributed).
narrative_ontology:cs_reading_relation('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', foundational, prior_indigenous_occupancy_grounds_land_claim_priority).
narrative_ontology:cs_axiom_status(prior_indigenous_occupancy_grounds_land_claim_priority, holdable).
narrative_ontology:cs_axiom_grounding('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', prior_indigenous_occupancy_grounds_land_claim_priority, empirically_contingent).
narrative_ontology:cs_axiom('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', foundational, settlement_institutions_constitute_colonial_apparatus_regardless_of_founders_intent).
narrative_ontology:cs_axiom_status(settlement_institutions_constitute_colonial_apparatus_regardless_of_founders_intent, holdable).
narrative_ontology:cs_axiom_grounding('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', settlement_institutions_constitute_colonial_apparatus_regardless_of_founders_intent, conventional).
narrative_ontology:cs_reference_frame('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', indigenous_palestinian_continuous_presence).
narrative_ontology:cs_drift_state('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', post_1993_oslo_and_settlement_expansion_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('319d6cc1-aa6b-4b43-9530-d2c1d62b01d4', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, jewish_national_fund_and_settlement_institutions).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs_displaced_1948).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees_denied_return).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land allocation, citizenship law, military governance of occupied territory, and settlement expansion policy. Sets and enforces the legal architecture — Law of Return, land trusts, military administration — that channels land and resources toward Jewish settlement and away from Palestinian claimants. Can revise this architecture but has structural incentive not to.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Arrived under Zionist settlement institutions with organized capital, international diplomatic backing, and legal preference under emerging state structures. Received land, citizenship, and state protection unavailable to the indigenous population on the same territory. Many retain exit options (foreign citizenship, diaspora ties) unavailable to Palestinians.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    organized, generational, mobile, national).

% Pre-state and state-era institutions that acquired, held, and allocated land explicitly for Jewish settlement, structurally excluding non-Jewish use even where formally 'public.' Continues to administer land trusts that reproduce the original exclusion pattern.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, jewish_national_fund_and_settlement_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, jewish_national_fund_and_settlement_institutions, beneficiary).

% Expelled or fled during the 1948 war and subsequent operations; barred by law and policy from returning to homes and land now held by the state or private Jewish ownership. Bear the direct cost of the dispossession the constraint names; multi-generational refugee status persists.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs_displaced_1948, payer,
    powerless, civilizational, trapped, regional).

% Live under military law, settlement expansion, checkpoint regimes, and differential legal status (compared to Jewish settlers on the same land) in the West Bank and historically Gaza. Cannot access citizenship, freedom of movement, or land protections available to settlers nearby.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, regional).

% Descendants of 1948 and 1967 refugees held in camps across neighboring states and the diaspora, formally barred from return by Israeli law while Jewish diaspora members are granted automatic return rights under the Law of Return — the core legal asymmetry this reading identifies.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees_denied_return, payer,
    powerless, civilizational, trapped, regional).

% Great powers (British Mandate authorities historically, Western states currently) provided the diplomatic, financial, and military backing that made settlement and state consolidation possible, but are not named as parties inside the domestic legal architecture and rarely bear direct accountability for the dispossession they enabled.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_diplomatic_patrons, excluded,
    institutional, generational, analytical, global).

% Analyzes the constraint using comparative settler-colonial frameworks (Algeria, South Africa, Australia, the Americas), documenting displacement patterns, legal exclusion mechanisms, and demographic engineering as structurally continuous with other settler-colonial cases.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, postcolonial_scholarship_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transfer of land, sovereignty, and demographic control to a settler population organized through Zionist institutions, providing that population security, citizenship, and territorial consolidation that would not otherwise be achievable against indigenous claims.
% TRANSFER_FUNCTION: Moves land, water rights, citizenship privilege, and physical security from the indigenous Palestinian population to the settler population and the state apparatus built to represent it, enforced through military administration, legal asymmetry (Law of Return vs. denial of Palestinian return), and settlement expansion.
% ABSENT_VOICES: Palestinian refugees and their descendants, whose claims are foreclosed by law rather than argument, are structurally absent from the domestic legal and political process that adjudicates return, citizenship, and land restitution; international patron states that enabled the settlement project are also outside the accountability frame this reading identifies.
% DISAPPEARANCE_RATIONALE: If the legal-military architecture enforcing settler preference and Palestinian exclusion were removed overnight, land allocation, citizenship law, and territorial control would have to be renegotiated from a substantially different starting point — refugee return claims, land restitution, and equal citizenship would become live questions rather than foreclosed ones.
% FOUNDING_PROBLEM: The founding problem, as this reading frames it, was securing land, sovereignty, and demographic majority for an incoming settler population on territory already inhabited by an indigenous population that did not consent to the project.
% FOUNDING_PROBLEM_CORROBORATION: United Nations human rights bodies, B'Tselem and other Israeli human rights organizations, and comparative settler-colonial scholars outside the Zionist institutional structure attest that the exclusionary legal architecture (land trusts, differential citizenship, occupation law) remains active and expanding rather than resolved; this corroboration comes from sources outside the beneficiary institutions named above.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.82, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) and rising over the interval because the reading holds that dispossession compounds structurally — 1948 displacement, 1967 occupation, and ongoing settlement expansion in the West Bank represent an accumulating rather than one-time transfer. Suppression is high (0.78) because the reading holds the arrangement depends on active military and legal enforcement (checkpoints, permit regimes, land expropriation law) rather than voluntary participant consent. Theater ratio is moderate (0.4) reflecting the reading's view that some genuine security coordination exists for the settler population alongside a growing share of enforcement activity that serves exclusion rather than security. Resistance is authored very high (0.85) — Palestinian political resistance, international solidarity movements, and legal challenges are continuous and organized. Accessibility collapse is moderate (0.6) rather than near-total because the reading holds some legal and diplomatic avenues (UN resolutions, international courts, BDS) remain contested rather than fully foreclosed.
 *
 * DIRECTIONALITY LOGIC:
 *   European Jewish settlers and the institutions that organized settlement (Jewish National Fund, pre-state Zionist bodies, and their successor state apparatus) are authored as the structural beneficiaries — they received land, citizenship, and security backed by international patronage, placing them near the beneficiary end of directionality. Palestinian Arabs across all three named victim groups (1948 displaced, those under ongoing occupation, and refugees denied return) are authored as targets with trapped exit options — geography, legal statelessness, and denial of the right of return leave no meaningful mobility. The state apparatus and settlement institutions carry dual agenda-setter/beneficiary roles because they administer the exclusionary architecture AND collect its benefits directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as 'live' rather than 'dead' specifically because this reading holds the extraction is ongoing (ε rising through 2024), not merely inertial residue of a settled historical event — this distinguishes the reading from a piton reading, which would hold the coercive machinery persists past its function. The reading's structural claim is that the mechanism actively continues to serve its original extraction/displacement function, which is precisely why it is authored snare and not piton or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_jewish_self_determination,
    'Among the five declared readings of the jewish_self_determination kernel (settler_colonial, liberal_nationalist, indigenous_return, religious_covenant, diasporist), which reading''s premises should govern classification of any single historical event (e.g., 1948) — and can more than one reading be simultaneously ''true'' of the same event for different parties?',
    'No empirical resolution mechanism resolves this: the readings differ on foundational premises (whether Jewish people are indigenous or foreign to the land, whether national self-determination claims are commensurable with indigenous decolonization claims) that are not settled by additional historical evidence alone, though historical and demographic evidence bears on specific sub-claims within each reading.',
    'If the indigenous_return_reading''s premise (unbroken Jewish indigeneity) is accepted as the governing frame, this reading''s core classification (settler-colonial extraction from a prior indigenous population) is foreclosed, because the settler/indigenous roles invert. If this reading''s premise is accepted, the indigenous_return_reading is correspondingly foreclosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_jewish_self_determination, conceptual, 'Irreducible framing contest between settler-colonial and indigenous-return readings of the same kernel.').

omega_variable(
    law_of_return_asymmetry_naturalness,
    'Is the Law of Return''s asymmetry (automatic Jewish immigration/citizenship rights vs. denial of Palestinian refugee return) a constructed legal-political choice that could be revised, or does it follow necessarily from any coherent conception of a Jewish-majority state?',
    'Comparative analysis of other ethnic-preference immigration/return regimes (Germany''s historical Aussiedler law, Ireland''s Certificate of Irish Heritage, etc.) and their revision histories would bear on whether such asymmetries are typically negotiable or structurally load-bearing for the state''s self-definition.',
    'If the asymmetry is a negotiable policy choice, ε attributable to it could fall with legal reform without dissolving the state; if it is load-bearing for the state''s defining character, the reading''s high suppression/extraction score is more deeply structural and less reformable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_of_return_asymmetry_naturalness, empirical, 'Whether the Law of Return asymmetry is a reformable policy or structurally necessary to the state''s self-definition.').

omega_variable(
    settler_population_internal_heterogeneity,
    'Does treating ''european_jewish_settlers'' as a single beneficiary group erase significant internal heterogeneity — including Mizrahi and Sephardi Jewish populations displaced from Arab and Muslim countries, who arrived under different circumstances and hold contested status within the settler-colonial frame?',
    'Disaggregated demographic and legal-status analysis of Ashkenazi/European vs. Mizrahi/Sephardi Jewish populations in Israel, including their own displacement histories, would clarify whether the beneficiary category should be split into further constraint stories.',
    'If Mizrahi and Sephardi populations are better modeled as a distinct group with their own displacement history and a different, more contested relationship to the beneficiary role, this story''s beneficiary declaration is over-broad and a decomposition (per the ε-invariance principle) may be warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_population_internal_heterogeneity, conceptual, 'Whether the settler beneficiary category requires further decomposition given Mizrahi/Sephardi Jewish displacement histories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_self_determination__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jewi_tr_t1990, jewish_self_determination__settler_colonial_reading, theater_ratio, 1990, 0.32).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__settler_colonial_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.74).
narrative_ontology:measurement(jewi_be_t1990, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1990, 0.77).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(jewi_su_t1990, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraint stories decomposed from the jewish_self_determination kernel per the ε-invariance principle. Each sibling reading (liberal_nationalist_reading, indigenous_return_reading, religious_covenant_reading, diasporist_reading) authors its own ε, beneficiary/victim structure, and claimed_type from its own foundational premises. This story authors high, rising ε and a snare classification because it holds the coordination story (Jewish self-determination and safety) is cover for asymmetric extraction from an indigenous population sustained by active enforcement. The indigenous_return_reading sibling inverts the settler/indigenous role assignment entirely and would author correspondingly different beneficiary/victim declarations. Do not average across these stories; they are structurally distinct constraints linked only by shared kernel ancestry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
