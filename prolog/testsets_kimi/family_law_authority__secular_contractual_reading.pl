% ============================================================================
% CONSTRAINT STORY: family_law_authority__secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__secular_contractual_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: family_law_authority__secular_contractual_reading
 *   human_readable: Marriage as Civil Contract Under State Law (Secular Contractual Reading)
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the secular_contractual_reading of the
 *   family_law_authority kernel, in which marriage is a civil contract
 *   between autonomous individuals validated exclusively by state
 *   registration, with gender-symmetric and interfaith-permissive structure.
 *   Sibling readings (hindu_dharmashastra_reading, muslim_shariat_reading,
 *   christian_canonical_reading, parsi_zoroastrian_reading) treat marriage as
 *   sacramental, dharmic, or religiously governed. The decomposition follows
 *   Îµ-invariance: the secular reading's structural premises (no religious
 *   requirement, state monopoly on validity) produce a distinct Îµ from
 *   religious readings that embed sacred authority.
 *
 * KEY AGENTS:
 *   - State marriage authority (institutional/agenda_setter): monopolizes legal recognition and administers the civil registry
 *   - Secular households (moderate/beneficiary): gain legal certainty and equal rights without religious conformity
 *   - Interfaith couples (moderate/beneficiary): obtain legal recognition across religious boundaries that religious readings often prohibit
 *   - Religious institutions (organized/payer): lose autonomous jurisdiction over marriage validity and must defer to state registration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, 0.58).
domain_priors:suppression_score(family_law_authority__secular_contractual_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__secular_contractual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__secular_contractual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__secular_contractual_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__secular_contractual_reading, "Marriage as Civil Contract Under State Law (Secular Contractual Reading)").
narrative_ontology:topic_domain(family_law_authority__secular_contractual_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__secular_contractual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__secular_contractual_reading, '1590bedb-7c1d-4819-80ab-88af1f50d615').
narrative_ontology:cs_kernel_codification('1590bedb-7c1d-4819-80ab-88af1f50d615', formalized).
narrative_ontology:cs_authority_grounding('1590bedb-7c1d-4819-80ab-88af1f50d615', lineage).
narrative_ontology:cs_interpretation_layer_present('1590bedb-7c1d-4819-80ab-88af1f50d615').
narrative_ontology:cs_reading_relation('1590bedb-7c1d-4819-80ab-88af1f50d615', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('1590bedb-7c1d-4819-80ab-88af1f50d615', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('1590bedb-7c1d-4819-80ab-88af1f50d615', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('1590bedb-7c1d-4819-80ab-88af1f50d615', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('1590bedb-7c1d-4819-80ab-88af1f50d615', foundational, state_law_exclusive_validity).
narrative_ontology:cs_axiom_status(state_law_exclusive_validity, holdable).
narrative_ontology:cs_axiom_grounding('1590bedb-7c1d-4819-80ab-88af1f50d615', state_law_exclusive_validity, conventional).
narrative_ontology:cs_axiom('1590bedb-7c1d-4819-80ab-88af1f50d615', foundational, gender_symmetric_contractual_autonomy).
narrative_ontology:cs_axiom_status(gender_symmetric_contractual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('1590bedb-7c1d-4819-80ab-88af1f50d615', gender_symmetric_contractual_autonomy, deontological).
narrative_ontology:cs_reference_frame('1590bedb-7c1d-4819-80ab-88af1f50d615', secular_constitutional_marriage_framework).
narrative_ontology:cs_drift_state('1590bedb-7c1d-4819-80ab-88af1f50d615', contemporary_pluralism_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1590bedb-7c1d-4819-80ab-88af1f50d615', '').
narrative_ontology:cs_kernel_id(family_law_authority__secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, secular_households).
narrative_ontology:constraint_beneficiary(family_law_authority__secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_victim(family_law_authority__secular_contractual_reading, religious_institutions).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, state_sovereignty_over_family_law).
narrative_ontology:constraint_vindicates(family_law_authority__secular_contractual_reading, liberal_neutrality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers civil marriage registration and maintains the exclusive legal framework for spousal status, property, and kinship recognition. Validates marriages through state offices and courts; religious ceremonies receive no legal effect without parallel civil registration.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, state_marriage_authority, agenda_setter,
    institutional, generational, constrained, national).

% Enter marriage through state registration to obtain legal certainty in property, inheritance, taxation, and parental rights. They do not need religious approval; their union is valid by state contract alone.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, secular_households, beneficiary,
    moderate, biographical, constrained, national).

% Are permitted to marry and receive full legal recognition across religious boundaries, which religious-governed readings of marriage often prohibit or restrict. Their legal security depends entirely on the state's willingness to register unions that no single religious authority would endorse.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, interfaith_couples, beneficiary,
    moderate, biographical, constrained, national).

% Perform religious marriage ceremonies that lack autonomous legal force; their historical jurisdiction over family law has been absorbed by state civil codes. They must advise adherents to seek separate civil registration for legal validity.
narrative_ontology:constraint_stakeholder(family_law_authority__secular_contractual_reading, religious_institutions, payer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__secular_contractual_reading, state_marriage_authority).
narrative_ontology:fixing_cost_class(family_law_authority__secular_contractual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a uniform, territorially bounded legal status for intimate partnerships that solves property disputes, inheritance claims, child custody arrangements, and immigration sponsorship without requiring religious consensus across plural populations.
% TRANSFER_FUNCTION: Transfers jurisdiction over marriage validity from religious authorities and customary institutions to state registrars and civil courts; transfers legal standing and protective rights to individuals who form unions outside religious boundaries or across them.
% ABSENT_VOICES: Religious jurists (qadis, pandits, priests) and traditional council elders who would argue that sacramental, dharmic, or canonical criteria should independently determine marital validity; they are present in society but structurally excluded from the state's validity determination.
% DISAPPEARANCE_RATIONALE: Inheritance systems, spousal immigration, tax filing, child custody adjudication, and welfare eligibility all depend on the state's marriage registry. Without it, these domains would revert to fragmented religious or customary jurisdictions, and interfaith partnerships would lose legal protection.
% FOUNDING_PROBLEM: Early modern religious wars and colonial legal pluralism produced overlapping, contradictory marriage jurisdictions across confessions and communities, leading to uncertain legitimacy, property conflict, and political violence over whose law governed family life.
% FOUNDING_PROBLEM_CORROBORATION: Liberal constitutional historians attest the founding problem as historically live. Contemporary religious authorities and post-colonial legal pluralists contest that the problem is solved, arguing that secular civil marriage is itself a sectarian imposition; no corroborator fully outside these competing frameworks exists.
narrative_ontology:disappearance_verdict(family_law_authority__secular_contractual_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__secular_contractual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__secular_contractual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__secular_contractual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__secular_contractual_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__secular_contractual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__secular_contractual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-high because the constraint displaces religious jurisdiction over a domain historically governed by sacred law, transferring authority to state bureaucracies without monetary rent but with significant sovereign extraction. Suppression (0.62) reflects that religious-only marriages are legally void for most civil purposes, collapsing alternatives for couples who need inheritance, immigration, or welfare protections. Theater ratio (0.28) captures the residual ceremonial performativity and bureaucratic ritual that surrounds the contractual form. The measurement series run on a single shared time grid to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   From the secular-citizen and interfaith-couple seats, the constraint reads as emancipatory coordination that removes religious barriers and secures equal rights. From the religious-institution seat, it reads as sovereign confiscation of a sacred jurisdiction, where the state extracts legitimacy by declaring its own procedure the sole gateway to legal personhood as a spouse.
 *
 * DIRECTIONALITY LOGIC:
 *   The state marriage authority sits near the beneficiary end in sovereignty terms (it gains jurisdictional monopoly) but is not a financial rentier. Religious institutions are clear targets: they bear the cost of displaced authority and must subsidize the state system by advising adherents to undergo dual ceremonies. Secular households and interfaith couples are beneficiaries with constrained exit: they need the registration for legal protections, but the constraint subsidizes their status at the expense of religious autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (religious conflict over marriage jurisdiction) is arguably dead in highly secularized states, yet the constraint persists because it has been repurposed for welfare administration, immigration control, and child-custody adjudication. This mandate shift prevents piton classificationâthe function remains live and non-theatrical for a large populationâbut it also prevents pure rope classification because the state enforces a monopoly that suppresses alternative validity criteria.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_separation,
    'Is the secular state''s marriage monopoly one reading among many of a shared kernel, or has it displaced religious kernels entirely in jurisdictions where it holds?',
    'Comparative constitutional analysis of states with optional civil marriage versus states with mandatory civil marriage; measure whether religious readings retain parallel legal validity or are reduced to purely social ceremony.',
    'If religious readings retain parallel legal force, the secular reading may be rope-like (genuine coordination among alternatives); if they are fully displaced, the secular reading is more extractive and the classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Whether the secular reading coexists with or displaces religious kernels').

omega_variable(
    religious_authority_extraction,
    'Does the secular state''s monopoly on marriage validity extract jurisdiction from religious institutions, or merely provide a parallel track?',
    'Cross-jurisdictional comparison of legal sequelae for unregistered religious marriage: inheritance standing, spousal immigration, and child custody outcomes in states with sole state registration versus dual-recognition systems.',
    'If unregistered religious marriage carries zero civil sequelae, the constraint''s effective extraction is higher than if religious marriage enjoys nested or partial recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_extraction, empirical, 'Whether state monopoly extracts jurisdiction or runs parallel to it').

omega_variable(
    gender_symmetry_implementation_gap,
    'Do gender-symmetric rights on paper translate to symmetric outcomes in judicial practice and social fact within civil marriage regimes?',
    'Empirical study of divorce settlement distributions, alimony patterns, custody awards, and property division in purportedly gender-symmetric civil marriage jurisdictions.',
    'If practice remains asymmetric, the coordination claim is partly theatrical and the constraint extracts disproportionately from one gender despite its contractual framing of equality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_symmetry_implementation_gap, empirical, 'Whether gender symmetry is implemented or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__secular_contractual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fla_scr_tr_t0, family_law_authority__secular_contractual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fla_scr_tr_t20, family_law_authority__secular_contractual_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(fla_scr_tr_t40, family_law_authority__secular_contractual_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(fla_scr_tr_t60, family_law_authority__secular_contractual_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(fla_scr_tr_t80, family_law_authority__secular_contractual_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(fla_scr_tr_t100, family_law_authority__secular_contractual_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(fla_scr_be_t0, family_law_authority__secular_contractual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fla_scr_be_t20, family_law_authority__secular_contractual_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(fla_scr_be_t40, family_law_authority__secular_contractual_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(fla_scr_be_t60, family_law_authority__secular_contractual_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(fla_scr_be_t80, family_law_authority__secular_contractual_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(fla_scr_be_t100, family_law_authority__secular_contractual_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fla_scr_su_t0, family_law_authority__secular_contractual_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(fla_scr_su_t20, family_law_authority__secular_contractual_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(fla_scr_su_t40, family_law_authority__secular_contractual_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(fla_scr_su_t60, family_law_authority__secular_contractual_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(fla_scr_su_t80, family_law_authority__secular_contractual_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement(fla_scr_su_t100, family_law_authority__secular_contractual_reading, suppression_requirement, 100, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__secular_contractual_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__secular_contractual_reading, muslim_shariat_reading).

% DUAL FORMULATION NOTE:
% This constraint is the secular_contractual_reading of the family_law_authority kernel, decomposed from religious readings (hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, parsi_zoroastrian_reading) per the Îµ-invariance principle: the core premise of state-exclusive validity is structurally distinct from sacramental or dharmic validity criteria, producing different Îµ values and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
