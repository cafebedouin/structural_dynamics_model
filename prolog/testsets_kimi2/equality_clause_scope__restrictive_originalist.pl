% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Restrictive Originalist Equality Clause Scope Limitation
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive_originalist reading of the
 *   equality_clause_scope kernel. The kernel is the scope of application of
 *   equality guarantees in the social-contract constitutional order. This
 *   reading holds that equality was originally fixed to propertied white
 *   males as a matter of original public meaning, and that any expansion
 *   beyond that class requires formal constitutional amendment. Sibling
 *   readings include expansive_universalist (equality as self-evident truth
 *   for all humans) and progressive_textualist (text contains an expandable
 *   equality principle, but expansion must occur through democratic amendment
 *   rather than judicial reinterpretation). The story models the historical
 *   arrangement described by this reading and its persistent interpretive
 *   force.
 *
 * KEY AGENTS:
 *   - Propertied white males: primary beneficiary (powerful/mobile) â collect political equality and franchise monopoly.
 *   - Enslaved black persons: primary target (powerless/trapped) â bear total extraction of personhood.
 *   - Free women: target (powerless/trapped) â bear extraction of legal standing and franchise.
 *   - Non-propertied white men: secondary target (moderate/constrained) â initially excluded, later partially admitted.
 *   - Indigenous peoples: target (powerless/trapped) â bear extraction of sovereignty and standing.
 *   - Founding propertied elite: agenda setter (institutional/analytical) â authored and embedded the narrow scope.
 *   - Modern originalist interpreters: agenda setter (institutional/analytical) â maintain and enforce the narrow reading against expansion.
 *   - Universalist advocates: excluded (organized/constrained) â argue for universal scope but are ruled out of bounds.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.58).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.55).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.58).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Restrictive Originalist Equality Clause Scope Limitation").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '465a5d3c-9692-4a12-9544-0861a767b735').
narrative_ontology:cs_kernel_codification('465a5d3c-9692-4a12-9544-0861a767b735', fixed_text).
narrative_ontology:cs_authority_grounding('465a5d3c-9692-4a12-9544-0861a767b735', lineage).
narrative_ontology:cs_interpretation_layer_present('465a5d3c-9692-4a12-9544-0861a767b735').
narrative_ontology:cs_reading_relation('465a5d3c-9692-4a12-9544-0861a767b735', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('465a5d3c-9692-4a12-9544-0861a767b735', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('465a5d3c-9692-4a12-9544-0861a767b735', foundational, original_public_meaning_limited_scope).
narrative_ontology:cs_axiom_status(original_public_meaning_limited_scope, holdable).
narrative_ontology:cs_axiom_grounding('465a5d3c-9692-4a12-9544-0861a767b735', original_public_meaning_limited_scope, conventional).
narrative_ontology:cs_axiom('465a5d3c-9692-4a12-9544-0861a767b735', foundational, amendment_required_for_expansion).
narrative_ontology:cs_axiom_status(amendment_required_for_expansion, holdable).
narrative_ontology:cs_axiom_grounding('465a5d3c-9692-4a12-9544-0861a767b735', amendment_required_for_expansion, conventional).
narrative_ontology:cs_reference_frame('465a5d3c-9692-4a12-9544-0861a767b735', founding_era_polity_1787).
narrative_ontology:cs_drift_state('465a5d3c-9692-4a12-9544-0861a767b735', contemporary_constitutional_order, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('465a5d3c-9692-4a12-9544-0861a767b735', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_black_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_white_men).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_jurisprudence).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, social_contract_limited_polity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held exclusive franchise, equal legal standing, and political recognition within the early republic; their property and racial status placed them inside the social contract's protection, while all others were left outside.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_males, beneficiary,
    powerful, generational, mobile, national).

% Denied all legal personhood and equality claims; human status was extinguished under the framework, making them the object of property law rather than subjects of the social contract. Exit meant fugitivity or death.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_black_persons, payer,
    powerless, biographical, trapped, national).

% Excluded from franchise and independent legal personhood through coverture; civil standing flowed from fathers or husbands, and political equality was deemed outside their sphere.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_women, payer,
    powerless, generational, trapped, national).

% Initially barred from franchise by property qualifications and treated as politically dependent; gradually admitted over the nineteenth century but understood as outside the original scope of the equality guarantee.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_white_men, payer,
    moderate, biographical, constrained, national).

% Excluded from the political community and treated as foreign nations or subjects of removal; sovereignty and equality claims were structurally denied within the constitutional framework.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    powerless, generational, trapped, national).

% Authored and ratified the constitutional framework, embedding property and race qualifications into the structure of political equality and setting the interpretive baseline for subsequent generations.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, founding_propertied_elite, agenda_setter,
    institutional, civilizational, analytical, national).

% Judges, scholars, and advocates who enforce the narrow original scope as the binding constitutional meaning, insisting that expansion of equality beyond the founding beneficiary set requires formal constitutional amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, modern_originalist_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Argue that equality is a universal moral truth applying to all humans; their readings are structurally excluded from the originalist interpretive framework as illegitimate or ultra vires.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, universalist_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_males).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates political order, property rights, and limited republican government among the propertied white male elite by establishing a shared framework of mutual recognition and non-tyranny.
% TRANSFER_FUNCTION: Transfers political standing, legal personhood, franchise, and equal protection from excluded populations to the propertied white male class, who alone wield political equality under the framework.
% ABSENT_VOICES: Enslaved persons, women, and indigenous peoples were excluded from the constitutional convention and from the original public meaning; modern universalist advocates and critical theorists are present in discourse but their readings are ruled out of bounds within the originalist framework.
% DISAPPEARANCE_RATIONALE: The sudden universalization of political equality at the founding would have eliminated the slave economy, transformed gender and property relations, and redistributed all political power, completely restructuring the early republic.
% FOUNDING_PROBLEM: Establishing stable republican government and preventing intra-elite tyranny or anarchy among the propertied class in the post-revolutionary Atlantic world.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by historians (Wood, Bailyn) who attest the problem was elite coordination and faction; corroborated by modern critical theorists who attest the exclusionary solution is no longer necessary. Self-attested by the beneficiary class and originalist interpreters as still live, but this is not corroborated from outside the benefiting parties.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 because, although the most severe historical exclusions have been formally abolished, the reading still raises the legitimacy threshold for equality claims and retains moderate extraction by channeling all expansion through the amendment process. Suppression at 0.55 reflects the decline of direct violent enforcement (slavery, coverture) but the persistence of interpretive and doctrinal barriers. Theater_ratio at 0.62 captures the increasing performative quality of the reading: as formal practice has universalized, the originalist frame is maintained through ritual fidelity to a text whose operational meaning has been largely superseded. Accessibility_collapse at 0.75 registers that, within the originalist framework, alternatives to the narrow scope are doctrinally barred. Resistance at 0.72 reflects centuries of abolitionist, feminist, and civil-rights opposition. The measurement series run on one shared time grid so every metric is authored at every examined time point, showing extraction and suppression declining while theatricality rises.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (propertied white males) experiences the constraint as legitimate coordination: a stable republican order protecting property and preventing tyranny. The payer seats experience the identical structure as violent extraction: the denial of personhood, franchise, and standing. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white males are declared beneficiaries with mobile exit, yielding a beneficiary directionality (low d, subsidized by the constraint). Enslaved black persons, free women, and indigenous peoples are declared victims with trapped exit, yielding full-target directionality (high d, amplified extraction). Non-propertied white men sit between, with constrained exit and moderate power. Modern originalist interpreters administer the constraint but do not personally collect the equality monopoly; their directionality is near symmetric but slightly beneficiary because their institutional authority is vested in the narrow reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem â preventing intra-elite anarchy â was genuine, and the reading's classification as tangled_rope preserves that fact against a purely extractive (snare) reading. However, the founding problem is dead: republican government no longer requires the exclusion of women, non-propertied men, or racial minorities. The constraint persists as an interpretive regime whose maintenance now serves to resist rather than enable coordination, creating the rising theater ratio and the mandatrophy mismatch (dead founding problem, world_rearranges disappearance verdict).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusion_as_coordination_or_snare,
    'Was the exclusion of non-propertied groups a necessary side-effect of genuine elite coordination, or was elite coordination always primarily a cover for racialized extraction?',
    'Comparative historical analysis of republican founding moments with and without broad franchise and slavery (e.g., Haitian Revolution, early New England townships).',
    'If exclusion was structurally necessary for the coordination, the tangled_rope classification holds; if coordination was cover, the constraint retrogrades toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_as_coordination_or_snare, conceptual, 'Whether the coordination function was genuine or a cover story.').

omega_variable(
    originalism_as_historical_or_ideological,
    'Does the restrictive originalist reading accurately recover 18th-century public meaning, or does it retroject modern ideological commitments onto the founding?',
    'Historical-linguistic analysis of ''equal protection'' and ''equality'' in founding-era pamphlets, cases, and state constitutional conventions.',
    'If the narrow scope is a retrojection, the constraint''s authority grounding shifts from lineage to extraction, altering its cs_structure classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_as_historical_or_ideological, empirical, 'Historical accuracy of the originalist recovery claim.').

omega_variable(
    kernel_reading_distinction,
    'This reading is one of three sibling readings of the equality_clause_scope kernel. What structural element distinguishes it most sharply from progressive_textualist: the semantic content of the text, or the institutional gate (amendment) for expansion?',
    'Textual and historical analysis of whether the founding documents contain an abstract equality principle separable from its original application.',
    'If the text contains a broad principle, the difference from progressive_textualist narrows to institutional mechanism; if not, the difference is ontological (who is a rights-bearer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Structural boundary between restrictive originalist and progressive textualist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__restrictive_originalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__restrictive_originalist, theater_ratio, 30, 0.2).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__restrictive_originalist, theater_ratio, 60, 0.28).
narrative_ontology:measurement(equa_tr_t90, equality_clause_scope__restrictive_originalist, theater_ratio, 90, 0.35).
narrative_ontology:measurement(equa_tr_t120, equality_clause_scope__restrictive_originalist, theater_ratio, 120, 0.42).
narrative_ontology:measurement(equa_tr_t150, equality_clause_scope__restrictive_originalist, theater_ratio, 150, 0.5).
narrative_ontology:measurement(equa_tr_t180, equality_clause_scope__restrictive_originalist, theater_ratio, 180, 0.55).
narrative_ontology:measurement(equa_tr_t210, equality_clause_scope__restrictive_originalist, theater_ratio, 210, 0.6).
narrative_ontology:measurement(equa_tr_t240, equality_clause_scope__restrictive_originalist, theater_ratio, 240, 0.62).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__restrictive_originalist, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__restrictive_originalist, base_extractiveness, 30, 0.9).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__restrictive_originalist, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(equa_be_t90, equality_clause_scope__restrictive_originalist, base_extractiveness, 90, 0.78).
narrative_ontology:measurement(equa_be_t120, equality_clause_scope__restrictive_originalist, base_extractiveness, 120, 0.72).
narrative_ontology:measurement(equa_be_t150, equality_clause_scope__restrictive_originalist, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(equa_be_t180, equality_clause_scope__restrictive_originalist, base_extractiveness, 180, 0.65).
narrative_ontology:measurement(equa_be_t210, equality_clause_scope__restrictive_originalist, base_extractiveness, 210, 0.61).
narrative_ontology:measurement(equa_be_t240, equality_clause_scope__restrictive_originalist, base_extractiveness, 240, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__restrictive_originalist, suppression_requirement, 0, 0.96).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__restrictive_originalist, suppression_requirement, 30, 0.92).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__restrictive_originalist, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(equa_su_t90, equality_clause_scope__restrictive_originalist, suppression_requirement, 90, 0.8).
narrative_ontology:measurement(equa_su_t120, equality_clause_scope__restrictive_originalist, suppression_requirement, 120, 0.72).
narrative_ontology:measurement(equa_su_t150, equality_clause_scope__restrictive_originalist, suppression_requirement, 150, 0.65).
narrative_ontology:measurement(equa_su_t180, equality_clause_scope__restrictive_originalist, suppression_requirement, 180, 0.6).
narrative_ontology:measurement(equa_su_t210, equality_clause_scope__restrictive_originalist, suppression_requirement, 210, 0.58).
narrative_ontology:measurement(equa_su_t240, equality_clause_scope__restrictive_originalist, suppression_requirement, 240, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the equality_clause_scope kernel, decomposed per the epsilon-invariance principle. The restrictive_originalist reading and its siblings (expansive_universalist, progressive_textualist) are structurally distinct constraints linked by their shared kernel but carrying different epsilon values, beneficiary structures, and directionality profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
