% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Reading of Sovereign Legitimacy
 *   domain: political/constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the republican reading of the
 *   sovereign_legitimacy kernel: the claim that all legitimate political
 *   authority derives from popular sovereignty and is transmitted upward
 *   through delegated consent and electoral mechanisms. It functions as a
 *   coordination mechanism for large-scale collective self-governanceâthe
 *   enfranchised citizenry benefit from constitutional order and
 *   representationâwhile asymmetrically extracting from those excluded from
 *   the franchise or persistently marginalized by majoritarian outcomes. The
 *   constraint requires active enforcement through electoral administration,
 *   constitutional review, and the suppression of rival legitimacy claims.
 *   The kernel is contested: the monarchical reading holds authority flows
 *   downward by divine or hereditary right; the constitutional hybrid blends
 *   both. This reading forecloses the monarchical premise within any
 *   republican framework and exerts democratizing pressure on hybrid regimes.
 *
 * KEY AGENTS:
 *   - Enfranchised citizenry: Primary beneficiary (organized/constrained) â their aggregated consent is the claimed source of authority.
 *   - Disenfranchised residents: Primary target (powerless/trapped) â bear governance costs without franchise or recourse.
 *   - Structural minorities: Secondary target (moderate/constrained) â hold formal rights but suffer majoritarian tyranny.
 *   - Democratic state apparatus: Agenda setter (institutional/constrained) â administers elections and enforces constitutional order.
 *   - Constitutional judiciary: Agenda setter (institutional/constrained) â polices legitimacy boundaries and permissible exclusions.
 *   - Monarchist factions: Excluded voice (organized/constrained) â advocate rival legitimacy frameworks and are marginalized.
 *   - Comparative legitimacy scholars: Analytical observer (analytical/global) â track legitimacy correlates across regimes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.58).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.45).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '3e0d48aa-3553-4507-9218-80390936d6b1').
narrative_ontology:cs_kernel_codification('3e0d48aa-3553-4507-9218-80390936d6b1', formalized).
narrative_ontology:cs_authority_grounding('3e0d48aa-3553-4507-9218-80390936d6b1', lineage).
narrative_ontology:cs_interpretation_layer_present('3e0d48aa-3553-4507-9218-80390936d6b1').
narrative_ontology:cs_reading_relation('3e0d48aa-3553-4507-9218-80390936d6b1', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('3e0d48aa-3553-4507-9218-80390936d6b1', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('3e0d48aa-3553-4507-9218-80390936d6b1', foundational, popular_sovereignty_exclusive).
narrative_ontology:cs_axiom_status(popular_sovereignty_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('3e0d48aa-3553-4507-9218-80390936d6b1', popular_sovereignty_exclusive, deontological).
narrative_ontology:cs_axiom('3e0d48aa-3553-4507-9218-80390936d6b1', foundational, delegated_consent_requirement).
narrative_ontology:cs_axiom_status(delegated_consent_requirement, holdable).
narrative_ontology:cs_axiom_grounding('3e0d48aa-3553-4507-9218-80390936d6b1', delegated_consent_requirement, conventional).
narrative_ontology:cs_reference_frame('3e0d48aa-3553-4507-9218-80390936d6b1', popular_sovereignty_framework).
narrative_ontology:cs_drift_state('3e0d48aa-3553-4507-9218-80390936d6b1', contemporary_illiberal_democracy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e0d48aa-3553-4507-9218-80390936d6b1', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, enfranchised_citizenry).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, disenfranchised_residents).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, structural_minorities).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__republican_reading, social_contract_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold voting rights and participatory channels; their aggregated consent is claimed as the source of legitimate authority. They benefit from formal equality, constitutional protections, and channels of redress, though their actual influence is mediated by representation and unequal resources.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, enfranchised_citizenry, beneficiary,
    organized, generational, constrained, national).

% Residents subject to state authority but excluded from franchise or formal consent mechanisms, including non-citizens, disqualified voters, and minors. They bear governance costsâtaxes, compliance, penaltiesâwithout recourse to the validation mechanism that claims to authorize their governance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, disenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% Groups that hold formal voting rights but are persistently outvoted on vital interests, experiencing majoritarian tyranny. The republican framework coordinates the majority's preferences while these minorities pay the cost of policies they did not effectively consent to.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, structural_minorities, payer,
    moderate, generational, constrained, national).

% Elected and appointed officials who exercise delegated authority. They administer elections, enforce constitutional boundaries, and depend on the republican narrative for their own legitimacy. They set the policy agenda within the delegated framework and enforce compliance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, democratic_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Courts that review legislation and executive action against constitutional standards. They police the boundaries of legitimate delegation, deciding whose participation counts and whose exclusions are permissible under the republican framework.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Groups advocating alternative legitimacy frameworks such as divine right, hereditary rule, or authoritarian centralism. They are structurally marginalized in republican discourse and would object to the upward-flow consent model if given equal voice in the constitutional order.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, monarchist_factions, excluded,
    organized, generational, constrained, national).

% Academic analysts comparing republican, monarchical, and hybrid legitimacy claims across regimes. They track empirical correlates of legitimacy such as compliance rates, stability, and exit behavior without being governed by the specific constraint.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__republican_reading, comparative_legitimacy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for large-scale collective self-governance where authority is validated through popular consent, enabling coordination without recourse to hereditary or arbitrary rule.
% TRANSFER_FUNCTION: Moves authority from the dispersed populace to state institutions through electoral delegation; moves compliance burdens and policy costs from the state to the governed, with asymmetric concentration on those excluded from or marginalized within the consent mechanism.
% ABSENT_VOICES: Monarchist factions arguing for inherited sovereignty, disenfranchised residents who lack voting rights, and permanent minorities whose interests are consistently overridden by majorities are formally or effectively excluded from the consent narrative that claims to authorize their governance.
% DISAPPEARANCE_RATIONALE: If the upward-flow consent model disappeared, the constitutional order would lose its primary legitimacy narrative. Governments would face crises of authority, compliance would fragment, and rival legitimacy claimsâmonarchical, authoritarian, secessionistâwould mobilize to reorganize the political space.
% FOUNDING_PROBLEM: How to justify and stabilize political authority without recourse to divine right, hereditary succession, or naked coercion; how to ground governmental power in the will of the governed rather than in tradition or force.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists such as Pettit and Habermas attest the problem remains live through the lens of non-domination and deliberative deficits. Comparative political scientists note that regimes claiming republican legitimacy vary widely in coercion and corruption levels, suggesting the founding problem is addressed unevenly. Monarchist and anarchist critics from outside the beneficiary set argue the social contract narrative is itself a cover story, providing dissenting external corroboration that the problem remains contested.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__republican_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__republican_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because while the framework enables genuine self-governance, it systematically concentrates costs on excluded groups and permanent minorities. Suppression (0.45) reflects the active exclusion of monarchical and authoritarian alternatives, which are suppressed but not eliminated. Theater ratio (0.52) captures the performative dimension: electoral rituals and constitutional veneration increasingly substitute for substantive popular control and responsive governance. Accessibility collapse (0.65) registers that, once socialized into the republican framework, most citizens rarely imagine legitimate alternatives to electoral delegation. Resistance (0.40) indicates intermittent but persistent challenge from excluded groups and rival factions.
 *
 * PERSPECTIVAL GAP:
 *   The enfranchised majority experiences the constraint as rope or scaffoldâself-governance through recognized channelsâwhile disenfranchised residents and structural minorities experience it as snare or tangled_rope: governance without meaningful consent, justified by a legitimacy narrative from which they are structurally excluded. The state apparatus experiences it as an enforcement mechanism necessary for order and stability, sitting closer to symmetric due to its delegated but bounded authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Enfranchised citizenry are declared beneficiaries (low d, subsidized by the legitimacy order they collectively control). Disenfranchised residents and structural minorities are declared victims (high d, extraction amplified by trapped or constrained exit and national scope). The democratic state apparatus and constitutional judiciary sit near symmetric: they administer the constraint and derive institutional stability from it, but are themselves constrained by electoral and legal accountability. Monarchist factions are structurally excluded (no seat, high d if considered).
 *
 * MANDATROPHY ANALYSIS:
 *   Classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) by requiring the victim setâthose excluded from the consent mechanismâwhile preventing mislabeling as pure extraction (snare) by acknowledging the genuine coordination function of electoral self-governance. The rising theater ratio over the interval signals drift risk: when electoral ritual decouples from substantive accountability, the constraint may slide toward piton (performative legitimacy without functional control) or intensify into snare (majoritarian tyranny). The founding problem remains contested, confirming the arrangement has not fully transcended its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_boundary_ambiguity,
    'Is the constraint''s extraction driven by formal exclusion from the franchise, or by structural power asymmetries that persist despite formal inclusion?',
    'Comparative analysis of disenfranchised versus enfranchised-but-marginalized groups across regimes with varying franchise breadth.',
    'If formal exclusion is the main driver, suffrage expansion reduces epsilon; if structural marginalization dominates, epsilon remains high regardless of franchise breadth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_boundary_ambiguity, empirical, 'Whether extraction is rooted in formal disenfranchisement or persistent majoritarian tyranny.').

omega_variable(
    consent_fiction_or_fact,
    'Is popular consent an empirically verifiable flow of delegation, or a legitimating fiction that coordinates through belief alone?',
    'Sociological measurement of voter efficacy beliefs, policy responsiveness studies, and exit/voice behavior.',
    'If pure fiction, the coordination function is theatrical and the constraint trends toward snare or piton; if verifiable, the coordination is genuine and the reading remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_fiction_or_fact, conceptual, 'Whether popular sovereignty is empirical coordination or legitimating narrative.').

omega_variable(
    kernel_reading_scope,
    'Does the republican reading foreclose the constitutional hybrid reading, or merely exert democratizing influence on it?',
    'Comparative constitutional analysis of whether dual-source legitimacy frameworks are structurally stable or transitional toward republicanism.',
    'If foreclosed, the republican reading claims universal scope and the hybrid is an unstable compound; if influence, mixed regimes are persistent and the constraint family admits long-term coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope, conceptual, 'Structural relationship between republican and hybrid readings of sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_leg_rep_tr_t0, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sov_leg_rep_tr_t50, sovereign_legitimacy__republican_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(sov_leg_rep_tr_t100, sovereign_legitimacy__republican_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(sov_leg_rep_tr_t150, sovereign_legitimacy__republican_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(sov_leg_rep_tr_t200, sovereign_legitimacy__republican_reading, theater_ratio, 200, 0.45).
narrative_ontology:measurement(sov_leg_rep_tr_t250, sovereign_legitimacy__republican_reading, theater_ratio, 250, 0.52).

% Extraction over time
narrative_ontology:measurement(sov_leg_rep_be_t0, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sov_leg_rep_be_t50, sovereign_legitimacy__republican_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(sov_leg_rep_be_t100, sovereign_legitimacy__republican_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(sov_leg_rep_be_t150, sovereign_legitimacy__republican_reading, base_extractiveness, 150, 0.48).
narrative_ontology:measurement(sov_leg_rep_be_t200, sovereign_legitimacy__republican_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(sov_leg_rep_be_t250, sovereign_legitimacy__republican_reading, base_extractiveness, 250, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sov_leg_rep_su_t0, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sov_leg_rep_su_t50, sovereign_legitimacy__republican_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(sov_leg_rep_su_t100, sovereign_legitimacy__republican_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(sov_leg_rep_su_t150, sovereign_legitimacy__republican_reading, suppression_requirement, 150, 0.5).
narrative_ontology:measurement(sov_leg_rep_su_t200, sovereign_legitimacy__republican_reading, suppression_requirement, 200, 0.55).
narrative_ontology:measurement(sov_leg_rep_su_t250, sovereign_legitimacy__republican_reading, suppression_requirement, 250, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
