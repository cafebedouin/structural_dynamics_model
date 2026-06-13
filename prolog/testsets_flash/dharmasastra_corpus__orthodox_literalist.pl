% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra as Eternal, Revealed Truth (Orthodox-Literalist Reading)
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the orthodox-literalist reading of
 *   Dharmasastra, which asserts that its prescriptions, particularly the
 *   varna/jati (caste) hierarchy, are eternal, divinely revealed truths
 *   requiring strict adherence. This reading positions the texts as immutable
 *   natural law, justifying a social order that grants significant privilege
 *   to upper castes while imposing severe restrictions and disadvantages on
 *   lower castes and women. The constraint is actively enforced through
 *   social ostracism, religious injunctions, and historical legal frameworks,
 *   leading to high extraction and suppression for its victims.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Primary beneficiary (institutional/arbitrage) — benefits from and administers the constraint.
 *   - upper_caste_elites: Primary beneficiary (powerful/mobile) — benefits from social and economic advantages.
 *   - dalits: Primary victim (powerless/trapped) — bears severe extraction and suppression.
 *   - shudras: Primary victim (powerless/constrained) — bears significant extraction and suppression.
 *   - women: Primary victim (powerless/identity_locked) — bears extraction through gendered roles and restrictions.
 *   - orthodox_religious_institutions: Agenda setter (institutional/constrained) — enforces and propagates the literalist reading.
 *   - reformist_scholars: Observer (analytical/mobile) — challenges the literalist reading and proposes contextual interpretations.
 *   - human_rights_advocates: Observer (organized/mobile) — actively resists the constraint and advocates for its abolition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.9).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.85).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.9).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, snare).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra as Eternal, Revealed Truth (Orthodox-Literalist Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '9e39b1f7-e61d-41ce-a0c8-27868254d874').
narrative_ontology:cs_kernel_codification('9e39b1f7-e61d-41ce-a0c8-27868254d874', fixed_text).
narrative_ontology:cs_authority_grounding('9e39b1f7-e61d-41ce-a0c8-27868254d874', lineage).
narrative_ontology:cs_interpretation_layer_present('9e39b1f7-e61d-41ce-a0c8-27868254d874').
narrative_ontology:cs_reading_relation('9e39b1f7-e61d-41ce-a0c8-27868254d874', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('9e39b1f7-e61d-41ce-a0c8-27868254d874', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('9e39b1f7-e61d-41ce-a0c8-27868254d874', foundational, dharmasastra_is_eternal_revealed_truth).
narrative_ontology:cs_axiom_status(dharmasastra_is_eternal_revealed_truth, holdable).
narrative_ontology:cs_axiom_grounding('9e39b1f7-e61d-41ce-a0c8-27868254d874', dharmasastra_is_eternal_revealed_truth, theological).
narrative_ontology:cs_axiom('9e39b1f7-e61d-41ce-a0c8-27868254d874', foundational, varna_jati_hierarchy_is_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_jati_hierarchy_is_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('9e39b1f7-e61d-41ce-a0c8-27868254d874', varna_jati_hierarchy_is_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('9e39b1f7-e61d-41ce-a0c8-27868254d874', vedic_social_order).
narrative_ontology:cs_drift_state('9e39b1f7-e61d-41ce-a0c8-27868254d874', contemporary_globalized_society, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9e39b1f7-e61d-41ce-a0c8-27868254d874', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_elites).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, orthodox_religious_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, lower_caste_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers rituals, interprets texts, and holds a privileged position in the varna hierarchy, benefiting from offerings, social deference, and exclusive access to knowledge. Their authority is directly tied to the literal observance of Dharmasastra.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from social status, economic opportunities, and political influence derived from their position in the varna hierarchy. They uphold the orthodox reading as it legitimizes their advantages, though some may selectively observe prescriptions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_elites, beneficiary,
    powerful, generational, mobile, national).

% Are subjected to severe social exclusion, economic exploitation, and ritual impurity based on their birth. They are denied access to education, temples, and dignified labor, bearing the highest costs of the hierarchical system with virtually no exit options within the traditional framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, generational, trapped, local).

% Are assigned roles of service to upper castes and denied access to Vedic learning and certain rituals. While their situation is less severe than Dalits, they still face significant social and economic restrictions, with limited avenues for upward mobility or exit.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, generational, constrained, local).

% Across all castes, women are subjected to patriarchal norms, restricted autonomy, limited access to education and property, and prescribed roles primarily within the domestic sphere. Their identity and social standing are often defined by their relationship to male family members, making exit from these norms deeply challenging.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women, payer,
    powerless, generational, identity_locked, national).

% Are the primary custodians and enforcers of the Dharmasastra texts and their literalist interpretation. They propagate the ideology, organize rituals, and exert social pressure to maintain adherence to the caste and gender hierarchies, benefiting from the authority and stability this provides.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, orthodox_religious_institutions, agenda_setter,
    institutional, civilizational, constrained, national).

% Engage in critical textual analysis, arguing for contextual interpretations of Dharmasastra that de-emphasize or reject caste and gender discrimination. They face social and academic resistance from orthodox circles but operate within a broader intellectual sphere that allows for dissent.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_scholars, observer,
    moderate, biographical, mobile, global).

% Work to challenge and dismantle the caste system and gender discrimination through legal, political, and social activism. They operate outside the traditional religious framework and actively resist the constraint, often facing threats and violence.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, human_rights_advocates, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive framework for social organization, ritual practice, and ethical conduct, aiming to maintain cosmic order (dharma) and social stability through a divinely ordained hierarchy.
% TRANSFER_FUNCTION: Transfers social status, ritual purity, economic resources, and access to knowledge and power from lower castes and women to upper castes and men, in exchange for perceived social order and spiritual merit.
% ABSENT_VOICES: The voices of Dalits, Shudras, and women have historically been systematically excluded from the interpretive and administrative bodies of Dharmasastra. If present, they would unequivocally reject the hierarchical prescriptions and demand equality and justice, exposing the extractive nature of the system.
% DISAPPEARANCE_RATIONALE: If the orthodox-literalist interpretation of Dharmasastra and its enforcement vanished overnight, the social fabric of many traditional communities would undergo profound rearrangement. The caste system would lose its primary religious legitimacy, leading to widespread challenges to existing power structures, demands for equality, and a redefinition of social roles and access to resources. While some social inertia might remain, the foundational justification for hierarchy would be gone, leading to significant societal shifts.
% FOUNDING_PROBLEM: The Dharmasastra corpus was compiled to establish and maintain social order, ritual purity, and ethical conduct in ancient Indian society, providing a comprehensive legal and moral code for various social groups.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious institutions and some upper-caste communities attest that the founding problem of maintaining social and cosmic order through hierarchy is still live and essential. However, reformist scholars, human rights advocates, and victim communities (Dalits, women's rights groups) strongly contest this, arguing that the original problem has either been superseded by modern ethical standards or was always a justification for an extractive system, and that the constraint now primarily serves to perpetuate inequality. Historical evidence of social mobility and regional variations in caste practice also challenge the 'eternal' nature of the problem.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.9) is high due to the systemic transfer of resources, status, and opportunities from lower castes and women to upper castes. Suppression (0.85) is also high, maintained through social coercion, religious authority, and historical legal backing, which limits exit options and punishes non-compliance. The theater ratio (0.1) is low, indicating that the constraint's primary function is indeed to maintain this hierarchical extraction, with little performative 'cover' beyond the claim of divine mandate. Accessibility collapse is high (0.7) as alternatives to the prescribed social order are severely limited within the traditional framework, and resistance (0.6) is substantial, reflecting ongoing challenges from victim groups and reform movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the brahmin priestly class and upper-caste elites, this constraint is a divinely ordained, beneficial social order (perceived as a Mountain or Rope). For Dalits, Shudras, and women, it is a deeply oppressive and extractive Snare. The engine's classification will highlight this divergence by computing a Snare classification for the victim seats, contrasting sharply with the claimed 'eternal truth' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The brahmin priestly class and upper-caste elites are clear beneficiaries, deriving social, economic, and ritual advantages (low directionality). Dalits, Shudras, and women are direct targets, facing systemic discrimination, exclusion, and exploitation (high directionality, often identity_locked). Orthodox religious institutions act as agenda setters, enforcing the constraint and benefiting from its stability. Reformist scholars and human rights advocates are analytical observers or active resistors, experiencing the constraint as a target of its suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining a divinely ordained social order) is presented as eternal, thus precluding mandatrophy in its own terms. However, from an external analytical perspective, the 'founding problem' of maintaining social cohesion through hierarchy is contested. The persistence of the constraint, despite its severe extractive and suppressive effects, is maintained by the concentrated benefits to upper castes and the active enforcement by religious institutions, rather than a genuine, universally accepted coordination function. The high extractiveness and suppression, coupled with the contested founding problem status, prevent mislabeling this as a benign coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of eternal truth, or a specific interpretation of the Dharmasastra corpus that benefits identifiable groups?',
    'Comparative textual analysis across diverse historical commentaries, sociological study of power dynamics in interpretive communities, and examination of alternative readings'' historical suppression.',
    'If a specific interpretation, the constraint''s claimed ''mountain'' status collapses, revealing its constructed, extractive nature. This would shift its classification from a perceived natural law to a snare or tangled rope, depending on the degree of coordination vs. pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''orthodox_literalist'' reading of the ''dharmasastra_corpus'' kernel. Sibling readings (reformist_contextual, abolitionist_rejection) offer alternative interpretations that would significantly alter the beneficiary/victim structure and extractiveness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social exclusion, legal barriers) or internalized (belief in one''s ''dharma'' or karma, acceptance of hierarchy)?',
    'Post-exit suppression trajectory: if suppression persists after external barriers are removed (e.g., individuals from victim groups internalize caste norms even in new social contexts), reclassify as partially internalized. Sociological studies of identity formation and self-perception among victim groups.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making escape from its effects more difficult. This would amplify the effective extractiveness (χ) for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of caste and gender hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dhar_tr_t100, dharmasastra_corpus__orthodox_literalist, theater_ratio, 100, 0.12).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(dhar_be_t100, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 100, 0.85).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dhar_su_t100, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 100, 0.8).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Dharmasastra corpus kernel. Its structural properties (high extraction, concentrated beneficiaries, expansive victim set) are distinct from sibling readings, which offer alternative interpretations of the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
