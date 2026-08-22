% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus as Oppressive Hierarchy (Abolitionist Reading)
 *   domain: religious_law/normative_authority/social_hierarchy
 *
 * SUMMARY:
 *   The abolitionist reading of the Dharmasastra corpus rejects the entire
 *   framework as fundamentally designed for extraction and oppression. Unlike
 *   reformist readings that seek to preserve an ethical core while abandoning
 *   caste prescriptions, or orthodox readings that treat the texts as
 *   eternally binding, this reading treats the texts themselves as mechanisms
 *   for encoding and perpetuating brahminical dominance. The hierarchy is not
 *   reinterpretable; it must be abandoned entirely. No legitimate textual
 *   authority remains under this reading. The victims (Dalit groups, Shudras,
 *   women, untouchables) are not subjects of a hierarchy to be reformed, but
 *   rather populations whose freedom requires the entire framework's
 *   destruction. This is one reading of the contested Dharmasastra kernel; it
 *   coexists with but forecloses certain elements of reformist and orthodox
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.91).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.87).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.91).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus as Oppressive Hierarchy (Abolitionist Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/normative_authority/social_hierarchy").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '8f59269f-4020-4429-95fe-7dbf37865259').
narrative_ontology:cs_kernel_codification('8f59269f-4020-4429-95fe-7dbf37865259', fixed_text).
narrative_ontology:cs_authority_grounding('8f59269f-4020-4429-95fe-7dbf37865259', extraction).
narrative_ontology:cs_interpretation_layer_present('8f59269f-4020-4429-95fe-7dbf37865259').
narrative_ontology:cs_reading_relation('8f59269f-4020-4429-95fe-7dbf37865259', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('8f59269f-4020-4429-95fe-7dbf37865259', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_axiom('8f59269f-4020-4429-95fe-7dbf37865259', foundational, dharmasastra_fundamentally_extractive).
narrative_ontology:cs_axiom_status(dharmasastra_fundamentally_extractive, holdable).
narrative_ontology:cs_axiom_grounding('8f59269f-4020-4429-95fe-7dbf37865259', dharmasastra_fundamentally_extractive, deontological).
narrative_ontology:cs_axiom('8f59269f-4020-4429-95fe-7dbf37865259', foundational, textual_authority_mechanism_of_oppression).
narrative_ontology:cs_axiom_status(textual_authority_mechanism_of_oppression, holdable).
narrative_ontology:cs_axiom_grounding('8f59269f-4020-4429-95fe-7dbf37865259', textual_authority_mechanism_of_oppression, empirically_contingent).
narrative_ontology:cs_reference_frame('8f59269f-4020-4429-95fe-7dbf37865259', brahminical_textual_supremacy).
narrative_ontology:cs_drift_state('8f59269f-4020-4429-95fe-7dbf37865259', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8f59269f-4020-4429-95fe-7dbf37865259', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahminical_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, upper_varnas).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, dalit_and_marginalized_groups).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, shudra_populations).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_across_varnas).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, untouchables_and_ritual_polluted).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, women_across_varnas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains textual authority over Dharmasastra interpretation and enforcement. Controls ritual knowledge, legitimacy claims, and educational gatekeeping. Their authority rests on preserving the hierarchy the texts encode. Exit from this role would collapse their institutional position and require abandoning claims to ritual superiority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, brahminical_priesthood, agenda_setter,
    institutional, civilizational, trapped, continental).

% Receive structural privileges: control of land, ritual authority, educational monopoly, marriage endogamy privileges, exemption from manual labor. The Dharmasastra framework legitimizes these as cosmically ordained rather than socially constructed. Their social identity is fused with varna status; exit would require rejecting generational claims to superiority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, upper_varnas, beneficiary,
    powerful, civilizational, identity_locked, continental).

% Bear enforced ritual pollution status, occupational restriction, exclusion from temples and sacred texts, denial of educational access. The framework assigns them degrading labor and denies them path to respectability. Enforcement operates through economic dependence (occupational restriction), ritual prohibition (temple exclusion, water segregation), and internalized inferiority narratives. Exit requires dismantling the entire hierarchy.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, dalit_and_marginalized_groups, payer,
    powerless, civilizational, trapped, continental).

% Restricted to service occupations, denied Vedic education, excluded from ritual leadership. Dharmasastra prescribes permanent subordination justified as natural. They experience lower suppression than Dalit groups but face substantial structural extraction through occupational restriction and denial of educational paths.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, shudra_populations, payer,
    powerless, civilizational, constrained, continental).

% Dharmasastra prescribes lifelong guardianship: father in youth, husband in marriage, son in widowhood. Women are denied property ownership, independent religious authority, and choice of marriage partner. Upper-varna women receive some educational and property protections relative to lower-varna women. All carry identity-lock through marriage structures and inheritance restrictions.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, women_across_varnas, payer,
    moderate, civilizational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__abolitionist_rejection, women_across_varnas, beneficiary).

% Occupy the extreme extraction pole: assigned occupations involving death, disposal, and bodily fluids. Dharmasastra assigns them pariah status justified as karmic consequence. They face the highest suppression (ritual prohibition, water segregation, temple exclusion, enforced social distance). The framework denies them any legitimate grievance by attributing their condition to past-life karma.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, untouchables_and_ritual_polluted, payer,
    powerless, civilizational, trapped, continental).

% Advocate contextual reinterpretation or selective abandonment of Dharmasastra prescriptions while preserving a Hindu spiritual core. The abolitionist reading forecloses their position: if the entire framework is fundamentally oppressive, reinterpretation is complicity rather than reform. They remain excluded from the abolitionist consensus because they seek to preserve some textual authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, reform_advocates_within_hindu_traditions, excluded,
    organized, generational, constrained, continental).

% Post-colonial Indian constitutional law and human rights frameworks are engaged in formal displacement of Dharmasastra authority. They adjudicate which textual claims remain legally valid. From the abolitionist position, this displacement is incomplete — the reading seeks intellectual/spiritual abolition, not merely legal supersession.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__abolitionist_rejection, modern_secular_authority_structures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__abolitionist_rejection, brahminical_priesthood).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__abolitionist_rejection, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. From the abolitionist reading, there is no legitimate coordination function. Dharmasastra appears to coordinate ritual purity maintenance and status hierarchy, but this reading treats that coordination as designed oppression, not as solving a collective problem. The 'coordination' is entirely a justification narrative for extraction.
% TRANSFER_FUNCTION: Moves status, authority, ritual privilege, educational access, occupational choice, and human dignity FROM lower varna/caste groups TO upper varnas and the Brahminical priesthood. The mechanism: texts assign intrinsic inferiority based on varna/jati, enforce it through ritual prohibition and occupational restriction, and justify it as cosmic law rather than human choice.
% ABSENT_VOICES: Dalit and marginalized groups whose voices are actively silenced by the texts themselves (assigned as ritually impure, denied Vedic education, excluded from legitimate speech). The abolitionist reading centers these absent voices as evidence of the framework's oppressive design: a structure that excludes the voices of those it harms is self-evidently extractive.
% DISAPPEARANCE_RATIONALE: If Dharmasastra authority and the hierarchy it encodes disappeared, the entire social structure of caste would lose its textual legitimacy. Occupational restrictions would collapse, ritual prohibitions would dissolve, educational access would open, and millions would be freed from enforced subordination. The world would rearrange radically: hierarchy would persist but would be exposed as constructed rather than cosmically ordained.
% FOUNDING_PROBLEM: The reading contests whether a founding problem ever existed. Orthodox readings claim Dharmasastra solved the problem of social order and ritual purity preservation. The abolitionist reading rejects this framing: the 'founding problem' is a post-hoc justification for a hierarchy designed to serve brahminical interests. If there was a founding moment, it was the moment a dominant priestly class encoded their power into sacred text and claimed it as eternal truth.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical tradition and orthodox literalist readings attest that Dharmasastra solved the problem of maintaining cosmic order and social stability. Modern Dalit scholars, particularly B.R. Ambedkar and contemporary Dalit intellectual traditions, attest that the 'problem' Dharmasastra solved was consolidating brahminical power and subordinating exploited populations — not solving a pre-existing collective need. Outside corroboration comes from historical analysis showing the emergence of caste rigidity correlating with brahminical textual consolidation, and anthropological evidence that many Indian societies maintained social coordination without caste hierarchy.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__abolitionist_rejection, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__abolitionist_rejection, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__abolitionist_rejection, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__abolitionist_rejection, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__abolitionist_rejection, 0.91, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.91) because the Dharmasastra framework allocates all meaningful authority, status, education, occupational choice, and ritual agency to upper varnas, while systematically denying these to lower varnas and Dalit populations. This extraction is justified cosmically (as karmic consequence or natural-law hierarchy) rather than as choice, which is the abolitionist reading's core claim: the texts perform extraction by disguising it as eternal order. Suppression is high (0.87) because the framework operates through multiple mechanisms: (1) ritual prohibition (temple exclusion, water segregation, pollution taboos), (2) occupational restriction (enforced hereditary occupation), (3) denial of sacred text access (Shudras and Dalit groups barred from Vedic study), (4) denial of religious leadership, and (5) internalized inferiority through karma doctrine (suffering is attributed to past lives, making present oppression appear deserved). These mechanisms persist through institutional enforceability (brahminical priesthood, caste councils, kinship enforcement) and through deep cultural embedding. Theater ratio is substantial (0.62) because the texts wrap extraction in elaborate legitimating narratives: cosmic order (the Purusha Sukta myth of social emergence from divine sacrifice), karmic justification (current station as earned through past conduct), and dharma-as-duty (portraying oppression as righteous cosmic function). A significant share of the enforcement machinery defends this narrative rather than the material hierarchy itself: commentary, ritual, education in brahminical interpretive tradition. The measurement series show slight upward drift in extractiveness and suppression (0.82→0.91 and 0.81→0.87) and stable theater, reflecting the abolitionist reading's diagnosis that brahminical authority intensified through history as resistance mounted — hierarchy hardened rather than relaxed.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahminical priesthood seat, the Dharmasastra framework is a legitimate cosmic ordering mechanism they steward and refine; from Dalit groups' seats, it is the textual apparatus through which their oppression is encoded and justified. The engine computes these as structurally opposite d values (beneficiary vs. target) and opposite computed types (the priesthood might see the framework as rope — genuine coordination for social order; Dalit groups see it as snare — pure extraction). The abolitionist reading declares the priesthood's perspective a cover story: the 'coordination' is only apparent to those who benefit from the hierarchy. This is not perspectival relativism; it is a claim that one seat's reading mischaracterizes the structure to defend its interests.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahminical priesthood and upper varnas are pure targets of this reading's concern: they collect extraction (status, authority, occupational privilege) through the Dharmasastra framework and would lose institutional power if it were abandoned. Their exit is identity-locked (upper varnas) or trapped (priesthood): accepting the abolitionist reading would require rejecting their foundational claims to natural superiority. Dalit and marginalized groups bear maximum extraction (high d, near 1.0) under this reading: the framework explicitly assigns them oppressive status and provides no legitimate exit short of complete hierarchy dismantling. Shudras face substantial extraction (d~0.80) through occupational restriction and Vedic exclusion, though they retain a formal varna category. Women across varnas are dual-positioned (d~0.55): they receive some intra-varna benefits (upper-varna women have property protections and ritual roles relative to lower-varna women) but face extraction through guardianship and denial of independent authority. The abolitionist reading does not compute d for the priesthood or upper varnas as beneficiaries in the reformist sense (collecting rents within a system to be improved); it computes them as structural beneficiaries of oppression that must be eliminated.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading involves a foundational diagnosis of mandatrophy: Dharmasastra's founding mandate (organizing society, preserving cosmic order) has become indistinguishable from oppression. The reading argues that the 'mandate' was always, structurally, an oppressive mandate — the texts never solved a pre-existing coordination problem, but rather encoded the solution to a problem of brahminical power consolidation. Unlike a piton (where a real coordination function atrophied over time), the abolitionist reading treats the function itself as extractive from inception. This forecloses reformist reinterpretation: if the founding mandate was oppression, reinterpreting the texts to preserve 'genuine dharma' is complicity. The classification remains snare, not piton, because the beneficiaries (priesthood, upper varnas) actively maintain the hierarchy; it is not merely theatrical residue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_oppression_vs_corruption,
    'Is Dharmasastra fundamentally oppressive by design, or is it a framework designed for legitimate social coordination that has been corrupted or rigidified through brahminical misinterpretation?',
    'Historical reconstruction of textual evolution and institutional emergence: do the earliest Dharmasastra layers show equality assumptions later corrupted into hierarchy, or does hierarchy appear in the earliest layers as constitutive of the framework?',
    'If fundamentally designed for oppression: the abolitionist reading holds and the framework cannot be reformed. If corrupted: a reformist reading becomes viable — the ethical core could be salvaged by excising brahminical additions. This is the core pivot between abolitionist and reformist readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_oppression_vs_corruption, empirical, 'Whether the oppressive hierarchy is intrinsic to Dharmasastra or represents later accretion.').

omega_variable(
    alternative_social_coordination_paths,
    'Are there coherent examples of pre-caste or non-caste Hindu societies that maintained social order without Dharmasastra hierarchy, or is some form of social stratification inherent to large-scale Hindu societies?',
    'Anthropological and historical study of non-brahminical Hindu communities, regions that resisted caste hierarchy, and comparative analysis with other religious traditions'' social structures.',
    'Evidence of successful non-caste Hindu social organization would strengthen the abolitionist claim that hierarchy is not inevitable; evidence that stratification re-emerges without brahminical texts would support reformist arguments that some framework is necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_social_coordination_paths, empirical, 'Whether Dharmasastra is the sole pathway to Hindu social order or one contingent choice.').

omega_variable(
    suppression_structural_vs_internalized,
    'What fraction of the measured suppression (0.87) is structural (external barriers: ritual prohibition, occupational restriction, legal disability) versus internalized (beliefs in natural inferiority, acceptance of oppressive status as deserved)?',
    'Post-abolition trajectories: if populations exit the framework entirely and suppression persists, it is partly internalized; if it dissolves, suppression was primarily structural. Psychological and ethnographic study of communities that have rejected Dharmasastra authority.',
    'Higher structural suppression suggests the framework is externally enforceable and can be dismantled by authority shift; higher internalized suppression suggests the framework has become self-perpetuating and requires deep cultural work beyond textual rejection. Both support the abolitionist conclusion but with different implications for transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'The mechanism of suppression persistence in the Dharmasastra framework.').

omega_variable(
    kernel_framing_plurality,
    'Does the Dharmasastra kernel have only these three readings (orthodox, reformist, abolitionist), or do additional incommensurable readings exist that would generate structurally distinct constraints?',
    'Systematic review of Hindu intellectual traditions (Dalit philosophy, feminist theology, secular nationalism, devotional heterodoxy, Western Hindu modernism) to identify readings not subsumed in the declared three.',
    'Additional distinct readings would expand the constraint family and complicate the claim that one reading is foreclosed by another. The abolitionist reading might coexist with readings not yet named rather than foreclosing reformist alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_plurality, conceptual, 'Whether the Dharmasastra kernel reading-space is exhausted by the three declared readings or admits further incommensurable positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.58).
narrative_ontology:measurement_basis(dhar_tr_t0, observed).
narrative_ontology:measurement(dhar_tr_t5, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 5, 0.59).
narrative_ontology:measurement_basis(dhar_tr_t5, observed).
narrative_ontology:measurement(dhar_tr_t10, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 10, 0.61).
narrative_ontology:measurement_basis(dhar_tr_t10, observed).
narrative_ontology:measurement(dhar_tr_t15, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 15, 0.62).
narrative_ontology:measurement_basis(dhar_tr_t15, observed).
narrative_ontology:measurement(dhar_tr_t20, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 20, 0.62).
narrative_ontology:measurement_basis(dhar_tr_t20, observed).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 25, 0.62).
narrative_ontology:measurement_basis(dhar_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(dhar_be_t0, observed).
narrative_ontology:measurement(dhar_be_t5, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 5, 0.85).
narrative_ontology:measurement_basis(dhar_be_t5, observed).
narrative_ontology:measurement(dhar_be_t10, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 10, 0.88).
narrative_ontology:measurement_basis(dhar_be_t10, observed).
narrative_ontology:measurement(dhar_be_t15, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 15, 0.9).
narrative_ontology:measurement_basis(dhar_be_t15, observed).
narrative_ontology:measurement(dhar_be_t20, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 20, 0.91).
narrative_ontology:measurement_basis(dhar_be_t20, observed).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 25, 0.91).
narrative_ontology:measurement_basis(dhar_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0, 0.81).
narrative_ontology:measurement_basis(dhar_su_t0, observed).
narrative_ontology:measurement(dhar_su_t5, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 5, 0.83).
narrative_ontology:measurement_basis(dhar_su_t5, observed).
narrative_ontology:measurement(dhar_su_t10, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 10, 0.85).
narrative_ontology:measurement_basis(dhar_su_t10, observed).
narrative_ontology:measurement(dhar_su_t15, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 15, 0.87).
narrative_ontology:measurement_basis(dhar_su_t15, observed).
narrative_ontology:measurement(dhar_su_t20, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 20, 0.87).
narrative_ontology:measurement_basis(dhar_su_t20, observed).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(dhar_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__abolitionist_rejection, 0.05).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).

% DUAL FORMULATION NOTE:
% The Dharmasastra corpus kernel admits three structurally distinct readings: orthodox_literalist (hierarchy is eternally binding), reformist_contextual (ethical core separable from time-bound prescriptions), and abolitionist_rejection (entire framework is oppressive, zero textual authority). The ε-invariance principle requires one constraint per reading because the readings assess the same texts under different interpretive frames and produce incommensurable ε values: orthodox reads low extraction (the texts solve coordination), reformist reads moderate extraction (some prescriptions are historically contingent but ethical authority persists), abolitionist reads high extraction (the texts are the oppressive mechanism itself). Each reading generates a different beneficiary/victim structure and a different directionality profile. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, powerless, 0.95).
constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
