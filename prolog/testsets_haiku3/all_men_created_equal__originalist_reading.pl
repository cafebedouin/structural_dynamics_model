% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Declaration of Independence Equality Clause (Originalist Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of 'all men are created equal' interprets the
 *   phrase within the 18th-century social taxonomy that governed the founding
 *   generation. Under this reading, 'all men' refers to all free men of
 *   property and standing within the colonies—not to women, enslaved peoples,
 *   indigenous peoples, or servants. The founders' intent governs the scope;
 *   historical reconstruction of that intent anchors the reading's
 *   legitimacy. This constraint story instantiates ONLY the originalist
 *   reading: the standing arrangement under contest is the narrow application
 *   of the equality guarantee bounded by the founders' explicitly held social
 *   categories. The referent is fixed: what the constraint is about is the
 *   arrangement the originalists defend. ε measures how much that narrow
 *   reading extracts from those excluded, assessed by the originalist
 *   reading's own structural logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.82).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.79).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Declaration of Independence Equality Clause (Originalist Reading)").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, '54822785-14c7-4918-88a6-760a1e6228a4').
narrative_ontology:cs_kernel_codification('54822785-14c7-4918-88a6-760a1e6228a4', fixed_text).
narrative_ontology:cs_authority_grounding('54822785-14c7-4918-88a6-760a1e6228a4', extraction).
narrative_ontology:cs_interpretation_layer_present('54822785-14c7-4918-88a6-760a1e6228a4').
narrative_ontology:cs_reading_relation('54822785-14c7-4918-88a6-760a1e6228a4', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('54822785-14c7-4918-88a6-760a1e6228a4', all_men_created_equal__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('54822785-14c7-4918-88a6-760a1e6228a4', foundational, founders_intent_is_binding).
narrative_ontology:cs_axiom_status(founders_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('54822785-14c7-4918-88a6-760a1e6228a4', founders_intent_is_binding, deontological).
narrative_ontology:cs_axiom('54822785-14c7-4918-88a6-760a1e6228a4', foundational, eighteenth_century_social_taxonomy_determines_scope).
narrative_ontology:cs_axiom_status(eighteenth_century_social_taxonomy_determines_scope, overridden).
narrative_ontology:cs_axiom_grounding('54822785-14c7-4918-88a6-760a1e6228a4', eighteenth_century_social_taxonomy_determines_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('54822785-14c7-4918-88a6-760a1e6228a4', founding_compact_among_free_men).
narrative_ontology:cs_drift_state('54822785-14c7-4918-88a6-760a1e6228a4', post_abolitionist_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('54822785-14c7-4918-88a6-760a1e6228a4', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_property_owners).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, white_male_citizens_of_standing).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, enslaved_africans).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, women).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indigenous_peoples).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, indentured_servants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_legal_scholars).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, excluded_future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Composed of the signatories and their political coalition—merchants, slaveholders, plantation owners, and creditor classes. They author the founding documents with 'all men' language while maintaining explicit, legal slavery and property qualification for political participation. They enforce the narrow reading through legal doctrine, judicial interpretation, and the enforcement of slave codes and property restrictions. They benefit from the rhetorical universalism (legitimacy) while maintaining extractive exclusion (material control).
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_property_owners, agenda_setter,
    institutional, generational, analytical, national).

% Inherit the benefit of the narrow reading: political voice, legal personhood, property rights, and freedom of movement. Access to the 'equality' guarantee while the exclusion machinery (slave codes, coverture, Indian removal) remains active and normalized. Their participation in the political system ratifies and reproduces the narrow scope.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, white_male_citizens_of_standing, beneficiary,
    powerful, generational, analytical, national).

% Explicitly excluded from the scope of 'all men' through legal doctrine and enforcement. The originalist reading grounds their exclusion in the founder's intent: they were not parties to the founding compact, not considered 'men' under the 18th-century social taxonomy, and therefore never possessed the guarantee. Trapped by law, violence, and the inherited institution of chattel slavery. Exit is impossible; resistance is criminalized.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, enslaved_africans, payer,
    powerless, civilizational, trapped, national).

% Excluded under the originalist reading through the legal doctrine of coverture and the interpretation that 'men' refers literally to males. The founding taxonomy placed women under male guardianship (father, husband, or state). No direct political voice, no independent property rights. The 'equality' guarantee is narrowly scoped to exclude them by definition—a reading defended as fidelity to the founders' intent.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, women, payer,
    moderate, generational, constrained, national).

% Excluded from the founding compact and from the scope of 'all men' through the originalist reading. Treated as external sovereignties (initially) and later as wards of the federal government. The founding documents do not grant them rights as 'men' because the social taxonomy that generated the equality clause did not recognize them as participants in the founding order. Systematic removal, legal subordination, and exclusion from the political franchise follow from the narrow reading.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indigenous_peoples, payer,
    organized, civilizational, trapped, national).

% Excluded through status: the 18th-century social taxonomy placed them beneath the class of 'free men of the colonies.' The originalist reading treats their servitude as compatible with the founding equality guarantee because the founders themselves maintained and relied on indentured labor. Their exclusion is naturalized as a matter of status at the moment of founding.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, indentured_servants, payer,
    powerless, immediate, trapped, local).

% Professionally invested in defending the originalist reading through close textual and historical analysis. Their intellectual authority and career advancement depend on establishing that the narrow reading faithfully represents the founder's intent. They benefit from the interpretive framework even though they may not benefit materially from the exclusion.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_legal_scholars, beneficiary,
    institutional, biographical, constrained, national).

% Courts that enforce the originalist reading through doctrine: they certify which groups fall within the scope of 'all men' and which are constitutionally outside. They have the power to widen or narrow the reading but typically defer to the originalist framework when it is politically dominant. They ratify exclusion through legal pronouncement.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, judicial_gatekeepers, agenda_setter,
    institutional, generational, constrained, national).

% Those born into categories the originalist reading excludes—descendants of enslaved peoples, women, indigenous nations—inherit the legal subordination as a structural feature. Exit requires either generational change in the reading itself (external intervention) or transformation of identity classification (which the constraint actively prevents). Suppression is partially internalized through legal doctrine and cultural legitimacy narratives.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, excluded_future_generations, payer,
    powerless, civilizational, identity_locked, national).

% Those who advocate for expanding the scope of 'all men' to include all humans regardless of race, gender, or status. They are structurally excluded from the originalist framework's decision-making apparatus because that framework treats the founders' taxonomy as fixed and authoritative. Their reform effort requires either amending the Constitution or displacing the originalist interpretive authority—both are costly and face institutional resistance.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_reformers, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, founding_property_owners).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a founding compact among free property-holding men: defines political membership, legitimate authority, and mutual obligation for those within the scope of 'all men.' Creates a coherent in-group with shared rights and duties.
% TRANSFER_FUNCTION: Concentrates political voice, legal personhood, property rights, and freedom of movement to founding property owners and their descendants (white males of standing); extracts labor, legal subordination, and political voicelessness from enslaved peoples, women, indigenous peoples, and servants through the narrow reading of 'all men.' The transfer is naturalized as the constitutive boundary of political membership itself.
% ABSENT_VOICES: All those legally excluded from the scope—enslaved Africans, women, indigenous peoples, indentured servants—are structurally prevented from contesting the reading. They are not parties to the founding compact in originalist doctrine and therefore cannot challenge the scope from inside the framework. Universalist reformers and abolitionist advocates remain marginal in the originalist canon; their objections are treated as external critique, not as claims the founders contemplated.
% DISAPPEARANCE_RATIONALE: If the originalist reading and its enforcement mechanism vanished overnight, the narrow scope would collapse; rights and political participation would immediately expand to include excluded groups, or the political order would fragment as the legitimacy provided by the equality clause narrative was lost. The founding compact as the originalists defend it depends entirely on the narrow reading.
% FOUNDING_PROBLEM: Establish political legitimacy and mutually binding authority among free property holders in the colonies after independence; distinguish the free population from those held in bondage or subordinate status; create a universal principle of membership to unite disparate colonies under one government.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal commentators attest the founding problem remains live: they argue the founders faced the genuine task of constituting a new political order and used universal language to unite free men. Historians outside the originalist tradition (Saidiya Hartman, Gordon Wood, Jennifer Homans) and abolition-era contemporaries attest the founding problem was SOLVED by creating a narrow scope, and the continued invocation of the problem is retrospective justification for maintaining that narrow scope. The contemporaneous record (slave codes, property qualifications, the three-fifths clause) corroborates the narrow reading was intentional, not accidental.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 terminal) and rising through the interval because the originalist reading must actively defend the narrow scope against universalist challenge—extraction increases as the reading's contradictions become more visible to those excluded. Suppression is high (0.79) because maintaining the narrow reading requires legal doctrine, enforcement of slave codes and coverture, exclusion from the franchise, and delegitimization of reform claims. Theater is elevated (0.68) because by the 1850s the reading increasingly performs legitimacy rather than describing a natural social order—the contradiction between 'all men' and the slave plantation becomes inescapable, yet the originalist framework doubles down on the narrow reading as authentic to the founders. Theater peaks near 1861 (Civil War) when the performance becomes most strained, then declines as the reading begins to lose institutional authority. The measurement series capture the constraint's increasing internal strain as universalist pressure mounts and the narrow reading must work harder to maintain itself.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seat perceive profoundly different constraint types. From the originalist agenda-setters' perspective, the reading is fidelity to the founding document and the founders' intent—a principle-guided coordination that happens to exclude those not intended to be parties. From the enslaved, the women, and the indigenous peoples' perspective, it is pure extraction justified post-hoc by an appeal to historical intent they were never allowed to contest. The engine computes these divergent classifications from the structural data: high d for the targets yields high χ; low d for the beneficiaries yields negative χ (subsidy). The originalist scholar benefits from the reading's institutional authority without directly collecting from enforcement; they sit at moderate d but high d-independent prestige gain. The measurement series capture the constraint's increasing instability: as resistance mounts, suppression must increase to sustain the reading, which increases the theater required to maintain legitimacy, which accelerates erosion of the reading's authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding property owners are the structural beneficiaries—they author the reading, control its interpretation, and accumulate the benefits of both the universal legitimacy narrative and the narrow, exclusionary scope. They face d near 0.0 (full beneficiary). The four victim groups (enslaved Africans, women, indigenous peoples, indentured servants) are the targets facing the full extraction: no political voice, no legal personhood where promised, trapped in subordinate status justified by the narrow reading. They face d near 1.0 (full target). White male citizens of standing are near-complete beneficiaries (d near 0.1) except for those with no property (who remain excluded by status, not just by reading). Universalist reformers are neither clearly benefited nor extracted from—they face exclusion from the interpretive apparatus but are not the direct targets of enforcement; they sit near d=0.5 (symmetric frustration). The measurement series show suppression rising as resistance grows: the excluded populations increasingly contest the reading, and suppression requirement escalates to maintain it.
 *
 * MANDATROPHY ANALYSIS:
 *   The originalist reading faces incipient mandatrophy by 1868. The founding problem it was built to solve—establish a political compact among free property holders—is achieved by 1776. The continued invocation of founders' intent by 1850 is not solving the founding problem; it is defending the narrow scope against universalist expansion. The reading persists because those who benefit from it (founding descendants, legal scholars, judicial gatekeepers) can maintain its authority through doctrine and enforcement, NOT because the founding problem remains alive. By 1868 (the Fourteenth Amendment), the narrow reading has been formally superseded—a new founding problem emerges (ensure equal protection of the laws regardless of race), and the originalist reading shifts from coordination principle to impediment to reform. The constraint does not disappear; rather, its function shifts from constitutive (founding the political order) to obstructive (preventing the order's expansion). Theater ratio rises because performance of legitimacy intensifies as the reading's functional justification evaporates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founders_intent_determination,
    'How is the founders'' intent reliably established? Was it genuinely unanimous on the narrow scope, or is the narrow reading a post-hoc legal construction imposed retrospectively?',
    'Close historical and textual examination of the founding generation''s writings, recorded debates, and contemporaneous legal practice. Compare against competing reconstructions of intent (universalist historians vs. originalist scholars). The resolution hinges on whether intent is determinate from the historical record or is itself contested.',
    'If intent was genuinely narrow and unanimous, the originalist reading is faithful to the founding and the narrow scope is structural. If intent was contested or has been reconstructed selectively by later originalists, the reading becomes a projection rather than a discovery, and ε should rise (more theatrical, less grounded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founders_intent_determination, empirical, 'Whether the narrow scope reflects founders'' intent or is a later originalist construction.').

omega_variable(
    suppression_internalization,
    'How much of the measured suppression (0.79) is structural (legal codes, franchise exclusion, economic dependency) versus internalized (the excluded believing the narrow reading is legitimate)?',
    'Post-constraint-collapse behavioral studies: if suppression persists after legal exclusion is removed, measure the internalized component. Historical records of resistance (slave rebellions, women''s suffrage movements, indigenous sovereignty assertions) indicate the suppression is structural—resistance persists despite legal barriers and legitimacy narratives.',
    'If largely structural, the constraint is highly extractive and should remain classified as snare. If substantially internalized, the constraint''s effective suppression carries over after legal enforcement ends, indicating deeper institutional entrenchment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Ratio of structural to internalized suppression in the narrow reading''s persistence.').

omega_variable(
    theater_function_ambiguity,
    'Is the rising theater ratio (0.68 by 1868) performing legitimacy for an increasingly indefensible reading, or is it performing coordination cost for maintaining political stability?',
    'Examine the content of the performance: speeches, judicial opinions, political rhetoric between 1820–1868. If the performance defends the narrow reading despite mounting contradiction, it is theater masking extraction. If the performance genuinely attempts to mediate between the universal language and narrow application (seeking compromise positions, marginal expansions), it is coordination theater. The 1850s reveal elevated performance of rigid narrow readings (defenses of slavery on originalist grounds) rather than attempted synthesis, indicating extraction theater.',
    'If extraction theater, the reading''s legitimacy is increasingly performative and fragile; institutional collapse when performance fails (Civil War era). If coordination theater, the reading persists as a stable compromise despite internal tension. The measurement series suggests extraction theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_function_ambiguity, conceptual, 'Whether rising theater ratio indicates failed legitimacy performance or coordination mediation cost.').

omega_variable(
    naturalized_taxonomy_contingency,
    'Is the 18th-century social taxonomy that bounds ''all men'' a natural, inevitable category set, or a historically contingent political choice that could have been framed differently?',
    'Compare against the Universal Declaration of Human Rights, other founding documents (French Revolution, Caribbean independence), and alternative framings available to the founding generation but not chosen. If alternative framings existed and were rejected, the narrow taxonomy is contingent; if the founders genuinely could not conceive of any other taxonomy, it is less contingent (but still authored, not natural).',
    'If contingent, the originalist reading is a specific historical choice defended post-hoc as faithful to intent, not a neutral discovery of intent. ε should rise (more constructed, less inevitable). If genuinely constrained by the founders'' worldview, the narrow reading is faithful to a real historical limitation—not less extractive but more excused by context. The measurement series treat it as contingent (rising theater suggests increasing awareness of its contingency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalized_taxonomy_contingency, conceptual, 'Whether the narrow social taxonomy is natural or contingent to the founding moment.').

omega_variable(
    kernel_reading_identity,
    'What distinguishes the originalist_reading from its sibling readings (textualist_paradox, universalist)? Is the distinction rooted in different axioms (what normative claims ground each reading) or in different data interpretations (what counts as evidence of founders'' intent)?',
    'Examine the root-level disagreement: originalists and textualists both appeal to the text but differ on what it means (universalist language vs. restricted application); originalists and universalists differ on whether intent is binding (it is for originalists, it should be overridden by principle for universalists). The distinction routes to axioms: originalism grounds legitimacy in fidelity-to-intent (deontological axiom); universalism grounds legitimacy in expanding rights (instrumental axiom about human dignity).',
    'Clarifies the kernel contest: it is not about facts (what the founders intended) but about normative authority (whether intent should bind). This confirms the originalist reading as a specific normative choice, not a neutral discovery, which supports the high ε measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Root distinction between originalist and sibling readings: axiom difference vs. empirical disagreement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.52).
narrative_ontology:measurement_basis(all__tr_t1776, observed).
narrative_ontology:measurement(all__tr_t1793, all_men_created_equal__originalist_reading, theater_ratio, 1793, 0.58).
narrative_ontology:measurement_basis(all__tr_t1793, observed).
narrative_ontology:measurement(all__tr_t1820, all_men_created_equal__originalist_reading, theater_ratio, 1820, 0.64).
narrative_ontology:measurement_basis(all__tr_t1820, observed).
narrative_ontology:measurement(all__tr_t1850, all_men_created_equal__originalist_reading, theater_ratio, 1850, 0.71).
narrative_ontology:measurement_basis(all__tr_t1850, observed).
narrative_ontology:measurement(all__tr_t1861, all_men_created_equal__originalist_reading, theater_ratio, 1861, 0.72).
narrative_ontology:measurement_basis(all__tr_t1861, observed).
narrative_ontology:measurement(all__tr_t1868, all_men_created_equal__originalist_reading, theater_ratio, 1868, 0.68).
narrative_ontology:measurement_basis(all__tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.78).
narrative_ontology:measurement_basis(all__be_t1776, observed).
narrative_ontology:measurement(all__be_t1793, all_men_created_equal__originalist_reading, base_extractiveness, 1793, 0.81).
narrative_ontology:measurement_basis(all__be_t1793, observed).
narrative_ontology:measurement(all__be_t1820, all_men_created_equal__originalist_reading, base_extractiveness, 1820, 0.84).
narrative_ontology:measurement_basis(all__be_t1820, observed).
narrative_ontology:measurement(all__be_t1850, all_men_created_equal__originalist_reading, base_extractiveness, 1850, 0.86).
narrative_ontology:measurement_basis(all__be_t1850, observed).
narrative_ontology:measurement(all__be_t1861, all_men_created_equal__originalist_reading, base_extractiveness, 1861, 0.85).
narrative_ontology:measurement_basis(all__be_t1861, observed).
narrative_ontology:measurement(all__be_t1868, all_men_created_equal__originalist_reading, base_extractiveness, 1868, 0.82).
narrative_ontology:measurement_basis(all__be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.71).
narrative_ontology:measurement_basis(all__su_t1776, observed).
narrative_ontology:measurement(all__su_t1793, all_men_created_equal__originalist_reading, suppression_requirement, 1793, 0.75).
narrative_ontology:measurement_basis(all__su_t1793, observed).
narrative_ontology:measurement(all__su_t1820, all_men_created_equal__originalist_reading, suppression_requirement, 1820, 0.8).
narrative_ontology:measurement_basis(all__su_t1820, observed).
narrative_ontology:measurement(all__su_t1850, all_men_created_equal__originalist_reading, suppression_requirement, 1850, 0.83).
narrative_ontology:measurement_basis(all__su_t1850, observed).
narrative_ontology:measurement(all__su_t1861, all_men_created_equal__originalist_reading, suppression_requirement, 1861, 0.81).
narrative_ontology:measurement_basis(all__su_t1861, observed).
narrative_ontology:measurement(all__su_t1868, all_men_created_equal__originalist_reading, suppression_requirement, 1868, 0.79).
narrative_ontology:measurement_basis(all__su_t1868, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1776, tn=1868
narrative_ontology:measurement(all__grid_01, all_men_created_equal__originalist_reading, accessibility_collapse(class), 1776, 0.71).
narrative_ontology:measurement(all__grid_02, all_men_created_equal__originalist_reading, accessibility_collapse(class), 1868, 0.73).
narrative_ontology:measurement(all__grid_03, all_men_created_equal__originalist_reading, accessibility_collapse(individual), 1776, 0.68).
narrative_ontology:measurement(all__grid_04, all_men_created_equal__originalist_reading, accessibility_collapse(individual), 1868, 0.71).
narrative_ontology:measurement(all__grid_05, all_men_created_equal__originalist_reading, accessibility_collapse(organizational), 1776, 0.74).
narrative_ontology:measurement(all__grid_06, all_men_created_equal__originalist_reading, accessibility_collapse(organizational), 1868, 0.76).
narrative_ontology:measurement(all__grid_07, all_men_created_equal__originalist_reading, accessibility_collapse(structural), 1776, 0.78).
narrative_ontology:measurement(all__grid_08, all_men_created_equal__originalist_reading, accessibility_collapse(structural), 1868, 0.8).
narrative_ontology:measurement(all__grid_09, all_men_created_equal__originalist_reading, resistance(class), 1776, 0.25).
narrative_ontology:measurement(all__grid_10, all_men_created_equal__originalist_reading, resistance(class), 1868, 0.64).
narrative_ontology:measurement(all__grid_11, all_men_created_equal__originalist_reading, resistance(individual), 1776, 0.22).
narrative_ontology:measurement(all__grid_12, all_men_created_equal__originalist_reading, resistance(individual), 1868, 0.58).
narrative_ontology:measurement(all__grid_13, all_men_created_equal__originalist_reading, resistance(organizational), 1776, 0.18).
narrative_ontology:measurement(all__grid_14, all_men_created_equal__originalist_reading, resistance(organizational), 1868, 0.71).
narrative_ontology:measurement(all__grid_15, all_men_created_equal__originalist_reading, resistance(structural), 1776, 0.12).
narrative_ontology:measurement(all__grid_16, all_men_created_equal__originalist_reading, resistance(structural), 1868, 0.52).
narrative_ontology:measurement(all__grid_17, all_men_created_equal__originalist_reading, stakes_inflation(class), 1776, 0.75).
narrative_ontology:measurement(all__grid_18, all_men_created_equal__originalist_reading, stakes_inflation(class), 1868, 0.82).
narrative_ontology:measurement(all__grid_19, all_men_created_equal__originalist_reading, stakes_inflation(individual), 1776, 0.72).
narrative_ontology:measurement(all__grid_20, all_men_created_equal__originalist_reading, stakes_inflation(individual), 1868, 0.76).
narrative_ontology:measurement(all__grid_21, all_men_created_equal__originalist_reading, stakes_inflation(organizational), 1776, 0.68).
narrative_ontology:measurement(all__grid_22, all_men_created_equal__originalist_reading, stakes_inflation(organizational), 1868, 0.74).
narrative_ontology:measurement(all__grid_23, all_men_created_equal__originalist_reading, stakes_inflation(structural), 1776, 0.79).
narrative_ontology:measurement(all__grid_24, all_men_created_equal__originalist_reading, stakes_inflation(structural), 1868, 0.85).
narrative_ontology:measurement(all__grid_25, all_men_created_equal__originalist_reading, suppression(class), 1776, 0.71).
narrative_ontology:measurement(all__grid_26, all_men_created_equal__originalist_reading, suppression(class), 1868, 0.8).
narrative_ontology:measurement(all__grid_27, all_men_created_equal__originalist_reading, suppression(individual), 1776, 0.73).
narrative_ontology:measurement(all__grid_28, all_men_created_equal__originalist_reading, suppression(individual), 1868, 0.81).
narrative_ontology:measurement(all__grid_29, all_men_created_equal__originalist_reading, suppression(organizational), 1776, 0.69).
narrative_ontology:measurement(all__grid_30, all_men_created_equal__originalist_reading, suppression(organizational), 1868, 0.78).
narrative_ontology:measurement(all__grid_31, all_men_created_equal__originalist_reading, suppression(structural), 1776, 0.71).
narrative_ontology:measurement(all__grid_32, all_men_created_equal__originalist_reading, suppression(structural), 1868, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__originalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__textualist_paradox_reading).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, all_men_created_equal__universalist_reading).

% DUAL FORMULATION NOTE:
% The 'all_men_created_equal' kernel decomposes into three structurally distinct constraint stories: originalist_reading (narrow scope bounded by 18th-century taxonomy; high extraction; this story), textualist_paradox_reading (universal language irreconcilable with restricted application; exposes performative contradiction), and universalist_reading (equality as principle requiring iterative expansion; low extraction if adopted). Each reading instantiates a different ε, different victim set, different type. They are linked by shared kernel (the Declaration equality clause) and competing authority claims (intent vs. text vs. principle). The originalist reading influences both siblings by setting the doctrinal status quo; the textualist reading exposes the originalist reading's logical weakness; the universalist reading offers an alternative authority structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, powerless, 0.98).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, moderate, 0.85).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, organized, 0.91).
constraint_indexing:directionality_override(all_men_created_equal__originalist_reading, institutional, 0.04).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
