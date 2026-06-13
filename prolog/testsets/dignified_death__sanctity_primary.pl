% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Doctrine: Intentional Life-Termination Prohibition
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The sanctity-of-life doctrine asserts that human life possesses
 *   intrinsic, inviolable dignity grounded in transcendent moral law, making
 *   intentional life-termination impermissible regardless of suffering,
 *   consent, or context. This constraint reading instantiates ONLY the
 *   sanctity-doctrine perspective—the claim that life's value is intrinsic,
 *   not contingent on autonomy or quality. The kernel 'dignified death' is
 *   contested across three incompatible readings: this one
 *   (sanctity/intrinsic value), autonomy-primary (self-determination
 *   determines permissibility), and relational-autonomy (distributed decision
 *   authority with safeguards). Each reading has different victim structures,
 *   beneficiary claims, and ε values. They coexist in contemporary bioethics
 *   as rival institutional positions; this constraint models only the
 *   sanctity position's structural logic. The other readings are separate
 *   constraint stories in the family, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Sanctity-doctrine authority (religious traditions, natural-law philosophy): sets the framework that life is sacred; enforces it through institutional medicine
 *   - Institutional medical establishment: agenda-setter and beneficiary; operationalizes the doctrine; collects professional authority and liability immunity
 *   - Terminally ill with intractable suffering: primary victim; trapped with no exit; forced choice between prolongation or covert/illegal alternatives
 *   - Elderly with diminished autonomy: victim via identity-lock mechanism; cannot author their own exit even retrospectively
 *   - Disabled populations: victims experiencing asymmetric doctrine application; doctrine used discriminatorily to devalue their lives
 *   - Economically constrained patients: victims; cannot access palliative alternatives; trapped between institutional prolongation and nothing
 *   - Physicians with moral distress: forced to violate their suffering-reduction training; trapped between two authority systems
 *   - Families of the dying: indirect victims; experience prolonged grief without remedy; cannot authorize or assist even with explicit wishes
 *   - Autonomy-doctrine and relational-autonomy advocates: excluded from policy-setting in sanctity-dominant institutions; voices present but structurally absent from decision-making
 *   - Bioethics discipline (observer): documents the constraint's actual operation; produces evidence of unintended harms and covert alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.62).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.71).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Doctrine: Intentional Life-Termination Prohibition").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '4705209c-5232-4408-95f2-8bfcc1c5dc6c').
narrative_ontology:cs_kernel_codification('4705209c-5232-4408-95f2-8bfcc1c5dc6c', formalized).
narrative_ontology:cs_authority_grounding('4705209c-5232-4408-95f2-8bfcc1c5dc6c', extraction).
narrative_ontology:cs_interpretation_layer_present('4705209c-5232-4408-95f2-8bfcc1c5dc6c').
narrative_ontology:cs_reading_relation('4705209c-5232-4408-95f2-8bfcc1c5dc6c', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('4705209c-5232-4408-95f2-8bfcc1c5dc6c', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('4705209c-5232-4408-95f2-8bfcc1c5dc6c', foundational, intrinsic_sanctity_of_biological_life).
narrative_ontology:cs_axiom_status(intrinsic_sanctity_of_biological_life, holdable).
narrative_ontology:cs_axiom_grounding('4705209c-5232-4408-95f2-8bfcc1c5dc6c', intrinsic_sanctity_of_biological_life, theological).
narrative_ontology:cs_axiom('4705209c-5232-4408-95f2-8bfcc1c5dc6c', foundational, transcendent_moral_law_binds_all_parties).
narrative_ontology:cs_axiom_status(transcendent_moral_law_binds_all_parties, holdable).
narrative_ontology:cs_axiom_grounding('4705209c-5232-4408-95f2-8bfcc1c5dc6c', transcendent_moral_law_binds_all_parties, deontological).
narrative_ontology:cs_reference_frame('4705209c-5232-4408-95f2-8bfcc1c5dc6c', natural_law_intrinsic_dignity).
narrative_ontology:cs_drift_state('4705209c-5232-4408-95f2-8bfcc1c5dc6c', contemporary_medical_technological_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4705209c-5232-4408-95f2-8bfcc1c5dc6c', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_doctrine).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, institutional_medicine).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_with_intractable_suffering).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_with_diminished_autonomy).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_populations).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, economically_constrained_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, institutional_medical_establishment).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, physicians_conscience_conflicted).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, families_of_dying).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, physicians_conscience_conflicted).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, families_of_dying).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, intrinsic_sanctity_of_biological_life).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious and philosophical traditions (primarily Christian, Catholic institutional medicine, natural-law philosophy) set and defend the doctrine that human life possesses intrinsic, inviolable dignity grounded in transcendent value. These authorities define what constitutes a 'good death' (natural, accepted, not hastened) and enforce this through medical ethics codes, institutional policy, and in many jurisdictions, criminal law. Their authority rests on claims to moral truth that predate and supersede individual preference.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, sanctity_doctrine_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Hospitals, hospice systems, and medical professional organizations operationalize the sanctity doctrine through practice standards, do-not-resuscitate protocols, and refusals to participate in assisted death. They collect institutional legitimacy, professional authority, and immunity from liability for prolonging life. They administer the constraint by training clinicians in sanctity-based ethics and rejecting requests for life-hastening interventions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, institutional_medical_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, institutional_medical_establishment, agenda_setter).

% Face a final period of medical abandonment if they refuse pain management because pain management drugs would hasten death, or isolation if they request hastening and are denied. They cannot exit the jurisdiction easily; they have no countervailing institutional authority to appeal to. Their requests for control over their death are treated as evidence of depression or moral confusion, not autonomous choice. The constraint delivers prolonged suffering with no exit route.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_with_intractable_suffering, payer,
    powerless, immediate, trapped, local).

% May lack decision-making capacity (dementia, delirium) and thus cannot even articulate a preference to die; the sanctity doctrine means family members cannot authorize withdrawal of life support on behalf of someone who never expressed wishes. They experience the constraint as prolonged institutional existence divorced from their (inaccessible) wishes. Identity fusion here is profound: 'the person I was would never have chosen this, but I cannot choose otherwise now.' Exit requires a prior-self authorization they did not leave behind.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_with_diminished_autonomy, payer,
    moderate, immediate, identity_locked, local).

% Live under the shadow of the sanctity doctrine applied discriminatorily: clinicians and families are more likely to suggest withdrawal of support or decline aggressive treatment for disabled people than for non-disabled people with identical medical status—the constraint becomes a vehicle for devaluing disabled life. They experience the doctrine as a two-faced extraction: when they request assistance with death, they are told life is sacred; when they live, medical professionals unilaterally deem their life not worth living and make withdrawal suggestions. Exit is constrained by medical paternalism and by the constraint's asymmetric application to disability.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_populations, payer,
    organized, biographical, constrained, regional).

% Cannot afford palliative care or home hospice that would make prolonged life tolerable; they face a choice between institutional prolongation or nothing. The sanctity doctrine blocks the middle path (hastened death) and offers no economic remedy for the inadequacy of care infrastructure. Poverty converts the doctrine from 'preserve life' into 'preserve expensive institutional dependence.' They cannot exit because exit routes (jurisdiction, private care, physician-assisted death) are barred by cost or law.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, economically_constrained_patients, payer,
    powerless, immediate, trapped, national).

% Experience internal conflict: their medical training in reducing suffering collides with the sanctity doctrine's prohibition on hastening death. Those who accommodate patient requests face legal jeopardy and professional discipline. Those who refuse experience moral distress. They are caught between two authority structures (medicine's healing mandate and the doctrine's prohibitive mandate) with no institutional path to resolve the conflict. Exit means abandoning medicine or moving to a jurisdiction with different constraints.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, physicians_conscience_conflicted, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, physicians_conscience_conflicted, beneficiary).

% Experience prolonged grief, financial drain, and moral weight as they watch a loved one suffer without remedy. The sanctity doctrine forbids them from assisting or authorizing hastening even when the dying person's wishes are clear. Some experience the constraint as protective (it prevents their guilt); others experience it as imprisonment in someone else's dying. Their exit is constrained by the loved one's dependency and by law.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, families_of_dying, payer,
    moderate, immediate, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, families_of_dying, beneficiary).

% Argue that individual self-determination, not transcendent moral law, should govern end-of-life decisions. They are excluded from setting institutional policy in many jurisdictions; their voice is present in legislatures and medical ethics journals but structurally absent from hospital ethics committees dominated by sanctity-doctrine holders. Their exclusion is enforced by the doctrine's institutional embeddedness in medical licensing and credentialing.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_doctrine_advocates, excluded,
    powerful, generational, trapped, global).

% Propose a middle path: decision-making authority distributed across patient-family-clinician triad with procedural safeguards, rejecting both pure autonomy and absolute sanctity prohibition. They are excluded from primary policy-setting in sanctity-dominant jurisdictions; their approach finds expression in some legislative frameworks (Netherlands, Canada) but faces institutional resistance in sanctity-doctrine strongholds. Their exclusion is partial and jurisdictionally contingent.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, relational_autonomy_advocates, excluded,
    powerful, generational, trapped, global).

% Studies the constraint empirically: measuring patient and family distress trajectories, documenting unassisted suicides and mercy killings in constraint-bound jurisdictions, comparing outcomes across constraint systems. Produces evidence that the constraint fails at its own stated goal (protecting vulnerable populations) because it drives covert, unmonitored, dangerous alternatives. Takes no stake in the outcome but documents the constraint's actual operation.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, bioethics_academic_discipline, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, institutional_medical_establishment).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared moral boundary: 'human life is inviolable; intentional hastening of death is never permissible.' Solves the coordination problem of 'how do we ensure that all members of a society protect human life equally' by positing a transcendent rule that binds all parties regardless of individual preference. In principle, this prevents discrimination and ensures vulnerable populations cannot be eliminated against their will.
% TRANSFER_FUNCTION: Moves decision-making authority from the individual to the moral order (as mediated by institutional medicine and doctrine). Transfers suffering from the dying person to family members, physicians, and society—distributing the burden of 'preserving life' across all parties. Extracts labor, emotional burden, and economic cost from caregivers and patients who would prefer hastened death but cannot access it.
% ABSENT_VOICES: Patients with active suicidal ideation grounded in suffering (not depression) are structurally excluded: their expressed wishes are reframed as symptoms rather than choices. Disabled people whose death wishes arise from structural ableism are excluded: clinicians assume the disability, not the medical situation, motivates the request. Economically constrained patients are excluded from the conversation about feasible alternatives (better palliative care, income support) because the doctrine frames hastening as the only alternative to sanctity, not infrastructure as a third option. Physicians experiencing moral distress are excluded from decision-making authority—their discomfort is treated as a failure of conscience, not a legitimate perspective.
% DISAPPEARANCE_RATIONALE: If the sanctity-based prohibition vanished overnight, end-of-life decision-making would shift to individual/family/clinician frameworks in most Western jurisdictions (as seen in Netherlands, Belgium, Canada, Switzerland). Covert assisted deaths would likely decrease as institutional pathways opened. Palliative care infrastructure would face new demand but would no longer be positioned as the sole alternative to prolongation. The moral authority of religious/philosophical traditions would remain, but their capacity to impose uniform practice would dissolve. Institutional medicine would reorganize around patient-centered decision-making rather than doctrine-centered prohibition.
% FOUNDING_PROBLEM: Medieval and early-modern mortality rates meant most deaths were outside human control; 19th-century medicalization created the first widespread instances of prolonged, managed dying. The sanctity doctrine arose to prevent infanticide, euthanasia of the disabled, and aristocratic murder masquerading as mercy. In that context, an absolute prohibition served a protective function: preventing the elimination of people deemed socially undesirable by the powerful.
% FOUNDING_PROBLEM_CORROBORATION: Modern palliative medicine, legal safeguards, and democratic institutions have created alternative mechanisms to protect vulnerable populations. Academic bioethics, disability rights advocates, and comparative legal analysis document that jurisdictions with assisted-death frameworks show LOWER rates of coercive death than sanctity-doctrine jurisdictions (Netherlands' meticulous tracking vs. U.S. covert-death estimates). Clinicians practicing under both constraint regimes testify that the founding problem (preventing murder by the powerful) is addressed by procedural regulation, not by absolute prohibition. The doctrine's own institutional defenders (Catholic medical societies, evangelical hospitals) do not claim the founding problem still requires an absolute ban—they defend sanctity as intrinsic moral law, not as instrumental protection. This shift from instrumental to intrinsic justification indicates the founding problem has been displaced.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.62) and rising because the constraint's nominal protective function (preventing coercive death) has been displaced by its actual operation (enforcing prolonged suffering without remedy). The founding problem (19th-century prevention of aristocratic murder via euthanasia) is dead; the constraint persists as rent collection by institutional medicine and doctrine-authority. Suppression is high (0.71) and rising because enforcement requires active criminal-law penalties, institutional discipline, and medical gatekeeping—not passive adherence. Theater ratio is moderate-high (0.48) because institutional medicine invokes sanctity language while actually enforcing prolongation for institutional (liability, authority) and economic (ongoing care) reasons; the stated protective function masks the extractive operation. Accessibility collapse is moderate (0.68) because alternatives DO exist (clandestine assistance, jurisdiction migration, informal family help) but are dangerous, illegal, or inaccessible to powerless populations. Resistance is high (0.74) because suffering individuals, families, and physicians actively mount resistance: covert assistance, legislative movements toward liberalization, civil disobedience. The measurement series (1900–2026) show rising extractiveness and theater as palliative medicine advanced: the constraint's claimed function became technically obsolete (suffering is now often manageable) while enforcement intensified, exposing the extraction. Coercion grid shows structural-level accessibility collapse (doctrine's institutional embeddedness) declining as democratic/legal alternatives emerged, but individual-level stakes inflation rising sharply as medical technology prolonged dying and made end-of-life decisions visible and acute.
 *
 * PERSPECTIVAL GAP:
 *   Sanctity-doctrine authority (institutional medicine, religious leaders) experiences this constraint as principled protection: a moral boundary that prevents devaluation of human life. From their structural seat, enforcing the prohibition is moral work. Victims (terminally ill, elderly, disabled, economically constrained) experience it as coercive prolongation without remedy. From their seat, the constraint's protective function is incomprehensible—it operates as imprisonment in suffering. Physicians sit between: they experience moral distress because healing (their mandate) is prohibited by the doctrine's prohibition. The engine computes these seats' types differently because the structural data declare incompatible directionalities: the doctrine-authority is near the beneficiary end (d~0.1–0.3: collects legitimacy and institutional authority, enforces the rule, can exit by revision), while victims are at the target end (d~0.8–0.95: bear the cost, trapped, no exit). This divergence is why the constraint computes as a snare from victim seats but appears as coordination from doctrine-authority seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Sanctity-doctrine authority declared as beneficiary (gains legitimacy, authority, institutional power from maintaining the doctrine; could change it but does not). Institutional medicine declared as beneficiary (collects authority, liability immunity, ongoing care revenue; could adopt different frameworks but profit from sanctity-enforced prolongation). Terminally ill, elderly, disabled, and economically constrained patients declared as victims (bear the cost—suffering, loss of agency, economic burden on family, risk of coercive shortcuts; no exit). Physicians as secondary payers (moral distress, constrained by two incompatible authority systems). Families as secondary payers (grief, labor, financial burden). Autonomy advocates as excluded (structurally absent from policy-setting in sanctity-dominant institutions). The constraint's directionality derives entirely from this beneficiary/victim structure: those who benefit from enforcing sanctity sit near d=0.0–0.3 (subsidy); those who bear the cost sit near d=0.8–1.0 (full extraction). No directionality override is needed because the structural data declare the asymmetry clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing euthanasia of vulnerable populations by the powerful) was a live, coherent reason for the absolute prohibition in the 19th–early 20th century. That problem is now dead: modern palliative medicine, legal regulation, disability-rights protections, and democratic institutions provide alternative mechanisms to prevent coercive death. The constraint persists despite mandatrophy through: (1) institutional inertia—medical ethics codes, professional licensing, and hospital policy embed sanctity doctrine, making revision costly; (2) doctrine-authority authority's claim to transcendent grounding—the constraint is defended not as instrumental protection but as intrinsic moral law, immune to pragmatic revision; (3) extraction capture—institutional medicine and religious authorities benefit from the constraint's persistence, so they invest in its maintenance; (4) suppressed alternatives—jurisdictions that legalize assisted death face political pressure and international isolation, making institutional transition costly. The mandatrophy is clear in the six_questions divergence: founding_problem_status=dead (the protection rationale is gone) but disappearance_verdict=world_rearranges (the constraint's effect is real—reversing it would change end-of-life practice substantially). This mismatch is mandatrophy: the constraint persists not because it solves its founding problem, but because it has become an instrument of extraction and institutional authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_axiom_intrinsic_vs_relational,
    'Is human dignity an intrinsic property of biological life, or does it emerge from relational and cognitive capacities?',
    'Metaphysical argument from philosophy of personhood; empirical evidence about when capacities for consciousness, agency, and relational engagement emerge and decline; comparative analysis of dignity attributions across cultures.',
    'If dignity is intrinsic to biological life, the sanctity prohibition follows structurally and the constraint reclassifies as genuine protection. If dignity is relational/cognitive, the prohibition becomes arbitrary preservation of biological form and the constraint reclassifies as pure extraction. The reading''s entire foundation depends on this axiom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_axiom_intrinsic_vs_relational, conceptual, 'Ontological status of dignity: intrinsic property vs. relational emergence.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (legal penalties, institutional gatekeeping, medical refusal) or internalized (patients and families internalizing the doctrine such that suppression persists even when external barriers are removed)?',
    'Post-legalization empirical tracking: in jurisdictions that legalize assisted death, measure whether patients/families who initially sought hastening continue to seek it or whether internalized sanctity-doctrine beliefs cause exit from the decision once legal barriers drop. Compare rates of actual assisted-death uptake vs. initial request rates.',
    'If suppression is primarily structural, removing legal barriers should increase assisted-death uptake. If substantially internalized, the doctrine persists as internalized constraint (self-imposed suppression) even after external enforcement is removed. This would indicate the constraint''s actual force is cognitive capture, not institutional coercion, requiring different therapeutic/educational interventions. Misclassifying internalized suppression as purely structural would underestimate the constraint''s power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Decomposition of suppression into structural and internalized components.').

omega_variable(
    victim_vulnerability_asymmetry,
    'Does the constraint''s purported protective function (preventing coercive death) actually protect vulnerable populations (disabled, elderly, economically constrained), or does the doctrine''s asymmetric enforcement expose them to greater risk?',
    'Comparative empirical study: measure coercive-death rates (covert assisted death, unassisted suicide, non-resuscitation decisions) in sanctity-doctrine jurisdictions vs. jurisdictions with legal safeguarded assistance. Disaggregate by vulnerability class (disability status, age, economic status). Measure family pressure and institutionalization rates.',
    'If vulnerable populations experience lower coercive-death rates in sanctity jurisdictions, the protective function is real and the constraint''s victim set is narrower than declared. If vulnerable populations experience higher rates of covert/unassisted/dangerous death and greater institutionalization in sanctity jurisdictions, the constraint has inverted its stated function—it protects the powerful (who can afford prolonged care, have family support) while endangering the vulnerable. This would support the snare classification and the declared victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_vulnerability_asymmetry, empirical, 'Whether the constraint actually protects or endangers its declared victim populations.').

omega_variable(
    kernel_reading_contest_boundaries,
    'Is the contest between sanctity-primary, autonomy-primary, and relational-autonomy readings genuine coexistence (three live positions with no foreclosure), or does one reading logically foreclose the others?',
    'Formal analysis: for each reading, derive what it logically commits to about the OTHER readings'' core premises. Test whether a party can hold both sanctity-doctrine (intrinsic dignity) AND autonomy-doctrine (self-determination is paramount) within a single coherent framework without contradiction.',
    'If the readings are logically incompatible, at least one relation must be ''forecloses'' rather than ''coexists_with.'' If sanctity-doctrine forecloses autonomy-doctrine (or vice versa), the kernel contest is a logical/metaphysical dispute, not a mere preference divergence. If the readings can coexist (one party holds sanctity as intrinsic truth, another party holds autonomy as intrinsic truth, and neither claims the other is incoherent within its own tradition), they genuinely coexist and the relation is ''coexists_with.'' The classification of the reading-relation type determines downstream foreclosure logic in the engine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_boundaries, conceptual, 'Logical independence of the three kernel readings.').

omega_variable(
    institutional_authority_grounding,
    'What grounds the authority structure that enforces sanctity doctrine—is it lineage (tradition/divine revelation), expertise (medical knowledge), extraction (institutional capture of the doctrine for benefit), or distributed disagreement?',
    'Institutional genealogy: trace the doctrine''s transmission from theological sources (Catholic natural law, evangelical theology, Orthodox tradition) through medical licensing, hospital policy, and state law. Identify where interpretation layers exist (diocesan ethics committees, Catholic hospital networks, state bioethics commissions) and whether they function to transmit doctrine or absorb drift. Test whether doctrine-enforcing institutions benefit materially from the constraint (litigation immunity, ongoing care revenue, professional authority).',
    'If grounding is primarily lineage, the constraint is doctrine-transmitted tradition with an interpretation layer. If grounding is primarily extraction (institutions enforce it because they profit), the authority structure is compromised and the doctrine functions as cover for institutional rent-seeking. If distributed (different parties ground it differently—some via lineage, some via medical tradition, some via law), the authority lacks coherence. Grounding classification affects whether the constraint is genuinely foundational (lineage) or captured (extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_authority_grounding, empirical, 'What sustains the institutional authority enforcing sanctity doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 1900, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1900, dignified_death__sanctity_primary, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(dign_tr_t1900, projected).
narrative_ontology:measurement(dign_tr_t1950, dignified_death__sanctity_primary, theater_ratio, 1950, 0.22).
narrative_ontology:measurement_basis(dign_tr_t1950, observed).
narrative_ontology:measurement(dign_tr_t1980, dignified_death__sanctity_primary, theater_ratio, 1980, 0.35).
narrative_ontology:measurement_basis(dign_tr_t1980, observed).
narrative_ontology:measurement(dign_tr_t2000, dignified_death__sanctity_primary, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(dign_tr_t2000, observed).
narrative_ontology:measurement(dign_tr_t2015, dignified_death__sanctity_primary, theater_ratio, 2015, 0.46).
narrative_ontology:measurement_basis(dign_tr_t2015, observed).
narrative_ontology:measurement(dign_tr_t2026, dignified_death__sanctity_primary, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(dign_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t1900, dignified_death__sanctity_primary, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement_basis(dign_be_t1900, projected).
narrative_ontology:measurement(dign_be_t1950, dignified_death__sanctity_primary, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement_basis(dign_be_t1950, observed).
narrative_ontology:measurement(dign_be_t1980, dignified_death__sanctity_primary, base_extractiveness, 1980, 0.51).
narrative_ontology:measurement_basis(dign_be_t1980, observed).
narrative_ontology:measurement(dign_be_t2000, dignified_death__sanctity_primary, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement_basis(dign_be_t2000, observed).
narrative_ontology:measurement(dign_be_t2015, dignified_death__sanctity_primary, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement_basis(dign_be_t2015, observed).
narrative_ontology:measurement(dign_be_t2026, dignified_death__sanctity_primary, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(dign_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1900, dignified_death__sanctity_primary, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement_basis(dign_su_t1900, projected).
narrative_ontology:measurement(dign_su_t1950, dignified_death__sanctity_primary, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement_basis(dign_su_t1950, observed).
narrative_ontology:measurement(dign_su_t1980, dignified_death__sanctity_primary, suppression_requirement, 1980, 0.61).
narrative_ontology:measurement_basis(dign_su_t1980, observed).
narrative_ontology:measurement(dign_su_t2000, dignified_death__sanctity_primary, suppression_requirement, 2000, 0.66).
narrative_ontology:measurement_basis(dign_su_t2000, observed).
narrative_ontology:measurement(dign_su_t2015, dignified_death__sanctity_primary, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement_basis(dign_su_t2015, observed).
narrative_ontology:measurement(dign_su_t2026, dignified_death__sanctity_primary, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(dign_su_t2026, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1900, tn=2026
narrative_ontology:measurement(dign_grid_01, dignified_death__sanctity_primary, accessibility_collapse(class), 1900, 0.64).
narrative_ontology:measurement(dign_grid_02, dignified_death__sanctity_primary, accessibility_collapse(class), 2026, 0.58).
narrative_ontology:measurement(dign_grid_03, dignified_death__sanctity_primary, accessibility_collapse(individual), 1900, 0.72).
narrative_ontology:measurement(dign_grid_04, dignified_death__sanctity_primary, accessibility_collapse(individual), 2026, 0.68).
narrative_ontology:measurement(dign_grid_05, dignified_death__sanctity_primary, accessibility_collapse(organizational), 1900, 0.85).
narrative_ontology:measurement(dign_grid_06, dignified_death__sanctity_primary, accessibility_collapse(organizational), 2026, 0.81).
narrative_ontology:measurement(dign_grid_07, dignified_death__sanctity_primary, accessibility_collapse(structural), 1900, 0.88).
narrative_ontology:measurement(dign_grid_08, dignified_death__sanctity_primary, accessibility_collapse(structural), 2026, 0.76).
narrative_ontology:measurement(dign_grid_09, dignified_death__sanctity_primary, resistance(class), 1900, 0.25).
narrative_ontology:measurement(dign_grid_10, dignified_death__sanctity_primary, resistance(class), 2026, 0.72).
narrative_ontology:measurement(dign_grid_11, dignified_death__sanctity_primary, resistance(individual), 1900, 0.22).
narrative_ontology:measurement(dign_grid_12, dignified_death__sanctity_primary, resistance(individual), 2026, 0.64).
narrative_ontology:measurement(dign_grid_13, dignified_death__sanctity_primary, resistance(organizational), 1900, 0.18).
narrative_ontology:measurement(dign_grid_14, dignified_death__sanctity_primary, resistance(organizational), 2026, 0.58).
narrative_ontology:measurement(dign_grid_15, dignified_death__sanctity_primary, resistance(structural), 1900, 0.15).
narrative_ontology:measurement(dign_grid_16, dignified_death__sanctity_primary, resistance(structural), 2026, 0.52).
narrative_ontology:measurement(dign_grid_17, dignified_death__sanctity_primary, stakes_inflation(class), 1900, 0.38).
narrative_ontology:measurement(dign_grid_18, dignified_death__sanctity_primary, stakes_inflation(class), 2026, 0.64).
narrative_ontology:measurement(dign_grid_19, dignified_death__sanctity_primary, stakes_inflation(individual), 1900, 0.48).
narrative_ontology:measurement(dign_grid_20, dignified_death__sanctity_primary, stakes_inflation(individual), 2026, 0.72).
narrative_ontology:measurement(dign_grid_21, dignified_death__sanctity_primary, stakes_inflation(organizational), 1900, 0.52).
narrative_ontology:measurement(dign_grid_22, dignified_death__sanctity_primary, stakes_inflation(organizational), 2026, 0.68).
narrative_ontology:measurement(dign_grid_23, dignified_death__sanctity_primary, stakes_inflation(structural), 1900, 0.42).
narrative_ontology:measurement(dign_grid_24, dignified_death__sanctity_primary, stakes_inflation(structural), 2026, 0.58).
narrative_ontology:measurement(dign_grid_25, dignified_death__sanctity_primary, suppression(class), 1900, 0.51).
narrative_ontology:measurement(dign_grid_26, dignified_death__sanctity_primary, suppression(class), 2026, 0.68).
narrative_ontology:measurement(dign_grid_27, dignified_death__sanctity_primary, suppression(individual), 1900, 0.58).
narrative_ontology:measurement(dign_grid_28, dignified_death__sanctity_primary, suppression(individual), 2026, 0.74).
narrative_ontology:measurement(dign_grid_29, dignified_death__sanctity_primary, suppression(organizational), 1900, 0.62).
narrative_ontology:measurement(dign_grid_30, dignified_death__sanctity_primary, suppression(organizational), 2026, 0.77).
narrative_ontology:measurement(dign_grid_31, dignified_death__sanctity_primary, suppression(structural), 1900, 0.48).
narrative_ontology:measurement(dign_grid_32, dignified_death__sanctity_primary, suppression(structural), 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel is contested across three structurally distinct constraint stories, each with different ε values, victim/beneficiary sets, and classifications. SANCTITY_PRIMARY (this constraint, ε~0.62, snare) asserts dignity is intrinsic; AUTONOMY_PRIMARY (ε~0.35–0.45 estimated, rope or tangled_rope) asserts dignity resides in self-determination; RELATIONAL_AUTONOMY (ε~0.40–0.50 estimated, tangled_rope) asserts dignity emerges from relational safeguards. Each reading instantiates a different constraint because they measure the same kernel with incommensurable observables—what counts as 'dignified death' is defined differently in each reading. The three stories are not alternative observations of one constraint; they are separate constraints unified by a contested kernel. Each story's claim and metrics are independent; the engine's per-seat classifications will diverge across readings, demonstrating the kernel contest's structural force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
