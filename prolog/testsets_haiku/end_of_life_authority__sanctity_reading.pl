% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life End-of-Life Authority (Categorical Prohibition Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity-of-life reading of end-of-life authority is one
 *   institutional settlement of a contested kernel: what grounds legitimate
 *   medical action at the end of life? This reading asserts that human life
 *   has intrinsic value that cannot be overridden by individual preference,
 *   suffering, or autonomy claims. Physicians are positioned as preservers of
 *   life, not facilitators of death. Institutional medical authority enforces
 *   a categorical prohibition on physician-assisted dying, treating requests
 *   for life-ending as symptoms of inadequate palliative care, depression, or
 *   coercion risk—especially for vulnerable populations (elderly, disabled,
 *   economically disadvantaged) whom the reading claims to protect. The
 *   constraint is CLAIMED as tangled_rope (coordination function: unified
 *   mandate for physicians; extraction: suppression of patient autonomy and
 *   choice over one's death). The authored metrics describe high suppression
 *   (0.72) and substantial extraction (0.68) that rises slightly over the
 *   interval, with a growing theater component (from 0.28 to 0.42),
 *   suggesting the palliative-care framing increasingly performs the
 *   functional work of the prohibition while the prohibition itself persists.
 *   This divergence between claim and metrics is deliberate—the engine
 *   measures whether the constraint's actual operation matches its
 *   coordination framing or masks extraction.
 *
 * KEY AGENTS:
 *   - institutional_medical_authority: enforces the categorical prohibition, controls the boundary between permissible comfort care and prohibited life-ending
 *   - sanctity_doctrine_adherents: religious and philosophical traditions whose foundational commitments are vindicated and embedded
 *   - terminally_ill_with_unbearable_suffering: face intractable pain and loss of function; request denied; trapped exit
 *   - elderly_economically_disadvantaged: paternalistically protected from coercion-risk but prevented from accessing choice they might otherwise make
 *   - disabled_persons_at_coercion_risk: protected by the constraint but also rendered invisible as agents; identity-locked in vulnerability
 *   - physicians_as_gatekeepers: required to refuse patient requests; role conflict between beneficence and non-maleficence
 *   - family_members_witnessing_suffering: constrained to witnessing prolonged dying they cannot intervene in
 *   - autonomy_reading_proponents: excluded from institutional authority; their alternative framework is not represented in medical boards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-of-Life End-of-Life Authority (Categorical Prohibition Reading)").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '418cc05a-7b41-43ec-ae8f-cc9dd50958dc').
narrative_ontology:cs_kernel_codification('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', formalized).
narrative_ontology:cs_authority_grounding('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', lineage).
narrative_ontology:cs_interpretation_layer_present('418cc05a-7b41-43ec-ae8f-cc9dd50958dc').
narrative_ontology:cs_reading_relation('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', end_of_life_authority__autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', foundational, intrinsic_value_overrides_autonomy).
narrative_ontology:cs_axiom_status(intrinsic_value_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', intrinsic_value_overrides_autonomy, deontological).
narrative_ontology:cs_axiom('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', foundational, physician_role_is_life_preservation).
narrative_ontology:cs_axiom_status(physician_role_is_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', physician_role_is_life_preservation, conventional).
narrative_ontology:cs_axiom('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', secondary, vulnerability_requires_paternalistic_protection).
narrative_ontology:cs_axiom_status(vulnerability_requires_paternalistic_protection, holdable).
narrative_ontology:cs_axiom_grounding('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', vulnerability_requires_paternalistic_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', sanctity_doctrine_institutional_authority).
narrative_ontology:cs_drift_state('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', contemporary_autonomy_reading_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('418cc05a-7b41-43ec-ae8f-cc9dd50958dc', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, institutional_medical_authority).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, sanctity_doctrine_adherents).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminally_ill_with_unbearable_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, elderly_economically_disadvantaged).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disabled_persons_at_coercion_risk).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, family_members_witnessing_prolonged_suffering).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians_as_gatekeepers).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, family_members_witnessing_suffering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medical licensing boards, hospital ethics committees, and physician associations enforce the categorical prohibition on physician-assisted dying and actively police physician conduct to prevent life-ending interventions outside narrow terminal sedation carve-outs. They adjudicate which deaths are 'natural' and permissible versus 'intentional' and prohibited—a distinction that rides on the sanctity doctrine. They administer continuing education, credentialing, and disciplinary action that reinforce the reading's interpretation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, institutional_medical_authority, agenda_setter,
    institutional, generational, analytical, national).

% Religious and philosophical traditions (Catholic teaching, evangelical Protestantism, natural law jurisprudence, disability rights advocates operating from sanctity premises) whose foundational claim—that human life has intrinsic non-negotiable value—is vindicated and institutionally embedded by the constraint. They collect no material rents but secure the codification of their worldview in medical practice, law, and institutional authority.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, sanctity_doctrine_adherents, beneficiary,
    powerful, civilizational, analytical, global).

% Face months or weeks of progression toward certain death, often with intractable pain, loss of bodily function, and existential distress. The constraint bars physicians from intentionally ending their lives even when they request it repeatedly and competently. Their options are limited to accepting prolonged suffering, seeking informal/illegal alternatives (black-market drugs, traveling to jurisdictions with different rules), or suicide outside medical supervision. The constraint treats their persistent autonomy claim as subordinate to society's commitment to life preservation.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminally_ill_with_unbearable_suffering, payer,
    powerless, immediate, trapped, local).

% Enter the coercion-risk category flagged by sanctity advocates themselves: their explicit worry is that if life-ending were permitted, economic pressure (healthcare costs, family burden, social marginalization of disability) would push vulnerable elderly and poor toward 'choosing' death they would not choose in conditions of genuine security and support. The constraint aims to protect them by foreclosing the option entirely. But the same foreclosure also prevents them from accessing a choice that would alleviate their suffering—trapping them in the very conditions the constraint fears would coerce them.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, elderly_economically_disadvantaged, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, elderly_economically_disadvantaged, payer).

% Disabled advocates highlight that the sanctity constraint, while aimed at protecting them, also expresses a societal unwillingness to invest in disability accommodation and palliative care—the resources that would make continued living bearable. The constraint reflects and reinforces a narrative that disabled life is objectively not worth living, even as it formally prohibits intentional death. Their exit is identity-locked: they cannot leave the disability category that makes them targets of the paternalistic protection; their structural position within the constraint is defined by the very vulnerability the constraint claims to guard.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disabled_persons_at_coercion_risk, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, disabled_persons_at_coercion_risk, payer).

% Physicians are positioned as enforcer-agents: they must refuse requests for life-ending, diagnose and treat suicidality, navigate the boundary between permissible comfort-focused care and prohibited active killing. Individual physicians who doubt the sanctity framework still face licensing risk if they aid a patient's death. Their role requires them to oppose patient will when the patient seeks life-ending, even as they are bound by beneficence to prevent suffering. The constraint creates internal role conflict and suppresses physician discretion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians_as_gatekeepers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, physicians_as_gatekeepers, payer).

% Witness prolonged dying of relatives, often unable to access physician aid in death even when their relative requests it. They experience the constraint as enforcing their loved one's suffering. Some seek informal alternatives (traveling to jurisdictions with medical assistance in dying, black-market palliatives). Their constrained exit is both structural (geography, cost) and relational (identity as child/spouse/caregiver binds them to the dying person's fate).
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, family_members_witnessing_suffering, payer,
    moderate, biographical, constrained, local).

% Medical ethicists, patient advocates, and jurisdictions that have adopted autonomy-based end-of-life frameworks (the Netherlands, Belgium, Canada, some U.S. states) are structurally excluded from this constraint's authority—their alternative reading is not represented in the medical boards and hospital ethics committees that enforce the sanctity interpretation. The constraint exists partly through their exclusion from institutional authority. Their presence in the conversation would directly challenge the constraint's foundational premises.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_reading_proponents, excluded,
    powerful, generational, analytical, national).

% The institutional commitment to non-abandonment under the sanctity framework creates demand for expanded palliative and comfort-focused care—sedation, pain management, spiritual support—as the alternative to life-ending. The constraint vindicates palliative medicine as a specialty and directs resources toward it, though chronically under-resourced relative to acute care. Not an agent but a structural beneficiary of the constraint's logic.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_infrastructure, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(end_of_life_authority__sanctity_reading, palliative_care_infrastructure).

% Criminal law in most jurisdictions treats physician-assisted death as assisted suicide or manslaughter, embedding the sanctity reading in penal code. Prosecutors occasionally bring charges against physicians who honor end-of-life requests; the threat of prosecution reinforces the medical prohibition. Legal authority is the backup enforcer when medical self-regulation weakens.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, legal_authority_enforcing_prohibition, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, institutional_medical_authority).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified institutional commitment that human life is intrinsically valuable and non-negotiable, even in the face of individual requests for death. Solves a coordination problem among physicians, hospitals, and society: what is the physician's role? The sanctity reading answers: never to intentionally end life, always to preserve it—a stable, unambiguous mandate. Coordinates professional identity and institutional policy around a single bright-line rule rather than case-by-case discretion.
% TRANSFER_FUNCTION: Moves authority from individuals (patient autonomy in deciding timing and manner of death) to the institutional medical authority and state (which enforces the categorical prohibition). Transfers suffering—from the patient through refusal of life-ending, to families through witnessing prolonged dying, to physicians through role conflict. Transfers vulnerability—from general human mortality to concentrated risk on terminally ill, elderly, disabled, and economically disadvantaged persons who bear the suppression of the life-ending option most acutely.
% ABSENT_VOICES: Autonomy-reading proponents are structurally excluded from most medical authority structures in jurisdictions where the sanctity reading dominates. Patient autonomy advocates, those who have accessed life-ending in other jurisdictions and found it aligned with their values, and disability advocates who view the paternalistic protection as itself disabling are not represented in licensing boards and hospital ethics committees that enforce the constraint.
% DISAPPEARANCE_RATIONALE: If the constraint vanished—if physicians could legally aid intentional life-ending for competent patients requesting it—medical practice would reorganize: new protocols for assessing patient competence and request authenticity would emerge, palliative care would face different resourcing priorities, the framing of end-of-life conversations would shift from 'accepting inevitable death' to 'choosing timing.' The institutional identity of the physician would bifurcate into life-preservers and death-facilitators (or be redefined to include both). Legal authority would need to rewrite criminal statutes. Family dynamics around dying would restructure—some families would access aid in death they currently cannot; others would retain the present structure of prolonged dying. The world would not return to a pre-constraint state but would reorganize around a different settlement of authority over death.
% FOUNDING_PROBLEM: In the mid-20th century, advances in life-support technology created a new crisis: patients could be maintained in persistent vegetative states, unconscious and dying slowly over months or years. Society lacked a framework for when to stop intervening. The sanctity reading answered: life itself is the value to be preserved; the physician's role is to sustain it; the problem is that technology had erased the natural boundary of death. The constraint institutionalized a commitment to preserve life as the default, treating requests for death as symptoms of inadequate palliative care or depression rather than legitimate expressions of autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Medical historians and bioethicists outside the sanctity tradition (Peter Singer, bioethics commissions in autonomy-reading jurisdictions, palliative care specialists in jurisdictions with both frameworks) attest that the founding crisis—inadequate end-of-life frameworks—has been partially solved: hospice and palliative medicine have matured, legal surrogate-decision frameworks exist, and many jurisdictions have developed autonomy-based frameworks that address the same crisis differently. Sanctity-reading proponents attest the problem persists because they define it as the risk of society devaluing life rather than as the technical problem of prolonged dying. The corroboration outside the benefiting parties is mixed: the founding problem as originally stated (how to manage technology-prolonged dying) has architectural solutions; the problem as redefined by sanctity doctrine (how to prevent society from devaluing human life) remains contestable.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.68: the constraint suppresses a recognized autonomy claim (self-determination in timing and manner of death) that is live in sibling reading jurisdictions. The suppression is not justified by technical inability (physicians have the medical knowledge) but by institutional commitment to a normative premise (intrinsic life value overrides autonomy). Suppression is high (0.72) and rising: it requires active enforcement through licensing discipline, criminal law, and gatekeeping. The theater ratio rises from 0.28 to 0.42, indicating that the functional work of the constraint increasingly rides on framing (palliative care quality, comfort-focused messaging, reframing of 'accepting death' as acceptance-of-suffering-while-preserving-life) rather than crude prohibition. This rise in theater relative to extractiveness suggests the constraint is approaching a bifurcation point: either palliative care genuinely addresses suffering and the theater and extraction both stabilize, or palliative care remains resource-constrained and theater rises further (performing adequacy that functional reality does not support). Accessibility collapse is high (0.78): once patients understand the categorical prohibition, alternatives are nearly invisible within the medical system—travel to other jurisdictions is costly and not available to most vulnerable populations; informal alternatives carry criminal and health risk. Resistance is high (0.71): patient-autonomy advocates, disability-rights perspectives, and autonomy-reading jurisdictions all actively contest the constraint. The measurement series tracks the constraint from an era (t=0, late 20th century) when the sanctity reading dominated most Western medicine toward the present (t=40) when it coexists with autonomy-reading in some jurisdictions and faces organized resistance in others.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (institutional medical authority) computes the constraint as necessary coordination: physicians need a unified mandate, and the sanctity reading provides one. From this seat, the constraint solves the crisis of technology-prolonged dying and protects vulnerable populations from coercion. From the victim seats (terminally ill, elderly, disabled, family members), the same constraint operates as extraction: it suppresses autonomy claims that are live elsewhere, enforces prolonged suffering, and performs protection while denying choice. The coercion-risk seats (elderly economically disadvantaged, disabled persons) experience a paradox: they are protected FROM choice by a constraint that was designed to protect them from coerced choice—the paternalism is structural, not incidental. The excluded seat (autonomy-reading proponents) would compute the constraint as foreclosure—their framework is simply not present in the institutional structure that enforces this reading. The engine should compute these divergences clearly: the institutional medical authority computes as beneficiary/low-target, while the victim seats compute as high-target, and the excluded seat is simply absent from the authority structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional medical authority: d near 0.1–0.2 (full beneficiary). They set the rules, collect authority and professional legitimacy, face no suppression, and can articulate their position in institutional language. Sanctity doctrine adherents: d near 0.05–0.15 (full beneficiary). Their foundational commitment is vindicated; they collect no material rents but secure normative authority. Terminally ill: d near 0.85–0.95 (full target). They bear the constraint's core suppression—the prohibition on life-ending when suffering is unbearable. They have trapped exit (cannot leave the terminal illness category; cannot access a recognized alternative within the system; suicidal alternatives carry risk). Elderly economically disadvantaged: d near 0.80–0.90 (full target). They are the explicitly named coercion-risk populations. The constraint aims to protect them, but the protection mechanism is to foreclose choice entirely—trapping them in the very conditions (economic vulnerability, inadequate palliative care) that create coercion risk. Their exit is constrained by both economics and the paternalistic protection. Disabled persons: d near 0.75–0.85 (full target). Identity-locked: they cannot exit the disability category that makes them targets. The constraint's operation reinforces a narrative that disabled life has diminished value—the protection is intertwined with stigma. Physicians: d near 0.55–0.65 (close to symmetric). They benefit from the unified mandate (professional clarity) but also face suppression (role conflict, licensing risk if they deviate, internal moral tension if they doubt the sanctity framework). Family members: d near 0.70–0.80 (substantial target). They are constrained to witnessing suffering they cannot intervene in; their relational exit is identity-locked (cannot leave the role of child/spouse/caregiver). Autonomy proponents: not seated in the authority structure; excluded entirely. The directionality override on physicians might be warranted: the structural derivation (moderate power, constrained exit, beneficiary mandate) puts them near symmetric, but the on-the-ground experience of physicians who hold autonomy frameworks is more target-like (they face discipline if they violate the prohibition). An override downward for a subset of physicians (those with moderate power but value-conflict with the sanctity reading) could refine this.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was the 20th-century crisis of technology-prolonged dying: respirators and feeding tubes created states of suspended death. The sanctity reading solved this by institutionalizing a commitment to life preservation as the default—establishing when NOT to intervene became as important as when TO intervene. The founding problem (how to manage technology-prolonged dying) is substantially solved architecturally: most jurisdictions now have legal frameworks for surrogate decision-making, advance directives, and withdrawal of life support. But the sanctity reading redefines the problem: it becomes not 'how to manage prolonged dying' but 'how to prevent society from devaluing life.' This redefinition is contestable. The mandatrophy is partial: the original founding problem is dead or solved (technology-prolonged dying is managed; legal frameworks exist), but the constraint persists because it now serves a different (and contestable) function—encoding a specific philosophical commitment about the intrinsic value of life. The constraint is not purely mandatrophic (it is not pure performance with no function) because it does enforce a unifying mandate for institutional medicine and does protect some vulnerable populations from certain coercion risks. But the theater ratio rising from 0.28 to 0.42 suggests the constraint increasingly relies on reframing (comfort-care narratives) to justify persistence beyond the solved founding problem. This is classic mandatrophy drift: the constraint outlives its founding justification and increasingly performs that justification rather than executing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_protection_paradox,
    'Does the paternalistic protection of vulnerable populations from coerced choice actually prevent coercion, or does it constitute a form of coercion—removing choice to prevent choice-under-pressure?',
    'Comparative empirical analysis: in jurisdictions with autonomy-based frameworks (Netherlands, Belgium, Canada), do vulnerable populations (elderly, disabled, economically disadvantaged) report higher rates of coerced choosing for death than in sanctity-reading jurisdictions? The answerable question is empirical (do vulnerable populations choose differently in frameworks that permit choice?), but the conceptual question is normative (is prevention-of-choice-to-prevent-coerced-choice itself protective or oppressive?).',
    'If vulnerable populations in autonomy-reading jurisdictions do NOT report higher coercion, the paternalistic justification for the sanctity reading is undermined; the constraint would then appear to extract autonomy without delivering protection. If they DO report higher coercion, the constraint''s paternalistic framing is vindicated. But even vindication does not resolve the normative question: whether preventing choice to prevent coerced choice is legitimate protection or itself coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_protection_paradox, conceptual, 'Whether the constraint protects or coerces vulnerable populations through paternalism.').

omega_variable(
    palliative_care_adequacy,
    'Is the rising theater_ratio (from 0.28 to 0.42) indicating that palliative care quality has genuinely improved such that the functional suppression of autonomy has become less necessary, or is the theater_ratio rising because the constraint is increasingly performing adequacy without functional delivery?',
    'Track palliative care resource allocation (percentage of hospital budget, number of trained specialists, median time from terminal diagnosis to palliative-care enrollment), patient-reported pain and symptom burden at end of life, and rates of patients reporting death-wish motivated by inadequate symptom control (vs. autonomy-motivated death wishes). If resources and reported adequacy track upward while death-wishes-from-poor-symptom-control decline, the theater rise reflects genuine functional improvement. If resources remain flat or declining while death-wishes persist, the theater rise reflects narrative reframing without functional change.',
    'Genuine palliative adequacy would suggest the constraint is approaching a stable equilibrium where life-ending requests are rarer because suffering is better managed. Narrative-theater without functional change would suggest the constraint is drifting toward pure performance—the prohibition persists but increasingly justified by a comfort-care framing that does not match actual resource deployment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palliative_care_adequacy, empirical, 'Whether theater rise reflects actual palliative-care improvement or narrative reframing without functional delivery.').

omega_variable(
    intrinsic_value_givenness,
    'Is the intrinsic value of human life a given fact about the world (discoverable, non-negotiable, grounding the prohibition), or is it a normative commitment that societies CHOOSE to make (reversible, culturally contingent, contestable)?',
    'This is conceptual, not empirical. The resolution depends on what metaphysical and moral framework the interpreter adopts. From a natural-law perspective, intrinsic value is given and the constraint expresses a reality outside society''s choice. From a constructivist or contractarian perspective, intrinsic-value commitments are made by societies and can be remade. The question is not resolvable by data but by philosophical tradition.',
    'If intrinsic value is given, the sanctity reading is grounded in reality and deviations (autonomy readings) are philosophical error. If intrinsic value is a commitment, both readings are defensible normative stances, and the constraint''s authority rests on institutional power, not metaphysical fact. This omega locates the exact point where the readings diverge at their foundation—the nature of the value ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_givenness, conceptual, 'Metaphysical status of intrinsic human value: given fact or normative commitment.').

omega_variable(
    vulnerable_population_agency,
    'Are vulnerable populations (elderly, disabled, economically disadvantaged) properly characterized as at-risk-of-coerced-death (requiring paternalistic protection from choice), or as agents with legitimate preferences about death that are suppressed by the constraint?',
    'Qualitative research: interview elderly, disabled, and economically disadvantaged persons about their own preference-formation. Do they experience the sanctity constraint as protective (preventing coercion they feared), oppressive (removing choices they would make), or some mixture? The resolution is not what external observers judge is ''really'' their interest, but what these populations themselves report about their agency and preference.',
    'Widespread reported experience of protection would support the constraint''s paternalistic framing. Widespread reported experience of oppression would suggest the vulnerability frame mischaracterizes these populations'' agency. Mixed reports would indicate the constraint both protects and oppresses—a constitutive dilemma without clean resolution. This omega captures the core identity-lock problem: these populations cannot exit the vulnerability category, and the constraint''s protection is intertwined with their disempowerment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_agency, empirical, 'Whether vulnerable populations experience the constraint as protective or oppressive.').

omega_variable(
    kernel_reading_contestation,
    'Across the three sibling readings (sanctity, autonomy, slippery_slope), which structural settlement of the end-of-life kernel is most defensible: the categorical prohibition (sanctity), the autonomy-based framework (autonomy), or the empirical hypothesis that autonomy-based frameworks expand to non-terminal populations (slippery_slope)?',
    'This omega documents that the three readings are live positions held by different institutional actors and jurisdictions. No data will resolve which is ''correct''—the readings rest on different normative premises (is life intrinsically valuable? is autonomy paramount? are empirical trajectories predictable?). The resolution is political and institutional: which reading achieves authority, whose voice is represented in medical boards and legislatures, what power dynamics determine the settlement. This omega marks the irreducible pluralism of the kernel.',
    'The three readings cannot coexist stably in a single jurisdiction—institutional authority must settle on one. Where the sanctity reading dominates, autonomy-seeking patients are suppressed; where autonomy reading dominates, sanctity advocates are marginalized; where slippery_slope concerns dominate, restrictions are tightened preemptively. The engine''s task is to measure these divergences per seat, not to resolve which reading is metaphysically correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, preference, 'Which normative settlement of the end-of-life kernel is most defensible—an unsettlable question resting on values.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__sanctity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__sanctity_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__sanctity_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__sanctity_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__sanctity_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__sanctity_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__sanctity_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__sanctity_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__sanctity_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__sanctity_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__sanctity_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__sanctity_reading, suppression_requirement, 32, 0.72).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel generates three distinct constraint stories: sanctity_reading (this file), autonomy_reading, and slippery_slope_mechanism. Each story instantiates a different institutional reading of the same contested kernel—what grounds legitimate medical action at the end of life. The sanctity reading asserts life's intrinsic value overrides autonomy; the autonomy reading asserts individual choice should determine timing and manner of death; the slippery_slope reading asserts autonomy-based frameworks empirically expand beyond their justified scope. These are not three perspectives on one constraint but three structurally distinct constraints riding the same kernel. Each has its own ε, beneficiary/victim structure, stakeholder positions, and classification. They are linked here because they share the kernel and mutually affect institutional authority (a jurisdiction that adopts the autonomy reading undercuts the sanctity reading's authority; expansion under autonomy reading feeds the slippery_slope reading's validation). The ε-invariance principle (DP-001) requires separate stories: the referent 'what constitutes appropriate end-of-life medical action' is the same across readings, but how each reading measures extraction differs (sanctity measures extraction-of-life-value; autonomy measures extraction-of-choice; slippery_slope measures extraction-of-predictability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, powerless, 0.88).
constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
