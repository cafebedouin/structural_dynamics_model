% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Slippery Slope Mechanism in End-of-Life Autonomy Frameworks
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint models the empirically observed drift in jurisdictions
 *   that legalized assisted dying on autonomy grounds: initial statutes
 *   limited to competent, terminally ill adults predictably expand within
 *   5-10 years to include incompetent patients (via advance directives or
 *   surrogate consent) and non-terminal conditions (chronic suffering,
 *   psychiatric illness, disability). The autonomy framework becomes the
 *   vehicle for its own inversion — the safeguard becomes the mechanism of
 *   exposure. This reading instantiates the 'slippery slope' not as a logical
 *   fallacy but as an observed structural dynamic: the framework's own
 *   conceptual architecture (autonomy as self-determination + suffering as
 *   justification) generates pressure for expansion because the limiting
 *   criteria (competence, terminality) are conceptually unstable within the
 *   framework's own logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.54).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.54).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Slippery Slope Mechanism in End-of-Life Autonomy Frameworks").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'df11a07b-4e4b-4013-9def-e0cf1c90d0be').
narrative_ontology:cs_kernel_codification('df11a07b-4e4b-4013-9def-e0cf1c90d0be', formalized).
narrative_ontology:cs_authority_grounding('df11a07b-4e4b-4013-9def-e0cf1c90d0be', lineage).
narrative_ontology:cs_interpretation_layer_present('df11a07b-4e4b-4013-9def-e0cf1c90d0be').
narrative_ontology:cs_reading_relation('df11a07b-4e4b-4013-9def-e0cf1c90d0be', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('df11a07b-4e4b-4013-9def-e0cf1c90d0be', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('df11a07b-4e4b-4013-9def-e0cf1c90d0be', foundational, autonomy_frameworks_necessarily_expand).
narrative_ontology:cs_axiom_status(autonomy_frameworks_necessarily_expand, holdable).
narrative_ontology:cs_axiom_grounding('df11a07b-4e4b-4013-9def-e0cf1c90d0be', autonomy_frameworks_necessarily_expand, empirically_contingent).
narrative_ontology:cs_axiom('df11a07b-4e4b-4013-9def-e0cf1c90d0be', foundational, vulnerability_justifies_safeguards_not_expansion).
narrative_ontology:cs_axiom_status(vulnerability_justifies_safeguards_not_expansion, holdable).
narrative_ontology:cs_axiom_grounding('df11a07b-4e4b-4013-9def-e0cf1c90d0be', vulnerability_justifies_safeguards_not_expansion, deontological).
narrative_ontology:cs_reference_frame('df11a07b-4e4b-4013-9def-e0cf1c90d0be', original_autonomy_legislation).
narrative_ontology:cs_drift_state('df11a07b-4e4b-4013-9def-e0cf1c90d0be', contemporary_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('df11a07b-4e4b-4013-9def-e0cf1c90d0be', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, legislative_drafting_bodies).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, professional_regulatory_bodies).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, institutional_ethics_committees).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, sanctity_aligned_advocacy_groups).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_without_advance_directives).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, marginalized_populations_lacking_advocacy).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, families_pressured_into_surrogate_decisions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_vindicates(end_of_life_authority__slippery_slope_mechanism, autonomy_as_protective_safeguard).
narrative_ontology:constraint_vindicates(end_of_life_authority__slippery_slope_mechanism, vulnerability_justifies_expanded_safeguards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enact end-of-life legislation with initially narrow autonomy-based criteria. Benefit from the law's symbolic legitimacy and political capital; later amendments expand eligibility without new legislative fights because the framework itself generates pressure for expansion. Control the formal text but face limited accountability for interpretive drift.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, legislative_drafting_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Medical colleges and nursing regulators interpret and enforce eligibility criteria. Gain professional authority and discretionary power as the framework expands; their interpretive guidance becomes de facto law. Exit is constrained by professional identity and institutional mandate — they cannot disavow the framework without undermining their regulatory legitimacy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, professional_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, professional_regulatory_bodies, beneficiary).

% Hospital and regional ethics committees adjudicate individual cases. Gain procedural authority and resource allocation control as case volume grows with expanded criteria. Their professional identity is fused with the framework — they became ethicists *through* this work; exit would require reconstituting their professional self-concept.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, institutional_ethics_committees, beneficiary,
    organized, biographical, identity_locked, local).

% Groups that oppose euthanasia/assisted dying on principle. Paradoxically benefit from the slippery slope: each expansion validates their warnings and fuels fundraising, recruitment, and political influence. They do not administer the framework but extract symbolic and material capital from its drift. Mobile exit — they could pivot to other issues but the slope is their most effective mobilization engine.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, sanctity_aligned_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Patients lacking decisional capacity (advanced dementia, severe intellectual disability, persistent vegetative state) who never executed advance directives. Become eligible for life-ending procedures through surrogate decision-makers applying expanded 'best interests' criteria that now include 'chronic suffering' and 'quality of life' judgments made by others. No exit — they cannot consent, dissent, or leave the jurisdiction.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_patients_without_advance_directives, payer,
    powerless, immediate, trapped, local).

% Patients with severe chronic conditions (neurodegenerative, psychiatric, refractory pain) who are not dying but experience profound suffering. Originally excluded by 'terminal illness' requirement; now eligible under expanded 'grievous and irremediable suffering' criteria. Constrained exit — could refuse but face structural pressure (inadequate palliative support, family burden, institutional incentives) that makes refusal costly.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_suffering_patients, payer,
    moderate, biographical, constrained, national).

% Disabled, elderly, poor, racialized, and institutionalized populations who lack effective advocacy access. Disproportionately steered toward life-ending options when eligibility expands because their suffering is read as 'burden' rather than 'injustice requiring support.' Trapped by structural intersection of the framework with existing inequities — no meaningful exit from either the constraint or the conditions that make them its targets.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, marginalized_populations_lacking_advocacy, payer,
    powerless, biographical, trapped, national).

% Family members designated as substitute decision-makers for incompetent patients. Pressured by clinical teams, institutional pathways, and social expectations to authorize life-ending procedures framed as 'respecting autonomy' or 'relieving suffering.' Identity-locked — their role as 'loving caregiver' fuses with the decision; refusing feels like abandonment, authorizing feels like betrayal. No clean exit from the relational frame.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, families_pressured_into_surrogate_decisions, payer,
    moderate, biographical, identity_locked, local).

% The original intended beneficiaries: competent adults with terminal illness requesting assisted dying. Gain genuine access to a desired option. Mobile exit — they can use the framework or not; their situation is not structurally dependent on its expansion. Included because the framework's legitimacy still rests on their case.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    moderate, immediate, mobile, local).

% Clinicians specializing in symptom management and end-of-life support. Structurally excluded from eligibility adjudication despite being the primary alternative to life-ending procedures. Their expertise is consulted but not determinative; resource allocation favors the framework over palliative expansion. Constrained exit — they could advocate louder but institutional incentives marginalize palliative investment.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, palliative_care_clinicians, excluded,
    organized, biographical, constrained, national).

% Academic analysts tracking empirical drift in eligibility criteria, jurisdictional comparisons, and theoretical coherence. No material stake in the framework's operation; their role is diagnostic. Analytical exit by definition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, bioethics_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally structured pathway for competent terminal patients to control the timing and manner of death, replacing ad-hoc clinical decisions and criminal liability with transparent criteria and procedural safeguards.
% TRANSFER_FUNCTION: Transfers authority over life-ending decisions from individual autonomous choice to institutional interpretation: expands eligibility from competent+terminal to incompetent+non-terminal, moving decision power from patients to surrogates, committees, and regulators. Transfers the burden of suffering from 'unbearable to the patient' to 'judged burdensome by others.'
% ABSENT_VOICES: Future patients who will face expanded criteria but cannot yet speak; disabled people's organizations that warned of this drift but were excluded from legislative hearings as 'ideological'; palliative care researchers whose evidence on unmet needs is cited to justify expansion but whose solutions are not funded.
% DISAPPEARANCE_RATIONALE: If the slippery slope mechanism vanished — i.e., if eligibility criteria were frozen at competent+terminal with no interpretive expansion — the end-of-life landscape would reorganize: legislative focus would shift to palliative investment; professional bodies would lose expanded discretionary authority; sanctity-advocacy groups would lose their primary mobilization engine; incompetent and chronic-suffering populations would revert to existing (inadequate) protection regimes. The world rearranges because the drift *is* the constraint's operating logic.
% FOUNDING_PROBLEM: Competent terminally ill patients faced criminal prosecution for seeking assisted death and had no legal pathway to control their dying; clinicians who assisted faced murder charges; families endured traumatic, uncontrolled deaths.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is historically attested by court records (Rodriguez v British Columbia, Carter v Canada), legislative debates, and clinical testimony from the 1990s-2010s. Its *status* is contested: the original autonomy-reading proponents attest the problem is substantially solved for the original population (holdable); slippery-slope analysts attest the problem has mutated into a new one — the framework now creates the very vulnerabilities it was meant to protect against (holdable); sanctity-reading proponents attest the founding problem was a strategic wedge (holdable). No single corroborating source outside all three beneficiary sets exists — the contest *is* the corroboration structure.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is high because the framework transfers life-ending authority from the individuals it claims to empower to institutional interpreters who expand eligibility. Suppression (0.54) is moderate — the constraint does not physically prevent exit but structurally channels vulnerable populations toward the procedure through inadequate alternatives and interpretive pressure. Theater (0.42) is substantial and rising: safeguards (independent assessments, waiting periods, reporting) are real but increasingly performative as eligibility expands beyond their design basis. Accessibility collapse (0.48) and resistance (0.52) reflect that alternatives exist (palliative care, disability supports) but are systematically underfunded while the framework expands. The measurement grid shows coordinated drift: extraction and theater rise together as the framework's coordination function (protecting autonomous choice) is displaced by its extraction function (institutional authority over vulnerable lives).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (legislators, regulators) experience this as a coordination success: they built a framework that works and adapts. The payer seats experience it as extraction: the framework's adaptation *is* the extraction. The engine computes this divergence from the declared power/exit/beneficiary structure — the claimed_type 'tangled_rope' reflects the genuine coordination origin (competent terminal cases) fused with the observed extraction drift (incompetent/non-terminal expansion). The autonomy_reading would claim 'rope'; the sanctity_reading would claim 'snare'; this reading claims 'tangled_rope' because both functions are structurally present and the drift mechanism is the coupling between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative and regulatory bodies are structural beneficiaries (d ~ 0.15) — they gain authority and legitimacy with minimal cost. Professional ethics committees are identity-locked beneficiaries (d ~ 0.2) — their professional existence is constituted by the framework. Sanctity-advocacy groups are mobile beneficiaries (d ~ 0.3) — they extract symbolic capital but could pivot. The four victim groups are all high-d: incompetent patients are trapped (d ~ 0.95); marginalized populations are trapped by structural intersection (d ~ 0.9); chronic suffering patients are constrained by inadequate alternatives (d ~ 0.75); families are identity-locked into surrogate decisions (d ~ 0.8). Competent terminal patients are near-symmetric (d ~ 0.45) — genuine beneficiaries of the original coordination function. Palliative clinicians are excluded (d ~ 0.6) — their alternative is structurally suppressed. Bioethics scholars are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (protect autonomous choice for the dying) has not atrophied — it has been *captured* by the expansion dynamic. The framework still serves the original population (competent terminal patients gain access) while simultaneously expanding to serve institutional and symbolic interests. This is not piton (inertial persistence of dead function) but tangled_rope (active coordination fused with active extraction). The founding problem is 'contested' not 'dead' — the original problem persists for some while the framework creates new problems for others. Mandatrophy resolution would require either freezing criteria at competent+terminal (politically unstable) or decoupling the coordination function from the expansion logic (conceptually difficult within autonomy discourse).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansion_causality,
    'Is the observed eligibility expansion caused by the autonomy framework''s internal logic, by external political pressure, or by their interaction?',
    'Comparative jurisdictional analysis: jurisdictions with identical autonomy frameworks but different political cultures show different expansion velocities. If expansion correlates with framework features (e.g., ''suffering'' vs ''terminal illness'' language) across cultures, internal logic dominates. If it correlates with political alignment regardless of framework text, external pressure dominates.',
    'If internal logic dominates, the slippery slope is structurally necessary — any autonomy framework *will* expand. If external pressure dominates, the slope is contingent — better drafting or political coalitions could stabilize criteria. If interaction, both the framework''s conceptual architecture and the political economy of implementation are causal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_causality, empirical, 'Causal attribution for the drift mechanism').

omega_variable(
    autonomy_sanctity_coupling,
    'Does the autonomy framework''s expansion functionally serve sanctity concerns (by validating warnings and mobilizing opposition) or undermine them (by normalizing life-ending)?',
    'Longitudinal analysis of sanctity-advocacy group capacity: if their resources, political influence, and recruitment grow *with* each expansion, the coupling is functional for them. If they shrink despite expansions, the coupling is destabilizing.',
    'If functional for sanctity groups, the autonomy/sanctity opposition is a stable dialectic that sustains both — the framework''s drift feeds its opposition. If destabilizing, the slope may eventually collapse the sanctity position entirely. This determines whether the kernel''s dispute is a persistent structural feature or a transient phase.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_sanctity_coupling, conceptual, 'Structural relationship between the autonomy framework''s drift and sanctity-advocacy mobilization').

omega_variable(
    competence_boundary_stability,
    'Can a stable competence threshold be maintained within an autonomy framework, or does autonomy discourse necessarily dissolve the competence boundary?',
    'Philosophical analysis of whether ''autonomy'' as a justificatory concept can coherently limit itself to competent agents without either excluding the vulnerable (arbitrary line) or including them (expansion). Empirical test: jurisdictions that adopted ''decisional capacity at time of request'' vs ''advance directive'' models — does the latter expand faster?',
    'If competence boundary is internally unstable, the slippery slope is conceptually necessary — no autonomy framework can stop at competence. If stable boundaries are possible, the observed drift is a policy failure, not a structural necessity. This is the core theoretical dispute between autonomy_reading and slippery_slope_mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_boundary_stability, conceptual, 'Whether the autonomy concept can structurally contain its own eligibility limits').

omega_variable(
    palliative_substitution_effect,
    'Does the framework''s expansion causally suppress palliative care investment, or do both expand in parallel driven by aging demographics?',
    'Counterfactual resource allocation modeling: compare palliative funding trajectories in expansion jurisdictions vs non-expansion jurisdictions with matched demographics. If expansion jurisdictions show relative palliative suppression, the framework extracts resources from the alternative.',
    'If causal suppression, the transfer function includes resource displacement — the framework doesn''t just add an option, it structurally undermines the alternative. If parallel, the extraction is primarily authority-based, not resource-based.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palliative_substitution_effect, empirical, 'Whether framework expansion displaces palliative care investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_slippery_tr_t2015, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(eol_slippery_tr_t2018, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(eol_slippery_tr_t2021, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2021, 0.27).
narrative_ontology:measurement(eol_slippery_tr_t2024, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2024, 0.35).
narrative_ontology:measurement(eol_slippery_tr_t2027, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2027, 0.4).
narrative_ontology:measurement(eol_slippery_tr_t2030, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 2030, 0.42).

% Extraction over time
narrative_ontology:measurement(eol_slippery_be_t2015, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(eol_slippery_be_t2018, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2018, 0.31).
narrative_ontology:measurement(eol_slippery_be_t2021, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement(eol_slippery_be_t2024, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2024, 0.56).
narrative_ontology:measurement(eol_slippery_be_t2027, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2027, 0.63).
narrative_ontology:measurement(eol_slippery_be_t2030, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eol_slippery_su_t2015, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement(eol_slippery_su_t2018, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(eol_slippery_su_t2021, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(eol_slippery_su_t2024, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement(eol_slippery_su_t2027, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2027, 0.51).
narrative_ontology:measurement(eol_slippery_su_t2030, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 2030, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__slippery_slope_mechanism, 0.08).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, palliative_care_resource_allocation).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, disability_rights_protection_framework).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three constraint stories: autonomy_reading (coordination function for competent terminal patients, low extraction), sanctity_reading (extraction function for sanctity-advocacy groups, moderate extraction), and slippery_slope_mechanism (the drift coupling between them, high extraction). This story is the coupling mechanism — it exists because the autonomy framework's conceptual architecture generates expansion pressure that the sanctity framework mobilizes against. The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, organized, 0.25).
constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, institutional, 0.15).
constraint_indexing:directionality_override(end_of_life_authority__slippery_slope_mechanism, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
