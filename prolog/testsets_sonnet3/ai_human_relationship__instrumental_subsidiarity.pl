% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI Governance as Neutral-Tool Regulatory Subsidiarity
 *   domain: political_theology/technology_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the 'instrumental subsidiarity' reading of
 *   the contested AI-human relationship kernel: AI is morally neutral
 *   technology whose ethical weight lies entirely in use-cases, and
 *   subsidiarity operates as a procedural safeguard — law, disclosure
 *   requirements, and layered institutional review (developer compliance,
 *   state regulation, professional ethics boards) — rather than as a
 *   substantive claim about what AI should never be permitted to do to a
 *   human person. This reading coordinates a real problem (getting workable
 *   governance in place amid deep disagreement about AI's nature) but does so
 *   by displacing cost onto those with the least capacity to contest
 *   algorithmic determinations: the procedural apparatus benefits the
 *   institutions that administer and comply with it while leaving powerless
 *   payers with an appeals right rather than a substantive protection. The
 *   rising theater_ratio reflects growing disclosure/certification activity
 *   that increasingly substitutes for contestable limits on deployment.
 *
 * KEY AGENTS:
 *   - ai_developers_and_deployers: primary beneficiary (institutional/arbitrage) — shapes compliance framing while retaining deployment latitude
 *   - regulatory_bodies: agenda_setter (institutional/constrained) — gains jurisdiction from the procedural frame but lacks independent audit capacity
 *   - gig_platform_workers, algorithmically_screened_applicants: primary targets (powerless/trapped) — bear the costs of a frame that treats their harms as correctable defects rather than reasons for prohibition
 *   - communities_lacking_regulatory_capacity: secondary target (powerless/trapped, regional) — the frame's premise of functioning institutional oversight does not hold universally
 *   - catholic_social_teaching_theorists: analytical observer — compares this reading against sibling kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.42).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.28).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.42).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI Governance as Neutral-Tool Regulatory Subsidiarity").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'dbb13aab-be2d-4142-b3f4-546e1568772f').
narrative_ontology:cs_kernel_codification('dbb13aab-be2d-4142-b3f4-546e1568772f', distributed).
narrative_ontology:cs_authority_grounding('dbb13aab-be2d-4142-b3f4-546e1568772f', practice).
narrative_ontology:cs_interpretation_layer_present('dbb13aab-be2d-4142-b3f4-546e1568772f').
narrative_ontology:cs_reading_relation('dbb13aab-be2d-4142-b3f4-546e1568772f', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_reading_relation('dbb13aab-be2d-4142-b3f4-546e1568772f', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_axiom('dbb13aab-be2d-4142-b3f4-546e1568772f', foundational, technology_is_morally_neutral_instrument).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('dbb13aab-be2d-4142-b3f4-546e1568772f', technology_is_morally_neutral_instrument, conventional).
narrative_ontology:cs_axiom('dbb13aab-be2d-4142-b3f4-546e1568772f', foundational, procedural_transparency_suffices_for_dignity_protection).
narrative_ontology:cs_axiom_status(procedural_transparency_suffices_for_dignity_protection, holdable).
narrative_ontology:cs_axiom_grounding('dbb13aab-be2d-4142-b3f4-546e1568772f', procedural_transparency_suffices_for_dignity_protection, instrumental).
narrative_ontology:cs_reference_frame('dbb13aab-be2d-4142-b3f4-546e1568772f', procedural_governance_compromise).
narrative_ontology:cs_drift_state('dbb13aab-be2d-4142-b3f4-546e1568772f', contemporary_ai_regulatory_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dbb13aab-be2d-4142-b3f4-546e1568772f', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_deployers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, professional_ethics_boards).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, gig_platform_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmically_screened_applicants).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, communities_lacking_regulatory_capacity).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technological_neutrality_thesis).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, law_as_sufficient_dignity_safeguard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and deploy AI systems and shape the compliance frameworks that govern them, often through standard-setting bodies and lobbying. Because the tool is framed as morally neutral, responsibility is displaced onto 'use case' classification and downstream regulatory compliance rather than design choices, allowing continued deployment while regulatory processes are pending. Can relocate operations or product lines across jurisdictions to arbitrage regulatory stringency.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_deployers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, ai_developers_and_deployers, agenda_setter).

% Draft and administer risk-tiering, transparency, and certification regimes (e.g. algorithmic impact assessments, disclosure mandates). Gain institutional mandate, budget, and jurisdiction from being the designated adjudicator of what counts as acceptable AI use. Depend on continued framing of AI as governable-through-procedure to justify their existence and scope, but lack technical capacity to audit systems independently of vendor disclosure.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, regulatory_bodies, beneficiary).

% Issue ethics guidelines, certifications, and review processes for AI deployment in medicine, law, and finance. Gain professional standing and gatekeeping authority from being positioned as the ethical check within the neutral-tool frame. Their authority depends on the premise that procedural review, not substantive limits on deployment, is the correct remedy.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, professional_ethics_boards, beneficiary,
    organized, generational, constrained, national).

% Are managed, scheduled, and disciplined by algorithmic systems whose 'neutrality' means the harms they experience (deactivation, wage suppression, opaque scoring) are treated as implementation defects to be corrected through disclosure rules rather than as reasons to prohibit the deployment itself. Cannot exit the platform without losing income and have no standing in the regulatory processes that govern the systems managing them.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, gig_platform_workers, payer,
    powerless, biographical, trapped, national).

% Are sorted by hiring, lending, or benefits-eligibility algorithms. Under the instrumental-subsidiarity frame, their recourse is a transparency right or an appeals process rather than a substantive claim against being scored at all; the burden falls on them to detect and contest an adverse determination they cannot see the basis for.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmically_screened_applicants, payer,
    powerless, biographical, trapped, national).

% Live in jurisdictions without the technical or legal capacity to build or enforce the risk-tiering and certification apparatus the frame presumes exists. The 'law and ethics will govern it properly' premise assumes institutional capacity that is unevenly distributed; where absent, deployment proceeds with no functioning safeguard at all.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, communities_lacking_regulatory_capacity, payer,
    powerless, generational, trapped, regional).

% Analyze whether the instrumental-subsidiarity reading's procedural safeguards actually secure human dignity or merely legitimate deployment by displacing substantive moral scrutiny onto compliance paperwork. Compare this reading against the incarnational-humanism and technocratic-optimization readings of the same underlying kernel.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, catholic_social_teaching_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, diffuse).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable, jurisdiction-portable framework for permitting AI deployment at scale: developers get predictable compliance pathways, regulators get a tractable oversight object (documented risk tiers, disclosures, audits), and professional bodies get a role certifying ethical use — without requiring anyone to adjudicate contested claims about AI's nature or ultimate ends.
% TRANSFER_FUNCTION: Moves the burden of proof and the cost of harm-detection from deployers (who design and profit from the systems) to the people scored, managed, or screened by them, who must discover, document, and contest adverse algorithmic outcomes themselves under a framework that presumes the tool's neutrality rather than scrutinizing its deployment.
% ABSENT_VOICES: Gig workers, screened applicants, and residents of low-capacity jurisdictions rarely sit on the standards bodies or regulatory advisory panels that write the risk-tiering rules; their harms enter the record, if at all, as post-hoc complaint data rather than as a voice in framing what counts as acceptable use.
% DISAPPEARANCE_RATIONALE: If the neutral-tool/procedural-subsidiarity frame were abandoned overnight in favor of substantive limits on deployment (the incarnational-humanism reading) or unrestrained optimization (the technocratic reading), the entire current architecture of risk-tiering, disclosure mandates, and certification boards would lose its rationale — regulators would need new legal theories, developers would face either harder prohibitions or fewer constraints, and current appeals processes for the powerless payers would either be replaced by categorical bans or eliminated entirely.
% FOUNDING_PROBLEM: Rapid deployment of AI systems across employment, credit, health, and criminal justice created harms that needed some governance response, but a full metaphysical or theological verdict on AI's proper relationship to human ends was neither politically available nor doctrinally settled quickly enough to guide policy — so a procedural, tool-neutral compromise emerged that could be implemented without resolving the underlying contest.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and industry standards bodies attest the procedural framework adequately manages risk given present political constraints. Labor advocates, algorithmic accountability researchers, and Catholic social teaching theorists outside the beneficiary set (citing integral human development frameworks) attest the founding problem — protecting human dignity against instrumentalization — remains substantially unaddressed by disclosure-and-appeal mechanisms alone, and that the procedural frame has become a durable substitute for substantive limits rather than a bridge toward them.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).
:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) and rising, because the frame's cost is not overt seizure but a chronic asymmetry in who bears the burden of proof for harm. Suppression is comparatively low (0.28) because this reading does not suppress exit through coercion so much as through the unavailability of an alternative institutional vocabulary — dissent is possible but structurally unheard in standards-setting venues. Theater ratio rises over the interval (0.20 to 0.44) as certification and disclosure activity increasingly substitutes for substantive deployment limits — a classic Goodhart pattern where compliance paperwork becomes the measured proxy for the underlying goal of protecting dignity.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory-body and developer seats, the arrangement reads as successful coordination: a functioning, auditable, jurisdiction-portable governance layer. From the gig-worker and screened-applicant seats, the identical structure operates as enforced asymmetry: their only remedy is a disclosure or appeal mechanism embedded in a frame that never asks whether the deployment itself should have occurred. The engine's per-seat computation should reflect this divergence directly from the power/exit data rather than from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and regulators derive low d (beneficiary end): the frame gives developers continued deployment latitude and gives regulators jurisdiction and budget, in exchange for administering a compliance apparatus they substantially design. Professional ethics boards similarly benefit from certifying authority. Gig workers, screened applicants, and low-capacity communities derive high d (target end): they are structurally trapped (no meaningful exit from algorithmic scoring systems that govern income or eligibility) and their voice in shaping the applicable rules is minimal or absent, which is exactly the asymmetric-extraction component required for tangled_rope alongside the genuine coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The instrumental-subsidiarity frame is not pure extraction — it solved a real coordination problem (some governance had to exist faster than theological or philosophical consensus could arrive) and it continues to provide developers and regulators with a workable common vocabulary. Classifying it as tangled_rope rather than snare preserves that genuine coordination function while still registering the asymmetric cost distribution: the founding problem (harm from unregulated deployment) was real, but the corroboration split shows the procedural remedy has increasingly become the destination rather than a bridge to substantive protection, which is the signature the mandatrophy check exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_safeguard_sufficiency,
    'Does disclosure-and-appeal-based subsidiarity actually protect human dignity, or does it function as a legitimating veneer that permits deployment to proceed while displacing the burden of harm-detection onto those least equipped to bear it?',
    'Longitudinal tracking of outcomes for algorithmically screened populations under the procedural regime versus under jurisdictions that impose substantive deployment limits (e.g. categorical bans on certain scoring uses); compare harm rates and successful-appeal rates.',
    'If procedural safeguards demonstrably reduce harm at rates comparable to substantive limits, the tangled_rope reading understates the coordination function; if harm persists regardless of procedural compliance, the reading understates the extraction and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_safeguard_sufficiency, empirical, 'Whether procedural subsidiarity delivers the dignity protection it claims or merely legitimates continued deployment.').

omega_variable(
    neutrality_thesis_contestability,
    'Is the claim that AI technology is morally neutral (with responsibility located solely in use-cases) itself a defensible philosophical position, or is it a framing convenient to developers that forecloses scrutiny of design-stage value embedding?',
    'Philosophical and technical analysis of whether design choices (training data selection, objective function specification, architecture) constitute value-laden decisions prior to any ''use case,'' as argued by both incarnational-humanism and critical AI ethics literatures.',
    'If neutrality is untenable, the instrumental_subsidiarity reading''s foundational premise is structurally weaker than its siblings'', which would strengthen the case for reclassifying toward snare (extraction disguised as procedural coordination) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_thesis_contestability, conceptual, 'Whether AI''s moral neutrality — the premise this reading''s whole apparatus rests on — is philosophically defensible.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three kernel readings (incarnational_humanism, instrumental_subsidiarity, technocratic_optimization) actually governs practice in any given jurisdiction, given that regulatory text often invokes dignity-language (incarnational) while implementation defaults to procedural compliance (instrumental) and market pressure pushes toward efficiency framing (technocratic)?',
    'Comparative institutional analysis of regulatory text versus enforcement practice versus market outcomes across jurisdictions (EU AI Act implementation, US sectoral guidance, Vatican-adjacent tech ethics initiatives) to determine which reading is operative in practice versus rhetoric.',
    'If instrumental_subsidiarity is the rhetorical cover under which technocratic_optimization actually operates, this story''s classification should be read alongside its sibling''s higher extraction — the two may share more structural overlap in practice than the clean decomposition suggests, though ε remains separately authored for each.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the declared reading (instrumental subsidiarity) is the operative one in practice, or a rhetorical layer over a different reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ai_h_tr_t4, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 4, 0.27).
narrative_ontology:measurement(ai_h_tr_t8, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 8, 0.33).
narrative_ontology:measurement(ai_h_tr_t12, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 12, 0.37).
narrative_ontology:measurement(ai_h_tr_t16, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 16, 0.41).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.44).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ai_h_be_t4, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(ai_h_be_t8, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(ai_h_be_t12, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(ai_h_be_t16, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(ai_h_su_t4, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(ai_h_su_t8, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(ai_h_su_t12, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 12, 0.24).
narrative_ontology:measurement(ai_h_su_t16, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ai_human_relationship kernel, decomposed per the ε-invariance principle: incarnational_humanism (AI ordered to integral human development and the imago Dei claim; expected low extraction where genuinely operative, high suppression of alternatives where used as cover), instrumental_subsidiarity (this story — AI as neutral tool under procedural governance; moderate, rising extraction as tangled_rope), and technocratic_optimization (AI as efficiency instrument measuring human worth by productivity; expected high extraction, snare-leaning). Each reading has its own ε and stakeholder structure; they are linked here because institutional practice frequently shifts between them rhetorically while implementation may diverge from stated commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
