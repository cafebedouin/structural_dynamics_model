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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI Instrumental Subsidiarity: Neutral Technology Under Legal and Ethical Governance
 *   domain: political_theology/technology_ethics/governance
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel
 *   ai_human_relationship: the instrumental_subsidiarity reading holds that
 *   AI is a morally neutral tool whose deployment can be governed through
 *   legal frameworks, ethical guidelines, and procedural safeguards grounded
 *   in subsidiarity (decisions made at appropriate institutional levels).
 *   This reading vindicates three propositions: technology_moral_neutrality
 *   (AI has no inherent moral character),
 *   subsidiarity_as_procedural_safeguard (proper governance structure
 *   protects human dignity), and human_dignity_through_legal_frameworks (law
 *   and transparency are sufficient protection mechanisms). The constraint
 *   models how this reading operates structurally: it benefits regulatory
 *   authorities and procedural legalism institutions (who collect legitimacy
 *   and authority), requires active enforcement of legal compliance and
 *   transparency mandates, and extracts from subordinated communities who
 *   have no voice in the procedures that claim to protect them. The rising
 *   extractiveness series reflects accumulating regulatory burden on
 *   developers paired with growing procedural theater (impact assessments,
 *   compliance audits, transparency claims) while fundamental asymmetries in
 *   who designs governance remain unchanged.
 *
 * KEY AGENTS:
 *   - Regulatory authorities: set standards, design governance architecture, claim legitimacy through procedural rigor
 *   - Technology developers: bear compliance costs, constrained by overlapping jurisdictions, benefit from legitimacy framing but face expanding liability
 *   - Subordinated communities: subject to deployed systems, excluded from design process, bear extraction risk, identity-locked into exposure they cannot refuse
 *   - Procedural legalism institutions: benefit from expanded jurisdiction over technology governance, legitimacy as gatekeepers of ethical AI
 *   - Human rights advocates: caught between observer seat (regulatory participation) and payer seat (extraction from failed protections)
 *   - Excluded persons: affected by cross-border AI deployment but outside any regulating state's legal jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.62).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.58).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI Instrumental Subsidiarity: Neutral Technology Under Legal and Ethical Governance").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics/governance").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, 'c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0').
narrative_ontology:cs_kernel_codification('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', formalized).
narrative_ontology:cs_authority_grounding('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', lineage).
narrative_ontology:cs_interpretation_layer_present('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0').
narrative_ontology:cs_reading_relation('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_reading_relation('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_axiom('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', foundational, technology_moral_neutrality).
narrative_ontology:cs_axiom_status(technology_moral_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', technology_moral_neutrality, deontological).
narrative_ontology:cs_axiom('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', foundational, subsidiarity_as_procedural_safeguard).
narrative_ontology:cs_axiom_status(subsidiarity_as_procedural_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', subsidiarity_as_procedural_safeguard, instrumental).
narrative_ontology:cs_axiom('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', secondary, legal_framework_sufficiency_for_dignity).
narrative_ontology:cs_axiom_status(legal_framework_sufficiency_for_dignity, holdable).
narrative_ontology:cs_axiom_grounding('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', legal_framework_sufficiency_for_dignity, empirically_contingent).
narrative_ontology:cs_reference_frame('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', subsidiarity_principle_as_procedure).
narrative_ontology:cs_drift_state('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', contemporary_regulatory_expansion_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c3b2afc0-2900-46a0-ad9d-c7b8f5a428a0', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, human_rights_frameworks).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, procedural_legalism).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, subordinated_communities).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, actors_outside_legal_jurisdiction).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, persons_without_regulatory_voice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, technology_developers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, procedural_legalism_institutions).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, technology_developers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frame AI as a neutral tool whose risks are managed through legal governance, ethical guidelines, and transparency mandates. They design and enforce regulatory frameworks, set approval standards for AI deployment, and adjudicate disputes over compliance. Benefit from centralized authority over technology governance and from the framing that legitimacy flows through procedural legalism.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Must comply with multiple overlapping regulatory regimes under the instrumental_subsidiarity framing, each claiming to neutralize AI's inherent risks through governance. They bear the cost of compliance infrastructure, audit burden, and jurisdictional fragmentation, yet the subsidiarity framing prevents fundamental questions about whether regulatory patchwork actually protects human dignity or merely transfers liability.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, technology_developers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, technology_developers, beneficiary).

% Subject to AI systems (employment screening, credit scoring, law enforcement risk assessment, healthcare rationing) deployed under the assumption that legal frameworks and transparency requirements protect them. They have no seat in the regulatory design process, limited recourse after harm, and identity-fused exposure to systems they cannot refuse or escape. The subsidiarity framing treats their dignity as protected by procedures they do not control.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, subordinated_communities, payer,
    powerless, biographical, identity_locked, regional).

% Benefit from the framing that legal process—transparency mandates, impact assessments, audit trails, consent frameworks—IS the mechanism of protecting human dignity. They expand jurisdiction over technology governance, claim legitimacy through procedural rigor, and justify their existence as the gatekeepers of ethical AI deployment.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, procedural_legalism_institutions, beneficiary,
    institutional, generational, analytical, national).

% Witness the constraint's operation from both inside and outside: they participate in regulatory consultations (observer seat) and simultaneously bear extraction when procedural protections fail to prevent harm or when their advocacy is absorbed into compliance theater without changing incentives (payer seat). They document gaps between the instrumental_subsidiarity framing and lived violation of dignity.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, human_rights_advocates, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, human_rights_advocates, payer).

% Affected by AI systems deployed across borders but excluded from the regulatory frameworks that claim to govern them. The instrumental_subsidiarity reading treats AI as governable through law, but legal jurisdiction is territorial; affected persons outside the regulating state's borders have no procedural access, no recourse, and no voice in determining what counts as proper subsidiarity.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, actors_outside_legal_jurisdiction, excluded,
    powerless, immediate, trapped, global).

% The instrumental_subsidiarity reading vindicates the proposition that legal and ethical procedures serve the common good when applied to a morally neutral tool. The common good is invoked but not instantiated: procedural compliance becomes the proxy for common-good achievement, enabling extraction to persist provided legal forms are observed.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, common_good_as_abstract_entity, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(ai_human_relationship__instrumental_subsidiarity, common_good_as_abstract_entity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, regulatory_authorities).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework that AI development can proceed under legitimate legal and ethical governance: technology is morally neutral, risks are managed through regulation and transparency, subsidiarity ensures decisions rest at appropriate levels of authority, and human dignity is protected through procedural safeguards and legal accountability.
% TRANSFER_FUNCTION: Transfers authority over technology governance to institutional regulators and procedural legalism institutions, who collect legitimacy and jurisdictional power in exchange for promised protection of human dignity through legal frameworks. Transfers the burden of compliance to developers. Transfers the risk of regulatory failure onto subordinated communities and excluded persons, who have no voice in the procedure that claims to protect them.
% ABSENT_VOICES: Subordinated communities most affected by AI deployment systems have no guaranteed seat in regulatory design. Persons outside legal jurisdiction (affected by AI deployed across borders) are structurally excluded. Technology workers whose labor enables the system are rarely included as stakeholders. Communities whose dignity claims do not translate into legal standing (undocumented persons, non-citizens, future generations) are absent by design.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the framing that AI governance can be managed through law and ethics would collapse. Technology developers would face competing informal pressures (social movement, market demand, technical communities) instead of centralized regulatory regimes. Subordinated communities would lose even the procedural protections that exist on paper, though those protections' actual efficacy is contested. The legitimacy of institutional authority over technology would have to be re-established through non-legal means.
% FOUNDING_PROBLEM: Early AI deployment (predictive policing, credit scoring, content moderation) revealed harms to individuals and communities with no clear accountability structure. The founding problem framed in this reading is: how can society enable beneficial technology development while protecting human dignity through transparent, accountable governance?
% FOUNDING_PROBLEM_CORROBORATION: Regulatory authorities and procedural legalism institutions affirm the founding problem is live and being solved through expanding governance frameworks. Technology critics and human rights advocates contest whether procedural legalism addresses the founding problem or generates new extraction mechanisms while providing legitimacy cover. Empirical analysis from outside the benefiting parties (academic AI ethics research, human rights documentation, labor organizing in technology sectors) supports the contested reading: some harms are addressed by regulation, many are displaced or abstracted into legal compliance theater.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.45 → 0.62 over 25 time points, then plateaus) reflects the constraint's maturation: early in its operation, regulatory frameworks are sparse and extraction is lower; as the reading becomes institutionalized, compliance burden rises faster than protection mechanisms improve, and extraction accumulates. The theater_ratio rise (0.25 → 0.41) signals growing performativity: transparency mandates, impact assessments, and audit trails proliferate without proportional reduction in harm to subordinated communities. The suppression_requirement series tracks the active enforcement needed to maintain the reading's legitimacy against competing framings (incarnational_humanism, which demands intrinsic reordering of human-technology relationship; technocratic_optimization, which abandons dignity claims entirely). Suppression plateaus at t=15 because the constraint achieves institutional stability by then—the competing readings are absorbed into consultative processes (observed by human rights advocates) rather than decisively defeated. This is characteristic of a tangled_rope under procedural legitimacy: genuine coordination function (shared governance framework enables orderly development) paired with asymmetric extraction (those outside the procedure bear the costs of procedural failure).
 *
 * PERSPECTIVAL GAP:
 *   The regulatory authority and procedural legalism seats experience genuine coordination benefit: clear standards reduce chaos, subsidiarity principles distribute authority legitimately, legal frameworks provide structure. From the subordinated community and excluded-person seats, the same structure operates as enforced extraction: no voice in design, no seat in governance, yet bearing the risk and cost of regulatory failures. The theater_ratio rise is particularly important here: as the reading matures, compliance theater grows faster than substantive protection, which suggests regulators are increasingly focused on maintaining the appearance of governance rather than preventing harm. This is the mark of extraction dynamics: the constraint persists because it benefits those who maintain it (regulatory authorities, procedural legalism institutions), even as its coordination function deteriorates.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed: the derivation chain (beneficiary/victim + power + exit → d) produces the correct directionality values from the structural data. Regulatory authorities are clearly beneficiaries with high power and analytical exit (they set the terms); subordinated communities are clearly victims with low power and identity-locked exit (they cannot refuse AI systems); excluded persons are victims with no legal jurisdiction (trapped, powerless, no appeal). The engine's derivation captures this structure correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy dynamics: its founding mandate (protect human dignity through legal governance of morally neutral AI) is contested and partially obsolete. The founding problem (early harms from unaccountable AI deployment) called for governance; the instrumental_subsidiarity reading answered with procedural legalism. However, empirical analysis shows procedural legalism has not prevented harms—it has absorbed them into legal process (class-action suits, regulatory fines, compliance theater) while leaving underlying asymmetries intact. The constraint's mandate (subsidiarity as procedural safeguard) is live in regulatory sectors but contested by the other kernel readings: incarnational_humanism argues subsidiarity cannot work because technology is not morally neutral; technocratic_optimization argues subsidiarity impedes beneficial optimization. The theater_ratio rise signals the mandate's partial obsolescence: compliance theater (impact assessments, audit trails, transparency claims) has grown disproportionate to mandate-achievement. The constraint persists because regulatory authorities benefit from it and have the power to maintain it, not because its founding mandate is being achieved. This is the classic mandatrophy pattern: the arrangement remains, the function it was meant to serve has been displaced or abstracted, and substituted purposes (regulatory authority, procedural legitimacy, compliance theater) now drive its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_moral_neutrality_contestation,
    'Is AI genuinely morally neutral, or does technology embody the values and biases of its designers and deployers such that ''moral neutrality'' is a cover story for designer-embedded extraction?',
    'Comparative analysis of AI systems showing consistent bias patterns correlated with designer demographics and incentive structures; examination of whether bias in training data, model architecture, and deployment decisions can be separated from ''neutral'' technical choices; case studies of systems claimed neutral but documented to encode particular power relationships.',
    'If technology is not morally neutral, the instrumental_subsidiarity reading''s core premise collapses and the constraint reclassifies toward snare (extraction mechanisms embedded in the tool itself, procedural governance is insufficient). If technology can be made neutral through proper procedures, the reading is vindicated but the constraint''s extractiveness depends on governance quality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_moral_neutrality_contestation, empirical, 'Whether AI is morally neutral or encodes designer values; central to this reading''s viability.').

omega_variable(
    subsidiarity_efficacy_in_digital_governance,
    'Can subsidiarity principle work in AI governance when authority is diffuse across multiple jurisdictions, developers operate globally, and affected communities lack regulatory standing?',
    'Study of how subsidiarity has functioned in comparable transnational governance contexts (internet regulation, pharmaceutical approval, environmental policy); analysis of whether territorial subsidiarity can govern digital systems that operate across borders; documentation of whether excluded persons (outside legal jurisdiction) gain any protection from subsidiarity arrangements.',
    'If subsidiarity cannot function across jurisdictions, the procedural safeguard collapses, and the constraint reclassifies toward tangled_rope or snare (coordination function for developers + extraction from affected persons who remain outside the procedure). If subsidiarity proves functional in digital contexts, the reading gains support but requires expansion to include cross-border voice mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidiarity_efficacy_in_digital_governance, empirical, 'Whether subsidiarity principle can actually govern AI in digital/transnational contexts; determines reading''s structural viability.').

omega_variable(
    legal_frameworks_vs_procedural_theater,
    'How much of the measured theater_ratio increase (compliance theater, impact assessments, transparency mandates) represents genuine protective governance vs. legitimacy performance that substitutes for substantive harm prevention?',
    'Longitudinal study of regulatory interventions: do they prevent harm or redistribute it (moving visible harms to less-visible populations, converting them to slow-moving systemic effects)? Analysis of compliance cost vs. measured harm reduction; documentation of whether firms use compliance theater as liability protection while continuing harmful practices within procedural bounds.',
    'If theater_ratio represents genuine protection, the constraint''s high extractiveness is justifiable cost of governance. If theater is performative substitute for protection, the constraint reclassifies toward piton (performance-maintained rather than function-maintained) or snare (governance theater becomes extraction mechanism itself).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_frameworks_vs_procedural_theater, empirical, 'Whether legal governance is protecting dignity or providing legitimacy cover for continued extraction; determines if extraction is justified coordination cost or dysfunctional mandate-drift.').

omega_variable(
    voice_inclusion_vs_procedural_exclusion,
    'Can procedural legalism protect dignity of persons not included in the procedure? What mechanisms, if any, could give voice to subordinated communities and excluded persons in subsidiarity governance?',
    'Documentation of regulatory processes showing who participates in design; case studies of expanded voice mechanisms (community co-design, mandatory affected-person consultation, cross-border dispute resolution); analysis of whether procedural expansion would conflict with subsidiarity principle or strengthen it.',
    'If procedural legalism cannot include affected voices, the constraint is fundamentally extractive despite benign framing. Structural solution would require abandoning instrumental_subsidiarity reading in favor of incarnational_humanism (which demands integral human dignity, not procedural proxy). If voice can be expanded without collapsing subsidiarity, the constraint gains viability but loses its claim to current legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voice_inclusion_vs_procedural_exclusion, conceptual, 'Whether procedure-based protection is possible without affected-voice inclusion; if not, reading is structurally incoherent.').

omega_variable(
    kernel_reading_interdependence,
    'Is the instrumental_subsidiarity reading logically independent from the incarnational_humanism reading, or does the subsidiarity reading depend on accepting incarnational premises (that human dignity has intrinsic worth that law must respect) even while denying that technology has moral obligations?',
    'Philosophical analysis of the reading''s foundational assumptions: if subsidiarity is justified because persons have inherent dignity requiring protection through procedure, then incarnational humanism''s core premise (human dignity as irreducible) is already accepted, and the disagreement is about whether legal procedure suffices. If subsidiarity can be grounded in purely instrumental terms (optimization of outcomes, not respect for persons), then the readings truly diverge at first principles.',
    'If the readings are logically dependent (instrumental_subsidiarity presupposes incarnational foundation), the constraint''s classification depends on which foundation is operative. If the readings are independent, they represent genuine alternative approaches. This affects how the engine processes competing readings at the kernel level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_interdependence, conceptual, 'Whether instrumental_subsidiarity is logically independent or parasitic on incarnational humanism premises; affects kernel-level processing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_h_tr_t5, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ai_h_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_h_tr_t15, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_h_tr_t25, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_h_be_t5, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ai_h_be_t15, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(ai_h_be_t25, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(ai_h_su_t5, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ai_h_su_t15, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(ai_h_su_t25, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.18).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, regulatory_capture_in_technology_governance).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, procedural_legalism_as_extraction_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel ai_human_relationship. The sibling readings—incarnational_humanism and technocratic_optimization—are separate constraint stories with different ε values, beneficiary/victim structures, and classifications. The kernel_reading system links these three stories bidirectionally so that the engine can track how competing readings affect each other's classification and how drift in one reading creates pressure on the others. The family structure: incarnational_humanism views technology as intrinsically ordered to human dignity (high ε for instrumental_subsidiarity because it treats technology as tool rather than intrinsic good); technocratic_optimization views technology as intrinsically good for optimization (high ε for instrumental_subsidiarity because subsidiarity impedes beneficial innovation); instrumental_subsidiarity (this story) views technology as neutral (moderate ε because coordination function is real but incomplete). All three share the referent (contemporary AI deployment and governance arrangements) but model it differently based on their reading's premises about technology's moral status and the sufficiency of legal procedure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
