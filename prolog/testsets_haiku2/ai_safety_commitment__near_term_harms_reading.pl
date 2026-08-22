% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__near_term_harms_reading, []).

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
 *   constraint_id: ai_safety_commitment__near_term_harms_reading
 *   human_readable: Near-Term AI Safety: Documented Harms Mitigation
 *   domain: technology/governance/risk_assessment
 *
 * SUMMARY:
 *   The near-term harms reading of AI safety is ONE instantiation of a
 *   contested kernel: what does 'AI safety' mean? This reading asserts that
 *   AI safety is fundamentally about preventing documented, measurable,
 *   present-day harms to vulnerable populations: algorithmic bias in hiring
 *   and lending, discrimination in content moderation, labor exploitation
 *   through algorithmic management, and misinformation amplification. This
 *   reading originated from and is sustained by affected communities,
 *   advocacy organizations, labor movements, safety researchers studying
 *   fairness and bias, and regulators tasked with protecting citizens. The
 *   constraint structures institutional power and resource allocation around
 *   this definition. It is a tangled rope: there is genuine coordination
 *   value (companies and societies both benefit from preventing measurable
 *   harms), but the definition of 'safety' and 'harm' is authored by the
 *   benefiting parties (companies, safety researchers, regulators), while the
 *   populations most harmed have minimal voice. Extraction grows over the
 *   interval as the framework becomes institutionalized: companies gain
 *   authority to define auditing standards and safe deployment practices,
 *   researchers gain funding and legitimacy, and marginalized populations
 *   gain only the limited recourse of compliance theater.
 *
 * KEY AGENTS:
 *   - tech_companies: agenda-setter and primary beneficiary; control deployment standards and auditing definitions
 *   - marginalized_populations: powerless payers; subject to algorithmic decisions without voice in safety definition
 *   - gig_workers: organized payers; targeted by algorithmic management systems and exploitation
 *   - safety_researchers: powerful beneficiaries; gain funding, publication venues, and institutional authority from near-term frame
 *   - regulators: institutional agenda-setters; adopt near-term frame to justify interventions already endorsed by tech companies
 *   - existential_risk_researchers: excluded; their research agenda competes for the same resources but is structurally sidelined by near-term framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__near_term_harms_reading, 0.72).
domain_priors:theater_ratio(ai_safety_commitment__near_term_harms_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ai_safety_commitment__near_term_harms_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__near_term_harms_reading, "Near-Term AI Safety: Documented Harms Mitigation").
narrative_ontology:topic_domain(ai_safety_commitment__near_term_harms_reading, "technology/governance/risk_assessment").

domain_priors:requires_active_enforcement(ai_safety_commitment__near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__near_term_harms_reading, '40bf7de2-fc19-4b89-a3ad-b416968ead58').
narrative_ontology:cs_kernel_codification('40bf7de2-fc19-4b89-a3ad-b416968ead58', distributed).
narrative_ontology:cs_authority_grounding('40bf7de2-fc19-4b89-a3ad-b416968ead58', extraction).
narrative_ontology:cs_reading_relation('40bf7de2-fc19-4b89-a3ad-b416968ead58', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('40bf7de2-fc19-4b89-a3ad-b416968ead58', ai_safety_commitment__dual_priority_reading, coexists_with).
narrative_ontology:cs_axiom('40bf7de2-fc19-4b89-a3ad-b416968ead58', foundational, algorithmic_harms_present_and_immediate).
narrative_ontology:cs_axiom_status(algorithmic_harms_present_and_immediate, holdable).
narrative_ontology:cs_axiom_grounding('40bf7de2-fc19-4b89-a3ad-b416968ead58', algorithmic_harms_present_and_immediate, empirically_contingent).
narrative_ontology:cs_axiom('40bf7de2-fc19-4b89-a3ad-b416968ead58', foundational, present_harm_mitigation_takes_priority_over_speculative_risk).
narrative_ontology:cs_axiom_status(present_harm_mitigation_takes_priority_over_speculative_risk, holdable).
narrative_ontology:cs_axiom_grounding('40bf7de2-fc19-4b89-a3ad-b416968ead58', present_harm_mitigation_takes_priority_over_speculative_risk, deontological).
narrative_ontology:cs_axiom('40bf7de2-fc19-4b89-a3ad-b416968ead58', secondary, marginalized_populations_should_center_safety_definitions).
narrative_ontology:cs_axiom_status(marginalized_populations_should_center_safety_definitions, holdable).
narrative_ontology:cs_axiom_grounding('40bf7de2-fc19-4b89-a3ad-b416968ead58', marginalized_populations_should_center_safety_definitions, deontological).
narrative_ontology:cs_reference_frame('40bf7de2-fc19-4b89-a3ad-b416968ead58', algorithmic_systems_causing_documented_harms_to_present_populations).
narrative_ontology:cs_drift_state('40bf7de2-fc19-4b89-a3ad-b416968ead58', contemporary_safety_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('40bf7de2-fc19-4b89-a3ad-b416968ead58', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, deployment_platforms).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, marginalized_populations).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, gig_workers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, algorithmic_discrimination_targets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__near_term_harms_reading, safety_researchers).
narrative_ontology:constraint_victim(ai_safety_commitment__near_term_harms_reading, misinformation_targets).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, responsible_ai_governance).
narrative_ontology:constraint_vindicates(ai_safety_commitment__near_term_harms_reading, stakeholder_accountability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy machine learning systems across products (recommendation, hiring, credit, moderation). Define what 'safety' means internally, conduct audits on their own terms, and set deployment standards. Benefit from avoiding heavier regulation by demonstrating compliance with self-authored safety commitments. Can shift auditing standards and transparency thresholds based on business constraints. Exit: moving deployment to jurisdictions with lighter governance.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, tech_companies, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__near_term_harms_reading, tech_companies, beneficiary).

% Subject to algorithmic decisions (credit denial, hiring rejection, content moderation removal) without transparency into the decision logic. Bear the harms of bias and discrimination but have no seat at the table where 'safety' is defined. Cannot opt out of algorithmic systems that are now embedded in essential services. Exit: none meaningful — they cannot avoid the systems that govern access to credit, employment, housing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, marginalized_populations, payer,
    powerless, biographical, trapped, global).

% Subject to algorithmic management systems (task allocation, rating/deactivation) that enforce behavioral compliance. Wage and employment stability depend on black-box scoring systems. Safety commitments around labor exploitation are routinely decoupled from actual working conditions. Can unionize or seek legislative intervention, but platforms can relocate or change their algorithms faster than regulation catches up.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, gig_workers, payer,
    organized, immediate, constrained, global).

% Disproportionately harmed by bias in hiring, lending, criminal-risk assessment, and content moderation algorithms trained on data reflecting historical discrimination. Experience material harms (denied jobs, inflated loan rates, unjust criminal recommendations, content suppression) without recourse to challenge or explain the decisions. Exit: difficult — cannot easily avoid platforms or services that use these systems for access to economic opportunity.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, algorithmic_discrimination_targets, payer,
    moderate, biographical, constrained, global).

% Define the research agenda for AI safety through publications, grant-making, and influence over institutional priorities. Near-term safety focus creates funding streams, paper-publication venues, and career advancement paths focused on measurable harms. Can choose research direction; if near-term focus wanes, can reorient. Benefit from legitimacy and resources that accompany 'responsible AI' framing.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, safety_researchers, beneficiary,
    powerful, generational, mobile, global).

% Charged with protecting citizens from algorithmic harms. Write standards, audit compliance, levy penalties. Constrained by industry expertise asymmetry: companies understand their systems better than regulators do. Often adopt the near-term safety frame to justify interventions that tech companies have already voluntarily committed to (regulatory capture dynamic). Exit: none — they are accountable to the public.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, regulators_and_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Harmed by algorithmic amplification of false or misleading content (health misinformation, election disinformation, conspiracy theories). Often already embedded in communities and identities that make them trust sources amplified by algorithms. Cannot easily verify information or exit the platforms that distribute it. Exit: would require rejecting the social networks or information ecosystems they depend on.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, misinformation_targets, payer,
    powerless, immediate, identity_locked, global).

% Research long-term alignment and existential safety of superintelligent systems. Structurally sidelined by the near-term harms frame: funding, publication venues, and institutional legitimacy flow to near-term auditing and bias detection rather than theoretical alignment work. Would argue that present-day bias is a distraction from the much larger existential risk; their research agenda competes for the same institutional and financial resources. They are excluded not by explicit rule but by the resource-allocation structure that the near-term frame sustains.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, existential_risk_researchers, excluded,
    powerful, civilizational, mobile, global).

% Communities most affected by algorithmic discrimination and misinformation (low-income neighborhoods, minority communities, rural areas) rarely have seats in safety governance conversations. Their knowledge of harms is lived experience; it enters safety discourse only through academic research or advocacy organizations, not as direct input into company or regulatory safety definitions.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__near_term_harms_reading, affected_communities, observer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__near_term_harms_reading, tech_companies).
narrative_ontology:fixing_cost_class(ai_safety_commitment__near_term_harms_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared commitment to prevent documented, present-day algorithmic harms (bias, discrimination, labor exploitation, misinformation spread) through transparency, auditing, and harm-mitigation standards. Solves a coordination problem: companies want to avoid reputational damage and regulation; societies want protection; safety researchers want legitimacy and funding. The frame unifies these interests by making 'safety' synonymous with 'preventing measurable present harms.'
% TRANSFER_FUNCTION: Moves accountability and governance authority from regulatory bodies and affected communities to tech companies and safety researchers. Companies gain the power to define what counts as a 'documented harm,' which audits matter, what transparency is required, and what remediation is sufficient. Safety researchers gain funding, publication authority, and institutional legitimacy. Affected populations lose voice in the safety definition process and have no recourse when the audits are conducted on company terms.
% ABSENT_VOICES: Existential risk researchers who argue the focus on present-day harms distracts from larger extinction-level alignment problems; affected communities from marginalized populations whose lived experience of algorithmic discrimination is not systematically incorporated into safety design; labor organizers arguing that 'safety' should center worker agency and collective bargaining power over algorithmic management, not just compliance audits.
% DISAPPEARANCE_RATIONALE: If this commitment vanished, deployment practices would shift dramatically: companies would reduce transparency obligations, safety auditing would decline, and affected populations would lose even the minimal recourse they now have through public pressure and regulatory threat. Regulatory frameworks built on this commitment (EU AI Act harm-mitigation provisions, SEC disclosure rules, jurisdictional algorithmic accountability laws) would collapse or require wholesale redesign. The constraint structures the entire landscape of AI governance; its disappearance would reorganize incentives and power flows.
% FOUNDING_PROBLEM: Deployed AI systems were causing measurable, documented harms to real people: biased hiring algorithms rejecting qualified candidates, loan-approval systems systematizing discrimination, content moderation systems amplifying misinformation, gig-work algorithms exploiting workers. These harms were happening in the present, to identifiable populations, and demanded urgent mitigation.
% FOUNDING_PROBLEM_CORROBORATION: Independent researchers (ProPublica, Algorithmic Justice League, Partnership on AI), affected community advocates, labor organizers, and regulators from multiple jurisdictions have documented the harms. The founding problem is corroborated by independent scholarship and investigative reporting outside the benefiting parties. Tech companies themselves acknowledge some forms of bias exist; the contest is not whether the problem is live but whose definition of 'safety' governs the response.
narrative_ontology:disappearance_verdict(ai_safety_commitment__near_term_harms_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__near_term_harms_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__near_term_harms_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__near_term_harms_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__near_term_harms_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__near_term_harms_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.48 → 0.68 over interval) because the near-term framing allows companies to control the definition of 'safety' and 'harm,' limiting audits to easily managed metrics (gender bias in hiring, racial disparities in content moderation) while excluding harder-to-measure harms (exploitation through opaque algorithmic management, epistemic injustice in who gets to define harm). Theater is substantial and rising (0.22 → 0.41) because the constraint is increasingly operated through compliance theater: companies conduct audits on their own systems, publish sanitized findings, and declare themselves 'safe' by standards they authored. The framework legitimizes the appearance of safety governance while devolving actual authority to companies. Suppression is high (0.72) because the constraint works by excluding alternative framings: existential risk researchers are sidelined (excluded from funding flows and publication authority), affected communities are kept out of governance conversations (their voice enters only through mediation), and labor organizers' demands for agency over algorithmic management are reframed as 'safety' problems to be solved through auditing rather than structural worker power. The measurement series shows extraction and theater increasing as the framework institutionalizes (regulatory adoption, funding consolidation around near-term research, company compliance machinery maturation) and suppression holding steady (the exclusions are structural, not time-dependent).
 *
 * PERSPECTIVAL GAP:
 *   From the tech-company and safety-researcher seats, the near-term harms frame is genuine coordination: it addresses real problems (algorithmic bias, misinformation), provides legitimate governance structure (audits, transparency), and creates accountability. From the marginalized-population seats, the same structure is extractive: companies define 'harm' to exclude the harms most consequential to them (labor exploitation, epistemological exclusion), audits are conducted on company terms, and populations remain powerless. Regulators occupy a middle position: they are agenda-setters but increasingly captured by the companies they regulate, so their framing of 'safety' drifts toward the tech-company definition over time. Existential risk researchers, excluded from the conversation, see the near-term frame as diverting resources and attention from what they believe is the larger and more important problem (superintelligent misalignment). The engine computes these divergences from the structural data: beneficiary/victim declarations (low d for companies, high d for marginalized populations, medium d for workers and researchers), exit options (trapped for marginalized populations, arbitrage for companies, mobile for researchers), and power differentials (institutional for companies and regulators, powerless for marginalized populations, powerful-but-excluded for existential researchers). The claim (tangled_rope) and the metrics (high extractiveness, high suppression, rising theater) together capture the structure: genuine coordination function (preventing real harms) married to asymmetric extraction (companies control definitions and maintain authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Tech companies compute as beneficiaries/agenda-setters (d near 0.0) because they control the definition of safety and auditing standards, and can shift them based on business constraints; they gain regulatory credibility without ceding substantive control. Marginalized populations compute as targets (d near 1.0) because they are the populations most harmed but have trapped or identity-locked exit: algorithmic systems are embedded in essential services, and they have no recourse within the safety governance structure. Gig workers sit at d~0.75: they are payers (subject to algorithmic management) with some organized power but constrained exit (their employment depends on the platforms). Safety researchers compute as beneficiaries (d near 0.1): they have mobile exit, powerful institutional position, and gain resources from the framing. Regulators compute as near-symmetric (d~0.5): they are agenda-setters but constrained by industry expertise asymmetry and political pressure; they benefit from the near-term frame as a way to justify intervention that companies have already endorsed, but they are not the primary extractors. Existential risk researchers are excluded and would compute very differently from an excluded position: they would see the near-term frame as extracting from their research agenda and legitimacy, but they are structurally outside the constraint's beneficiary/victim system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (documented harms exist and continue), but the constraint itself has become partially divorced from solving it. Mandatrophy is incipient: the constraint is increasingly operated as theater (companies conduct 'safety' audits, declare themselves safe, then continue practices that marginalized populations identify as harmful). The near-term frame is preserved not because it is uniquely effective at reducing harms but because it allocates authority to institutions (companies, safety researchers, regulators) rather than to the populations harmed. If the commitment to near-term harm reduction were actually enforced (i.e., if marginalized populations had real voice in defining 'harm' and real power to demand remediation), the extractiveness would collapse and the constraint would become genuinely cooperative rope. That does not happen because the constraint is structured to exclude that voice. The theater_ratio rising (0.22 → 0.41) is the signal of mandatrophy in motion: the appearance of governance without the substance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_contestation,
    'Who has legitimate authority to define what counts as an ''AI harm''? Is it the companies deploying the systems, the affected populations experiencing them, regulatory bodies, or some hybrid?',
    'Empirical: participatory design processes where affected communities co-author safety standards and audit criteria; compare outcomes to current company-authored standards. Conceptual: frameworks for legitimate governance (democratic legitimacy, epistemic justice, stakeholder representation) applied to AI safety institutions.',
    'If affected populations had real authority in harm definition, extractiveness would drop sharply (companies would not be able to exclude labor exploitation or epistemological exclusion from ''safety''). If definition authority remains concentrated in companies and safety researchers, extractiveness remains high and theater persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_definition_contestation, conceptual, 'Legitimacy of harm definition in AI safety governance').

omega_variable(
    existential_vs_near_term_tradeoff,
    'Is the focus on near-term documented harms structurally compatible with addressing existential risks from superintelligent misalignment, or are they genuinely competing for scarce institutional resources and attention?',
    'Empirical: time-series funding data for near-term safety research vs. alignment research; resource allocation decisions at major institutions. Conceptual: analysis of whether the two frames can coherently coexist within a single institution or whether institutional commitments to one necessarily marginalize the other.',
    'If the competition is genuine and irreducible, then the near-term frame is a choice to prioritize one set of risks over another (and exclude the other researchers from authority). If compatible, the exclusion of existential researchers is a contingent institutional choice, not structural to the near-term reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_vs_near_term_tradeoff, conceptual, 'Resource competition between near-term and existential AI safety research').

omega_variable(
    marginalized_population_identity_lock,
    'For marginalized populations subject to algorithmic harms, how much of their ''trapped'' exit status is structural (no alternative systems exist) vs. identity-locked (they have fused identity/community attachment to the systems even when alternatives exist)?',
    'Post-exit trajectory analysis: if algorithmic-free alternatives became available, would affected populations adopt them? Identity-lock would persist even after structural removal. Qualitative research into why people stay on systems they report as harming them.',
    'If identity-locked, the suppression is internalized and persists even after structural removal. The constraint''s extractiveness includes this internalized suppression cost. If purely structural, removing the constraint would fully restore agency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginalized_population_identity_lock, empirical, 'Structural vs. internalized suppression in algorithmic system participation').

omega_variable(
    reader_kernel_distinction,
    'Is this reading of AI safety a legitimate alternative frame grounded in different axioms (deontological priority on present harm), or is it a reading that other parties are forced into by the constraint''s exclusion structure?',
    'Genealogical: trace the origins of the near-term frame. Does it emerge from affected communities'' own priority-setting, or is it articulated by external advocates and researchers on their behalf? Who funds and legitimizes the frame?',
    'If organically developed from affected communities'' own knowledge, it is a genuine alternative reading of the kernel. If imposed by external advocates or researchers seeking institutional legitimacy, it is partially an artifact of the constraint itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_kernel_distinction, conceptual, 'Authenticity of the near-term reading as a community-grounded vs. imposed frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__near_term_harms_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__near_term_harms_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ai_s_tr_t0, observed).
narrative_ontology:measurement(ai_s_tr_t3, ai_safety_commitment__near_term_harms_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(ai_s_tr_t3, observed).
narrative_ontology:measurement(ai_s_tr_t6, ai_safety_commitment__near_term_harms_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement_basis(ai_s_tr_t6, observed).
narrative_ontology:measurement(ai_s_tr_t9, ai_safety_commitment__near_term_harms_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement_basis(ai_s_tr_t9, observed).
narrative_ontology:measurement(ai_s_tr_t12, ai_safety_commitment__near_term_harms_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(ai_s_tr_t12, observed).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__near_term_harms_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(ai_s_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_s_be_t0, observed).
narrative_ontology:measurement(ai_s_be_t3, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement_basis(ai_s_be_t3, observed).
narrative_ontology:measurement(ai_s_be_t6, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(ai_s_be_t6, observed).
narrative_ontology:measurement(ai_s_be_t9, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 9, 0.65).
narrative_ontology:measurement_basis(ai_s_be_t9, observed).
narrative_ontology:measurement(ai_s_be_t12, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(ai_s_be_t12, observed).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__near_term_harms_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(ai_s_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(ai_s_su_t0, observed).
narrative_ontology:measurement(ai_s_su_t3, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement_basis(ai_s_su_t3, observed).
narrative_ontology:measurement(ai_s_su_t6, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(ai_s_su_t6, observed).
narrative_ontology:measurement(ai_s_su_t9, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 9, 0.7).
narrative_ontology:measurement_basis(ai_s_su_t9, observed).
narrative_ontology:measurement(ai_s_su_t12, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(ai_s_su_t12, observed).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__near_term_harms_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(ai_s_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__near_term_harms_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__near_term_harms_reading, 0.12).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__near_term_harms_reading, ai_safety_commitment__dual_priority_reading).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel constrains three related but distinct constraint stories: near_term_harms_reading (present-day documented harms to marginalized populations), existential_risk_reading (extinction-level outcomes from superintelligent misalignment), and dual_priority_reading (both simultaneously). Each reading instantiates a different ε, beneficiary/victim structure, and suppression mechanism. The near-term reading structures governance around present harm mitigation (high ε on transparency, auditing, labor standards). The existential reading structures governance around alignment research freedoms (low ε on present-harm oversight, high ε on research secrecy). The dual reading attempts to hold both, creating structural tension. Decomposition is necessary because ε is reading-indexed: the same kernel (AI safety) generates different extractiveness values depending on which harms the reading prioritizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__near_term_harms_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
