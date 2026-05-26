% ============================================================================
% CONSTRAINT STORY: near_term_harms_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_near_term_harms_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: near_term_harms_reading
 *   human_readable: Near-Term AI Harms Governance Priority Reading
 *   domain: ai_governance/technology_ethics/risk_prioritization
 *
 * SUMMARY:
 *   The near-term harms governance reading prioritizes mitigating
 *   demonstrable present harms from AI systems—algorithmic bias, labor
 *   displacement, surveillance, misinformation—affecting marginalized
 *   populations now. This reading instantiates one commitment within the
 *   contested kernel of AI risk governance priority. The constraint exhibits
 *   tangled_rope structure: it solves a genuine coordination problem
 *   (companies and civil society aligning on bias mitigation, transparency
 *   standards, fairness audits) while simultaneously creating extraction
 *   asymmetry. Technology companies benefit from the attention-shift away
 *   from capability control and long-term concentration risks. Workers and
 *   Global South populations experience both genuine protection (from
 *   measurable algorithmic harms) and ongoing suppression (trapped in systems
 *   where marginal harm reduction through audits does not address fundamental
 *   asymmetries). The extractiveness has increased over the interval (0.35 to
 *   0.62) as the reading has consolidated institutional dominance, reducing
 *   space for competing prioritization frameworks. Theater has also risen
 *   (0.32 to 0.55) as bias mitigation has become compliance-driven rather
 *   than outcome-focused: companies perform fairness audits as institutional
 *   legitimacy signals rather than as mechanisms for measurable harm
 *   reduction.
 *
 * KEY AGENTS:
 *   - Technology Companies: Primary beneficiary (institutional/arbitrage) — gain regulatory leniency and reduced pressure on capability control by appearing to solve near-term harms; extract benefit from attention-diversion
 *   - Marginalized Workers & Global South Populations: Primary victim (powerless/trapped) — trapped in algorithmic systems where bias mitigation is marginal; bear suppression costs of deployment without meaningful exit
 *   - AI Fairness/Rights Advocacy Coalition: Secondary beneficiary (organized/constrained) — benefit from institutional legitimacy and funding flowing to near-term harm mitigation; genuine interest in reducing algorithmic discrimination
 *   - Existential Risk Research Community: Secondary victim (institutional/mobile) — institutional degradation as x-risk research loses policy voice and funding share; piton classification captures the atrophy of long-term risk identification function
 *   - Regulatory & Legislative Institutions: Enforcement actors (organized/constrained) — create compliance frameworks that serve coordination function for near-term harms but are designed with sunset clauses as technology matures
 *   - Capability Accelerationists: Indirect beneficiary (powerful/arbitrage) — benefit from governance focus on near-term harms that diverts attention from capability control and concentration risks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(near_term_harms_reading, 0.58).
domain_priors:suppression_score(near_term_harms_reading, 0.65).
domain_priors:theater_ratio(near_term_harms_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(near_term_harms_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(near_term_harms_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(near_term_harms_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(near_term_harms_reading, tangled_rope).
narrative_ontology:human_readable(near_term_harms_reading, "Near-Term AI Harms Governance Priority Reading").
narrative_ontology:topic_domain(near_term_harms_reading, "ai_governance/technology_ethics/risk_prioritization").

domain_priors:requires_active_enforcement(near_term_harms_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(near_term_harms_reading, distributed).
narrative_ontology:cs_authority_grounding(near_term_harms_reading, distributed).
narrative_ontology:cs_kernel_id(near_term_harms_reading, ai_risk_governance_priority).
narrative_ontology:cs_reading_relation(near_term_harms_reading, existential_risk_reading, influences).
narrative_ontology:cs_reading_relation(near_term_harms_reading, bridge_reading, coexists_with).
narrative_ontology:cs_axiom(near_term_harms_reading, foundational, present_algorithmic_harm_moral_urgency).
narrative_ontology:cs_axiom_status(present_algorithmic_harm_moral_urgency, holdable).
narrative_ontology:cs_axiom_grounding(near_term_harms_reading, present_algorithmic_harm_moral_urgency, deontological).
narrative_ontology:cs_axiom(near_term_harms_reading, foundational, governance_resource_singularity).
narrative_ontology:cs_axiom_status(governance_resource_singularity, holdable).
narrative_ontology:cs_axiom_grounding(near_term_harms_reading, governance_resource_singularity, instrumental).
narrative_ontology:cs_reference_frame(near_term_harms_reading, current_algorithmic_deployment_regime).
narrative_ontology:cs_drift_state(near_term_harms_reading, contemporary_ai_governance_consolidation, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(near_term_harms_reading, technology_companies).
narrative_ontology:constraint_beneficiary(near_term_harms_reading, ai_capability_accelerationists).
narrative_ontology:constraint_victim(near_term_harms_reading, global_south_populations).
narrative_ontology:constraint_victim(near_term_harms_reading, marginalized_workers).
narrative_ontology:constraint_victim(near_term_harms_reading, algorithmic_discrimination_targets).
narrative_ontology:constraint_victim(near_term_harms_reading, surveillance_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMIC DISCRIMINATION TARGET (SNARE) — Trapped in systems of bias with no exit option. Credit scoring, hiring algorithms, loan approval systems, and policing AI generate extractive asymmetry: data is extracted from these populations, models trained on biased data harm them, and they bear the cost of algorithmic failure. No mechanism to exit or appeal. Maximum extraction, maximum suppression.
constraint_indexing:constraint_classification(near_term_harms_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DISPLACED MANUFACTURING WORKER (SNARE) — Faces labor displacement from automation with constrained alternatives. Retraining barriers (cost, time, geographic mobility), social safety nets inadequate, and skill depreciation rapid. Suppression high — worker cannot easily exit the labor market disruption. Extraction is real (redistribution of value to capital owners) but workers retain some labor market mobility if they relocate or retrain. Near-snare territory: borders on tangled_rope only if reskilling programs provide genuine viable alternatives.
constraint_indexing:constraint_classification(near_term_harms_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY COMPANIES (TANGLED ROPE) — Face genuine coordination problem: deploying models without bias audits creates regulatory, reputational, and legal liability. Bias mitigation, fairness audits, and transparency frameworks solve this coordination challenge. BUT these same companies benefit from the near-term harms reading's dominance in governance discourse because it diverts attention from longer-term concentration risks and capability control. Extraction: maintaining opaque systems longer than necessary while appearing to address bias. Coordination: solving real fairness problems enables faster, more trusted deployment. Mixed: high coordination benefit + moderate extraction of regulatory leniency.
constraint_indexing:constraint_classification(near_term_harms_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI RIGHTS & FAIRNESS ADVOCACY COALITION (ROPE) — Organized groups (civil rights organizations, worker advocacy, algorithmic justice researchers) see the near-term harms governance priority as a coordination solution: focusing resources on demonstrable present harms enables coalition formation, regulatory progress, and immediate damage mitigation. Benefits from this framing by gaining funding, policy leverage, and institutional legitimacy. Minimal extraction — the constraint coordinates genuine shared interests (reducing algorithmic harm to marginalized populations). Theater low because the harm is real and measurable.
constraint_indexing:constraint_classification(near_term_harms_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: X-RISK RESEARCH COMMUNITY (PITON) — Once-functional community focused on long-term AI safety research is experiencing institutional degradation. As near-term harms governance dominates institutional resources and moral attention, the x-risk research infrastructure persists through inertia and traditional funding (academic positions, OpenPhil grants) but loses cultural authority and political voice. The constraint does not eliminate x-risk research, but marginalizes it within governance discourse. Theater high: the x-risk community maintains its institutional structures and publication outlets, but its core analytical function (identifying long-term structural risks) has atrophied relative to near-term fairness audits. Piton diagnosis: former coordination function (identifying systemic AI risks) now largely performative within governance frameworks dominated by near-term harm mitigation.
constraint_indexing:constraint_classification(near_term_harms_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY FRAMEWORKS (SCAFFOLD) — Near-term harms governance provides the structural legitimacy for creating regulatory requirements (algorithmic impact assessments, bias audits, transparency mandates, worker retraining funds). These frameworks are inherently temporary: as technical standards mature and industry practices evolve, the regulatory overhead declines. Sunset logic embedded: early-stage regulation is theater-heavy (proves intent) but time-limited (specific technological circumstances trigger eventual deregulation or normalization). Beneficiaries (regulated companies) and victims (marginalized populations) both experience this as coordination burden with declining suppression over time.
constraint_indexing:constraint_classification(near_term_harms_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - TECHNOLOGICAL DETERMINISM (MOUNTAIN) — From a universal/civilizational frame, AI capability advancement and deployment follows technological logic independent of governance priority. Marginal harm mitigation via fairness interventions does not change the underlying capability trajectory. The near-term harms governance frame is seen as addressing surface phenomena (bias in specific models) rather than structural drivers (capability scaling, competitive deployment pressure, misalignment between training objectives and human values). This perspective treats the governance priority choice as epiphenomenal to technological determinism. However, the presence of identifiable beneficiaries (technology companies, accelerationists) contradicts the mountain classification — the engine will flag this as a false summit.
constraint_indexing:constraint_classification(near_term_harms_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(near_term_harms_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(near_term_harms_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(near_term_harms_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(near_term_harms_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(near_term_harms_reading, TR),
    TR >= 0.70.

:- end_tests(near_term_harms_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint generates significant extraction through resource reallocation and attention-diversion, but it is not maximal snare territory (would require ε ≥ 0.66) because genuine coordination and harm mitigation occur alongside extraction. The beneficiary set (technology companies, accelerationists) gains direct benefit from governance prioritization that constrains long-term risk frameworks. Victims bear measurable suppression (trapped in biased systems with no exit, constrained labor mobility) but also receive material benefit from bias audits and fairness frameworks. The extractiveness has increased over the 10-year interval as the reading has consolidated institutional acceptance, reducing the salience of competing frameworks. Suppression (0.65): Moderate-high. Victims face structural barriers: marginalized populations cannot opt out of algorithmic systems; workers cannot easily exit labor market disruption; long-term risk researchers face institutional marginalization. Suppression is active (enforced through bureaucratic compliance, institutional funding allocation) but not total — advocacy coalitions can mobilize, regulatory pressure exists, and some technological alternatives remain available. Theater (0.48): Moderate, rising. Early-stage governance focused on measurable harm reduction (identifying bias, mandating audits) has low theater. As frameworks mature, compliance becomes ritualistic: companies conduct required fairness assessments as boxes to check rather than as mechanisms for eliminating discrimination. Theater increases because the institutional requirement (bias audit) becomes decoupled from outcome (actual harm reduction). The near-term reading generates moderate theater because the harms it addresses are real, but the interventions increasingly become compliance theater rather than substantive mitigation.
 *
 * PERSPECTIVAL GAP:
 *   The reading exhibits maximal perspectival divergence. Marginalized populations and displaced workers perceive snare: trapped in systems where bias mitigation is marginal and suppression total. Technology companies and accelerationists perceive tangled_rope or rope: they face genuine coordination challenges (regulatory compliance, public trust) and benefit from the governance framework's focus. The fairness advocacy coalition perceives rope: genuine coordination with beneficial outcome. Existential risk researchers perceive piton: a once-functional research community has degraded as institutional authority has shifted. Regulatory institutions perceive scaffold: temporary coordination framework with declining theater as standards mature. The analytical observer risks perceiving mountain: seeing near-term harms governance as either epiphenomenal to technological determinism or as inherent to deployment ethics. The presence of clear beneficiaries (technology companies gaining from attention-diversion) exposes this mountain as a false summit. The perspectival gap reveals that the reading's institutional dominance is not justified by structural equivalence across all positions—it is justified by the power of beneficiaries to shape governance discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Technology companies (institutional/arbitrage) occupy low d positions (~0.15): they are beneficiaries with exit capacity. Applied sigmoid f(d) produces near-zero or negative effective extraction from their perspective—they experience the constraint as coordination, not extraction. Marginalized populations (powerless/trapped) occupy high d positions (~0.95): maximum extraction target with zero exit capacity. Sigmoid f(d) produces maximum experienced extraction (~1.42). Displaced workers (moderate/constrained) occupy moderate-high d positions (~0.70): constrained exit, victim status produces f(d) ~1.0, moderate experienced extraction. The fairness advocacy coalition (organized/constrained) occupies moderate d positions (~0.50): both beneficiary and victim alignment, mixed exit options, produces f(d) ~0.65 and moderate experienced extraction offset by coordination benefits. X-risk researchers (institutional/mobile) occupy moderate-high d (~0.75): institutional victim (marginalized) but mobile (can relocate to industry or other research), produces f(d) ~1.05. Scope is global (σ=1.2) across all perspectives except regulatory frameworks (national, σ=1.0). Scope amplification increases experienced chi for all agents by 20%.
 *
 * MANDATROPHY ANALYSIS:
 *   The near-term harms reading exhibits mandatrophy structure: does prioritizing demonstrable harms coordinate genuine protection for vulnerable populations, or does it extract by diverting governance attention from structural capability risks? The classification (tangled_rope) resolves the tension by acknowledging both elements: the constraint solves real coordination problems (companies and civil society aligning on fairness standards) AND creates extraction (resources diverted from long-term risk frameworks, technology companies positioned as 'solution providers' rather than concentrated power). The mandatrophy is not resolved by choosing one interpretation—it is resolved by accepting that the constraint genuinely coordinates harm mitigation for some populations while extracting governance leniency for technology companies. This dual structure is not a classification error; it is the actual structural reality. The constraint is neither pure coordination (rope) nor pure extraction (snare) because the extraction mechanism depends on the diversion of attention to longer-term frameworks that exist outside this constraint's scope. Within-constraint analysis produces tangled_rope; cross-constraint analysis (network effects on existential risk governance) reveals the snare dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_near_term_threshold,
    'What temporal threshold operationally distinguishes ''near-term harms'' from ''long-term risks''? Is 5 years the boundary? 10 years? End of current deployment generation?',
    'Examination of governance documents, regulatory frameworks, and policy statements to extract implicit or explicit temporal cutoffs; longitudinal tracking of what counts as ''present harm'' vs ''future risk'' as technology evolves',
    'If threshold is short (< 3 years): only immediately observable failures count as governance priority, allowing medium-term structural risks (concentration, capability control) to accumulate unchecked. If threshold is long (> 15 years): governance attention diffuses across present and speculative harms, reducing resource focus on measurable discrimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_near_term_threshold, conceptual, 'Operational definition of near-term vs long-term in AI governance').

omega_variable(
    marginal_harm_reduction_efficacy,
    'Do bias audit requirements, fairness interventions, and transparency mandates measurably reduce algorithmic discrimination in deployed systems, or do they primarily shift harm distributions and create compliance theater?',
    'Comparative analysis of discrimination metrics before/after regulatory intervention in specific domains (credit scoring, hiring, criminal justice); controlled studies separating genuine harm reduction from displacement or obfuscation',
    'If genuinely effective: near-term harms governance is functional coordination, justifying continued resource priority. If largely theater: the reading instantiates a snare through regulatory compliance capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(marginal_harm_reduction_efficacy, empirical, 'Efficacy of bias mitigation interventions in reducing algorithmic discrimination').

omega_variable(
    resource_displacement_kernel_reading,
    'Does prioritizing near-term harms governance in institutional and policy attention systematically displace resources and research attention from existential/long-term capability control risks, or do these operate in separate institutional channels?',
    'Mapping of funding flows (government, philanthropy, academic) to near-term harm work vs long-term safety research; interviews with researchers about institutional pressure and opportunity costs; bibliometric analysis of publication trends',
    'If high displacement: near-term harms reading instantiates an extraction mechanism where focusing on measurable harms diverts governance from structural capability risks, indirectly benefiting technology companies. If low displacement: the readings operate in parallel institutions and the snare dynamic is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_displacement_kernel_reading, empirical, 'Whether near-term harms governance displaces long-term capability control resources').

omega_variable(
    capability_accelerationist_alignment,
    'Are technology companies and capability accelerationists genuinely beneficiaries of near-term harms governance prioritization, or does this reading impose genuine coordination costs that offset extraction benefits?',
    'Analysis of corporate positioning on bias audits and fairness requirements vs positions on capability control and AI governance scope; tracking of resource allocation to compliance vs acceleration',
    'If genuinely beneficiary: validates tangled_rope classification and high beneficiary alignment. If costs are high: reading is less extractive, moving toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_accelerationist_alignment, empirical, 'Whether near-term harms governance benefits technology companies and accelerationists').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is this reading (near-term harms priority) a genuinely held normative commitment grounded in moral urgency for vulnerable populations, or is it a post-hoc rationalization for resource distribution that benefits capability acceleration?',
    'Historical analysis of how this reading emerged in discourse; interviews with advocates; tracking of institutional origins and funding patterns; logical examination of whether the reading''s core axioms are independently justified or instrumentally derived',
    'If genuinely foundational commitment: reading is holdable and defensible within AI ethics traditions. If instrumentally derived: reading is vulnerable to foreclosure once the motivation becomes transparent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether near-term harms reading is foundational or instrumental commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(near_term_harms_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(near_term_theater_t0, near_term_harms_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(near_term_theater_t3, near_term_harms_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(near_term_theater_t6, near_term_harms_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(near_term_theater_t9, near_term_harms_reading, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(near_term_extractiveness_t0, near_term_harms_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(near_term_extractiveness_t3, near_term_harms_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(near_term_extractiveness_t6, near_term_harms_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(near_term_extractiveness_t9, near_term_harms_reading, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(near_term_harms_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(near_term_harms_reading, 0.2).
narrative_ontology:affects_constraint(near_term_harms_reading, existential_risk_reading).
narrative_ontology:affects_constraint(near_term_harms_reading, bridge_reading).
narrative_ontology:affects_constraint(near_term_harms_reading, ai_capability_acceleration).
narrative_ontology:affects_constraint(near_term_harms_reading, algorithmic_bias_disparity_amplification).
narrative_ontology:affects_constraint(near_term_harms_reading, labor_displacement_suppression).

% DUAL FORMULATION NOTE:
% The near-term harms reading decomposes from the contested kernel ai_risk_governance_priority. Its sibling readings (existential_risk_reading, bridge_reading) instantiate alternative commitments to the same kernel. The family structure reflects not a natural decomposition via ε-invariance but a genuine conceptual dispute: different parties read the kernel (the fundamental question of AI governance priority) differently. Each reading has its own ε, its own beneficiary/victim structure, and its own perspectives. Cross-reading effects are documented in network.affects_constraints: the near-term harms reading increases suppression on existential-risk research (piton perspective) and influences technology company positioning on capability control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(near_term_harms_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
