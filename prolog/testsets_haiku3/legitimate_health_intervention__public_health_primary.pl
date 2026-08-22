% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Legitimacy via Population Morbidity Reduction (Public Health Primary Reading)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   Under the public health primary reading, legitimacy of health
 *   interventions (vaccination mandates, treatment requirements, access
 *   restrictions) derives entirely from measured reduction in
 *   population-level morbidity and mortality. Individual refusal to comply is
 *   reframed as an externality — an imposition of disease transmission risk,
 *   hospitalization burden, and mortality on immunocompromised and vulnerable
 *   populations. Unvaccinated individuals enter the victim set as disease
 *   vectors whose refusal imposes costs on others. The enforcement mechanisms
 *   (employment termination, facility access restrictions, travel barriers,
 *   social exclusion) are justified as necessary to contain the externality.
 *   This reading is ONE instantiation of the contested kernel
 *   'legitimate_health_intervention'; sibling readings
 *   (bodily_autonomy_primary and proportionality_reading) reject or
 *   significantly reweight these claims.
 *
 * KEY AGENTS:
 *   - Public health authorities: agenda_setter, institutional power, generational time horizon — set and enforce population-level mandates; justify restrictions by appeal to measured morbidity/mortality reduction; control the epidemiological data that define externality magnitude.
 *   - Immunocompromised populations: beneficiary, powerless to organized power atoms, biographical to generational time horizons — receive protection from disease exposure; constrained exit (cannot leave society to avoid unvaccinated individuals); treatment access tied to compliance enforcement.
 *   - Vaccine-hesitant individuals: payer (in the victim set), moderate to powerless power atoms, biographical time horizon — bear employment termination, access restrictions, social exclusion; exit options range from identity_locked (refuse vaccination on principle) to constrained (require exemptions, testing, accommodation).
 *   - Medical exemption claimants: payer (in the victim set), powerless to moderate power atoms, biographical time horizon — face scrutiny of exemption claims; suppression via medical gatekeeping and institutional skepticism.
 *   - Unvaccinated disease vectors (framing from this reading): payer (in the victim set AND as disease sources), powerless to moderate power atoms, biographical time horizon — externality bearers in the public health account; their refusal constitutes harm imposition on vulnerable populations.
 *   - Healthcare system: beneficiary (reduced patient load, managed capacity) and payer (enforcement overhead), institutional power, generational time horizon — administers access restrictions and employment coordination.
 *   - Proportionality advocates: excluded (role=excluded), observer, analytical power — would argue for risk stratification, proportional intervention, and cost-benefit weighting; structurally outside the decision-making process in public_health_primary framing.
 *   - Bodily autonomy advocates: excluded (role=excluded), observer, analytical power — would argue refusal is a fundamental right regardless of externality magnitude; their voice is not in the room where this reading operates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.71).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Legitimacy via Population Morbidity Reduction (Public Health Primary Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, 'b4b2309f-7dc3-4b38-a88c-afec2ee27ff7').
narrative_ontology:cs_kernel_codification('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', formalized).
narrative_ontology:cs_authority_grounding('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', extraction).
narrative_ontology:cs_interpretation_layer_present('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7').
narrative_ontology:cs_reading_relation('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', legitimate_health_intervention__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', foundational, population_welfare_supremacy).
narrative_ontology:cs_axiom_status(population_welfare_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', population_welfare_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', foundational, externality_imposition_as_harm).
narrative_ontology:cs_axiom_status(externality_imposition_as_harm, holdable).
narrative_ontology:cs_axiom_grounding('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', externality_imposition_as_harm, deontological).
narrative_ontology:cs_reference_frame('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', pandemic_emergency_medical_necessity).
narrative_ontology:cs_drift_state('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', endemic_disease_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b4b2309f-7dc3-4b38-a88c-afec2ee27ff7', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, vulnerable_age_cohorts).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, unvaccinated_disease_vectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, medical_exemption_claimants).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, employment_terminated_unvaccinated).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_providers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set population-level morbidity/mortality targets; determine which interventions (vaccination mandates, access restrictions, treatment requirements) are necessary to achieve them; control epidemiological data and threat-level assessments; enforce compliance through employment, access, and regulatory mechanisms. Justify restrictions by appeal to public health benefit. Face sustained political pressure and legal challenges but retain institutional authority and resource control.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Depend on others' compliance with health interventions for their own safety; cannot exit society to avoid unvaccinated individuals; carry biological vulnerability that makes disease transmission lethal or severely harmful. Benefit from population-level compliance via reduced transmission risk. Suppression burden is indirect (social costs of enforcement felt through healthcare rationing, access delays, institutional tensions).
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, national).

% Refuse vaccination on grounds of bodily autonomy, medical skepticism, or ideological conviction. Subject to employment termination, facility access restrictions (healthcare, education, travel), social exclusion, and regulatory barriers. Exit options are severely constrained: refusing vaccination on principle makes exit identity-locked (the refusal is central to their self-conception); material exit (relocating to permissive jurisdictions) is expensive and often unavailable; legal exit (exemption claims) faces institutional skepticism. Bear direct suppression costs.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_hesitant_individuals, payer,
    powerless, biographical, identity_locked, national).

% Seek medical exemptions to mandatory interventions on grounds of genuine medical contraindications or prior adverse events. Face heightened institutional skepticism, gatekeeping by healthcare providers operating under enforcement pressure, and burden-of-proof requirements that shift decision authority to public health officials rather than treating physicians. Constrained exit: must navigate bureaucratic exemption processes; cannot simply refuse as a matter of principle.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, medical_exemption_claimants, payer,
    moderate, biographical, constrained, national).

% Benefit from reduced patient load (if population compliance reduces disease incidence) and from coordination of care around mandated interventions. Pay enforcement overhead costs: time spent administering access restrictions, processing exemption claims, documenting compliance status, managing conflicts with hesitant patients. Licensed to practice under regulatory conditions that mandate enforcement participation; exit means losing license and practice.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_providers, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, healthcare_providers, payer).

% Argue for risk-stratified interventions, proportionality between threat level and intervention severity, and cost-benefit weighting that accounts for both population welfare and individual autonomy. Structurally excluded from the decision-making process under public_health_primary framing (their voice is not seated in the authority structure). Would counsel for sunset clauses, intervention de-escalation, and exemption pathways, but lack institutional power to implement these proposals. Remain as critics rather than participants.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, proportionality_advocates, excluded,
    analytical, generational, analytical, national).

% Assert that mandatory medical interventions violate bodily integrity and the right to medical self-determination, regardless of population benefit. Structurally excluded from the authority structure under public_health_primary framing (their core premise — autonomy as supreme — is treated as illegitimate within this reading). Pursue legal challenges, legislative amendments, and civil disobedience, but lack institutional power to overturn the constraint in public_health_primary's operating jurisdiction. Represent the fundamental challenge to the constraint's legitimacy grounds.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, bodily_autonomy_advocates, excluded,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem: vulnerable populations depend on others' vaccination/treatment compliance to prevent disease transmission, which creates an externality problem (individuals can free-ride on others' compliance while refusing themselves). The constraint coordinates compliance by making individual refusal costly, internalizing the externality into the hesitant individual's cost-benefit calculation. Without coordination, disease transmission creates a tragedy-of-the-commons dynamic where individual incentives diverge from population welfare.
% TRANSFER_FUNCTION: Moves disease transmission risk and mortality burden FROM the compliant and immunocompromised populations TO the public health authority (which absorbs responsibility and faces credibility loss if disease spreads) and away from vaccine-hesitant individuals (who externalize their infection risk). Employment and access restrictions transfer economic burden and social status from hesitant populations to the authority as enforcement cost. Healthcare resources shift from routine care to disease management and enforcement administration.
% ABSENT_VOICES: Proportionality advocates and bodily autonomy advocates are structurally excluded. Proportionality advocates would argue for risk stratification, graduated interventions, and sun-setting mechanisms based on threat level — they are excluded because public_health_primary treats all disease reduction as equally valued regardless of threat magnitude. Bodily autonomy advocates would reject the entire externality framing and assert autonomy as a supreme value — they are excluded because their core premise contradicts public_health_primary's legitimacy ground. Also excluded: vaccine-injured individuals who would argue for robust adverse-event compensation and exemption pathways, and individuals in low-risk populations who would argue they bear intervention costs disproportionate to their personal risk.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, disease transmission dynamics would reorganize: vaccination rates would decline to individual-preference levels (estimated 40-60% depending on disease characteristics and prior immunity), disease incidence would rise among vulnerable populations, healthcare system capacity would be challenged by increased cases and hospitalizations, and immunocompromised individuals would face elevated mortality risk. Employers would no longer enforce compliance, healthcare access would normalize, and social exclusion would cease. The public health authority would lose institutional authority and epidemiological control, forcing policy reorganization around voluntary-uptake mechanisms or rationing by risk profile. The externality problem would remain live (disease transmission is real), but would be managed differently — through pricing mechanisms, insurance differentiation, or medical triage rather than legal mandates.
% FOUNDING_PROBLEM: Early pandemic disease threat (unknown pathogenicity, high transmission rate, vulnerable population risk) created a collective-action problem: individuals refusing vaccination/treatment imposed disease transmission risk on others without bearing the full cost. Immunocompromised populations had no exit option (cannot isolate indefinitely) and faced severe mortality risk. The founding problem was genuine: absent coordination, disease would spread rapidly through the vulnerable population, causing preventable deaths and overwhelming healthcare capacity.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the problem is still live, citing resistant pathogens, new variants, and persistent immunocompromised vulnerability. Independent epidemiological analysis from academic medical centers confirms disease transmission risk remains real but is substantially lower than in the founding period (disease has become endemic, vaccines are widely available, mortality is manageable). Proportionality advocates and bodily autonomy litigants attest the founding problem has been substantially solved and the constraint persists as extraction. Disease burden has shifted from crisis to endemic management, and the external evidence (hospitalization rates, mortality trends) shows the founding threat is no longer acute. However, public health authorities have not reduced enforcement intensity proportionally — suppression remains at 0.71 even as disease threat declined, suggesting mandate persistence beyond functional necessity.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint imposes substantial costs on vaccine-hesitant populations (employment loss, access denial, social exclusion) that persist even after disease risk declines or vaccines become less effective — the enforcement infrastructure decouples from the stated health objective and becomes institutionalized extraction. Suppression (0.71) reflects active enforcement mechanisms: employment termination is legal suppression; access restrictions are regulatory suppression; social exclusion is institutional suppression. Theater_ratio (0.42 and rising to plateau) indicates growing disjunction between stated public health function and actual enforcement activity — as disease threat declines or pathogen characteristics shift, the enforcement intensity does not proportionally decline, suggesting the constraint has become partially self-sustaining (theater). Accessibility_collapse (0.64) is moderate-to-high: once the public health framing is accepted as legitimate, alternatives (medical exemptions, testing protocols, risk-stratified approaches) appear collapsed or illegitimate, but some escape routes persist (religious exemptions, political accommodation, geographic exit). Resistance (0.73) is high: vaccine-hesitant populations, medical freedom advocates, bodily autonomy litigants, and proportionality advocates mount sustained resistance, visible in court challenges, legislative amendments, and civil disobedience. The measurement series shows extractiveness and suppression rising from 0.42/0.48 (t=0) to 0.68/0.71 (t=30) and plateauing, while theater_ratio rises from 0.18 to 0.42 and plateaus — indicating a phase transition where the constraint moved from high-function coordination (early pandemic, genuine coordination problem) to enforced extraction (later period, problem solved but mechanism persists). The plateau suggests equilibrium: further extraction would trigger political backlash or legal reversal; current extraction is sustained by institutional inertia and theater.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority seat (agenda_setter, institutional power) experiences this as genuine coordination: we identified an externality (disease transmission), set a measurable target (population morbidity/mortality reduction), and enforce compliance to achieve it. The constraint is justified by outcomes. The vaccine-hesitant seat (payer, powerless-to-moderate power) experiences this as extraction: we are being coerced to bear costs (employment loss, access denial, social exclusion) based on a framing we reject, enforced by institutions we cannot influence, with no path to exit or meaningful appeal. We did not accept the externality model; we were subjected to it. The immunocompromised beneficiary seat experiences genuine coordination function: we depend on others' compliance for our safety; the constraint protects us. The proportionality advocate (excluded, observer) sees both functions but argues they are asymmetrically weighted: the constraint treats disease transmission as a supreme value and individual autonomy as negligible, when proportionality would require re-weighting based on actual threat level, vaccine efficacy, and autonomy costs. The engine computes these divergences from the structural data — different power atoms, different exit options, different roles — without requiring consensus on the constraint's legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities sit at d near 0.0-0.2 (beneficiary directionality): they set the rules, control the data, and have no exit pressure; the constraint benefits their authority and institutional mission. Immunocompromised populations sit near d=0.3-0.4 (beneficiary/low-target asymmetry): they genuinely benefit from disease reduction but carry suppression burden indirectly (their safety depends on enforcement). Vaccine-hesitant individuals sit near d=0.8-0.95 (target directionality): they bear employment loss, access denial, and social costs; their exit options are constrained (identity_locked refusal, or constrained by material dependence on employment/services); the constraint extracts from them. Medical exemption claimants sit near d=0.75-0.9 (high-target): they bear suppression via medical gatekeeping and institutional skepticism. The directive asymmetry is sharp: the beneficiary seats (authority, immunocompromised) sit far toward the low-extraction end, while the payer seats (hesitant, exemption claimants) sit far toward the high-extraction end. This asymmetry is the tangled_rope signature: genuine coordination function (disease reduction) paired with asymmetric extraction (unequal cost/benefit distribution) and active enforcement (required to maintain the asymmetry).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is 'reduce population-level morbidity and mortality through mandatory health intervention compliance.' The founding problem (pandemic disease threat, vulnerable population risk) is contested in status: the public health authority claims the problem is live and ongoing (resistant pathogens, new variants, immunocompromised vulnerability); hesitant populations and proportionality advocates claim the problem is substantially solved (vaccines effective, disease endemic, mortality manageable) or solved at a cost disproportionate to remaining threat. The measurements show extractiveness and suppression rising and plateauing even as the founding threat declines (t=0-30 represents a period where disease severity declined, vaccines became widely available, and mortality fell). Theater_ratio rising from 0.18 to 0.42 and plateauing indicates the constraint is increasingly maintained by performance rather than function: enforcement activities continue (employment restrictions, access controls) but are less directly coupled to the stated health objective. The mandate has not died (no single moment of death), but it has outlived the acute founding problem — the constraint now persists by institutional inertia and enforcement infrastructure maintenance rather than by solving a live coordination problem. This is not a classic piton (which would require no beneficiary; here the public health authority still collects authority/prestige/resource from the constraint), but it is moving toward piton characteristics: persistence despite mandate decay, requiring theater to maintain legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    readings_kernel_contest,
    'Is legitimacy grounded primarily in measurable population-level health outcomes (public_health_primary reading), in individual bodily autonomy irrespective of outcomes (bodily_autonomy_primary reading), or in proportionality between intervention severity and threat level (proportionality_reading)?',
    'Constitutional adjudication, legislative mandate revision, or persistent empirical challenge to the reading''s core premise (e.g., evidence that interventions do not produce claimed population benefit, or that externality models fail to account for cross-cohort variation in disease transmission).',
    'This reading instantiates public_health_primary: the constraint''s legitimacy is ONLY sustained by measured reduction in population morbidity/mortality. If empirical evidence shows the intervention no longer reduces population harm (e.g., pathogen evolution, waning efficacy, behavioral substitution), the constraint loses its legitimacy ground in this reading — the constraint either reclassifies to snare (pure extraction with a weakened cover story) or the reading itself forecloses. Sibling readings would reframe the assessment differently: bodily_autonomy_primary would reject the constraint regardless of population benefit; proportionality_reading would require re-weighting severity against autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(readings_kernel_contest, conceptual, 'Which normative ground — population welfare, individual bodily integrity, or proportionality — legitimates the constraint.').

omega_variable(
    externality_model_accuracy,
    'Do unvaccinated individuals actually impose measurable externalities (disease transmission, hospitalization burden, mortality) on the general population, or does the externality model misattribute causal responsibility (e.g., by ignoring vaccinated transmission, behavioral compensation, or prior immunity)?',
    'Epidemiological analysis of transmission chains, hospitalization attribution by vaccination status, and counter-factual modeling of disease dynamics under alternative policy regimes.',
    'If externalities are overestimated or misattributed, the victim set (vaccine_hesitant_individuals, medical_exemption_claimants) bears costs not justified by the public health ground this reading claims. The constraint would then show characteristics of snare (extraction under a false coordination cover) rather than tangled_rope (genuine coordination + asymmetric enforcement). If externalities are real but smaller than measured suppression/enforcement suggests, the constraint shows extraction above the level the public health benefit justifies — still tangled_rope, but with higher excess extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_model_accuracy, empirical, 'Whether the externality model accurately identifies who harms whom and by how much.').

omega_variable(
    enforcement_mechanism_proportionality,
    'Are the suppression mechanisms (employment termination, access restrictions, social exclusion) proportionate to the public health objective and the magnitude of individual externality, or do they constitute punitive measures decoupled from the stated harm reduction goal?',
    'Comparative analysis of enforcement intensity across populations (are enforcement actions stratified by risk profile, or uniform across low-risk and high-risk populations?); cost-benefit assessment of alternative less-suppressive mechanisms (e.g., testing protocols, treatment access); post-removal trajectory of suppression (does suppression persist after the policy ends, indicating internalization or institutional capture?).',
    'If enforcement is disproportionate, the constraint shifts from tangled_rope (extraction justified by coordination need + actual enforcement necessity) toward snare (extraction maintained by suppression regardless of stated goal). High theater_ratio (0.42) already suggests growing disjunction between stated function and actual enforcement activity; if proportionality assessment shows enforcement far exceeds what the public health claim requires, that confirms the disjunction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_proportionality, empirical, 'Whether enforcement mechanisms remain calibrated to the public health objective or have become decoupled from it.').

omega_variable(
    alternative_readings_coexistence_boundary,
    'Can bodily_autonomy_primary and proportionality_reading coexist with public_health_primary in a single legal/institutional framework, or does public_health_primary''s supremacy axiom logically foreclose them?',
    'Jurisprudential analysis: if courts, legislatures, or institutions actively hold multiple readings simultaneously (some agents claim autonomy grounds, others claim proportionality, still others claim public health supremacy), the readings coexist; if institutional dominance rules out one reading''s core premise as legally binding, foreclosure is present.',
    'This omega addresses the cs_structure.reading_relations choice: if public_health_primary forecloses bodily_autonomy_primary (the readings logically cannot both be true in the same framework), that is a foreclosure relation. If they coexist as competing positions held by different institutional/legal authorities, that is coexistence. The choice shapes the family structure and terminal type predictions: foreclosure implies one reading will eventually lose legal standing; coexistence implies indefinite contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_coexistence_boundary, conceptual, 'Whether this reading''s axioms logically rule out sibling readings'' core premises, or whether multiple readings can remain live positions.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structurally enforced (external barriers: employment loss, legal restrictions) or internalized (cognitive patterns: acceptance of the legitimacy claim, fear of contagion attribution, identity fusion with ''good citizen'' status)?',
    'Post-intervention tracking: remove the enforcement mechanism and measure whether suppression persists in targeted populations. High persistence indicates internalization; rapid rebound indicates structural suppression.',
    'If suppression is internalized, the constraint''s effective suppressive force is higher than the authored 0.71 suggests — the target carries the constraint with them after external enforcement ends. If structural, the suppression is narrower (only active when enforcement machinery operates) and could be more easily disrupted by exit or legal challenge. Internalization also affects identity_locked exit coding: highly internalized suppression suggests stronger identity fusion with the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in the vaccine-hesitant population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__public_health_primary, theater_ratio, 6, 0.26).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.33).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.39).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.42).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__public_health_primary, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.59).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.65).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__public_health_primary, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__public_health_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'legitimate_health_intervention'. The constraint family includes three sibling readings, each instantiating a different legitimacy ground: public_health_primary (this constraint — population-level outcomes as supreme), bodily_autonomy_primary (individual medical consent as supreme), and proportionality_reading (multi-value optimization by threat level). Each reading has distinct beneficiary/victim structures, distinct ε values, and distinct type classifications. They are NOT the same constraint viewed from different angles; they are different constraints derived from the same contested kernel. The family is linked by network.affects_constraints and by the kernel_context commentary. Each reading must be authored separately to preserve the structural data that generates its classification; merging readings into one story would obscure the structural differences that make the contest real. The upstream reading (public_health_primary) influences the downstream readings: bodily_autonomy advocates argue against public_health_primary's supremacy claim; proportionality advocates argue for re-weighting of public_health_primary's weights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, powerless, 0.88).
constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
