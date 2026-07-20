% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law â Sovereignty Restoration Reading
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The National Security Law for Hong Kong (NSL) was imposed by the Standing
 *   Committee of the National People's Congress in June 2020 following the
 *   mass unrest of 2019. This constraint story instantiates the SOVEREIGNTY
 *   RESTORATION READING of the NSL legal text kernel: the framing that the
 *   NSL is a legitimate sovereign security instrument restoring
 *   constitutional order after the 2019 unrest. In this reading, protesters
 *   and activists enter the victim set as security threats, and the CPG
 *   authority enters the beneficiary set. The constraint targets political
 *   opposition rather than the general population, producing moderate
 *   extractiveness. This is one of three readings of a contested kernel; the
 *   democratic_enclosure_reading and jurisdictional_capture_reading are
 *   sibling constraints.
 *
 * KEY AGENTS:
 *   - cpg_authority: Primary agenda-setter and beneficiary (institutional/arbitrage) â defines threats and collects political stability
 *   - hk_sar_government: Secondary agenda-setter and beneficiary (institutional/constrained) â local implementer locked into CPG framework
 *   - pro_establishment_camp: Secondary beneficiary (organized/constrained) â gains electoral dominance from opposition suppression
 *   - protesters_activists: Primary target (powerless/trapped) â designated as security threats, face arrest and prosecution
 *   - political_opposition: Primary target (moderate/trapped) â platform criminalized, disqualified, exiled
 *   - hk_legal_profession: Observer (organized/constrained) â documents common law erosion without power to stop it
 *   - international_community: Excluded observer (institutional/analytical) â contests the law but has no authority within the framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.58).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.68).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law â Sovereignty Restoration Reading").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, 'baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38').
narrative_ontology:cs_kernel_codification('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', formalized).
narrative_ontology:cs_authority_grounding('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', lineage).
narrative_ontology:cs_interpretation_layer_present('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38').
narrative_ontology:cs_reading_relation('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', foundational, sovereign_duty_to_secure_territory_after_unrest).
narrative_ontology:cs_axiom_status(sovereign_duty_to_secure_territory_after_unrest, holdable).
narrative_ontology:cs_axiom_grounding('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', sovereign_duty_to_secure_territory_after_unrest, conventional).
narrative_ontology:cs_axiom('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', foundational, exceptional_security_measures_restore_constitutional_equilibrium).
narrative_ontology:cs_axiom_status(exceptional_security_measures_restore_constitutional_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', exceptional_security_measures_restore_constitutional_equilibrium, instrumental).
narrative_ontology:cs_reference_frame('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', basic_law_sovereignty_framework).
narrative_ontology:cs_drift_state('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', contemporary_post_nsl_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('baccaf6a-c3a2-41c5-a9bf-83a4bcfa3c38', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hk_sar_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_camp).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protesters_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, political_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted the NSL through the NPCSC and claims it as an exercise of sovereign power to restore constitutional order after the 2019 unrest. Defines security threats, authorizes mainland security apparatus involvement in Hong Kong, and issues binding legal interpretations. Collects the primary benefit of eliminated political opposition to its sovereignty narrative.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, cpg_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, cpg_authority, beneficiary).

% Implements the NSL locally through the HK Police National Security Department, the Department of Justice, and the courts. Gains administrative control over previously contested political spaces but operates under CPG oversight and cannot deviate from the NSL framework.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hk_sar_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hk_sar_government, beneficiary).

% Political parties, legislators, and civil society groups aligned with Beijing benefit from the disqualification and silencing of the opposition. They gain electoral dominance and policy control without needing to compete for moderate votes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_camp, beneficiary,
    organized, biographical, constrained, national).

% Individuals who participated in the 2019 protests are designated as security threats under broad definitions of subversion and secession. Face arrest, lengthy pre-trial detention, and severe sentences. Exit options are limited to silence or exile; remaining in Hong Kong while continuing activism guarantees prosecution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protesters_activists, payer,
    powerless, immediate, trapped, local).

% Former legislators, district councillors, and party members whose platforms and activities are now criminalized. Disqualified from office, subjected to mass arrests, or driven into exile. Their political identity itself constitutes the threat the NSL targets.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, political_opposition, payer,
    moderate, biographical, trapped, local).

% Barristers and solicitors operating under the NSL's erosion of common law procedural safeguards including bail presumptions and jury trial rights. They document and litigate within the constraint but cannot alter its structural operation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hk_legal_profession, observer,
    organized, generational, constrained, national).

% Foreign governments and international rights bodies that contest the NSL as a breach of the Sino-British Joint Declaration and international human rights law. Structurally excluded from the PRC legal framework; their sanctions and statements carry no authority within the constitutional order that hosts the constraint.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, international_community, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restoring public order and the functioning of government after months of mass unrest in 2019 that featured violent confrontations, transport paralysis, and open challenges to sovereign authority.
% TRANSFER_FUNCTION: Moves the power to define criminal political threats and adjudicate national security from Hong Kong's common law legislature and courts to the NPCSC-aligned security apparatus, transferring silence and compliance from opposition activists to the stability of the establishment.
% ABSENT_VOICES: Exiled pro-democracy activists, disqualified legislators, and international human rights bodies are structurally absent from the legal and constitutional conversation; their objections are treated as foreign interference or subversion rather than legitimate dissent.
% DISAPPEARANCE_RATIONALE: Without the NSL, the pro-democracy opposition would re-enter electoral politics and street mobilization, the 2019-style protest cycle would likely resume, and the CPG's narrative of restored constitutional order would face direct challenge. The current political arrangement in Hong Kong is organized around this constraint.
% FOUNDING_PROBLEM: Sustained mass unrest in 2019 featuring violent clashes, attacks on public institutions, transport paralysis, and a perceived collapse of governmental authority that the existing legal framework seemed unable to halt.
% FOUNDING_PROBLEM_CORROBORATION: The CPG and HK SAR government attest the problem remains live, citing continued threats and foreign interference. Pro-democracy figures, exiled activists, and independent academic analysis from outside the PRC beneficiary set largely frame the 2019 events as a political dispute requiring political accommodation rather than a security threat; international human rights organizations corroborate the political-framing reading. No corroboration from outside the benefiting parties supports the exclusively security framing.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the NSL targets a specific political subset (opposition, activists) rather than the general population, but within that subset the extraction is severe. Suppression is substantial (0.68) because the constraint's persistence depends on active enforcement by a dedicated security apparatus, show trials, and the exclusion of rival political organization. Theater ratio (0.40) reflects the performative dimension of high-profile prosecutions and public confessions that signal the restored order. Accessibility collapse (0.70) is high because once the legal framework is understood, alternatives to compliance (open dissent, opposition politics) effectively collapse. Resistance (0.48) captures persistent international condemnation and local resilience despite fear. The temporal series show a sharp inflection at imposition (2020) followed by a plateau as enforcement normalizes.
 *
 * PERSPECTIVAL GAP:
 *   The CPG authority seat experiences the NSL as legitimate restoration of order and sovereignty; the protesters_activists and political_opposition seats experience it as criminalization of their political existence. The engine computes this divergence from the structural data: agenda_setter/beneficiary with arbitrage exit versus powerless payer with trapped exit. The HK SAR government sits between these poles â it enforces and benefits but is itself constrained by CPG oversight.
 *
 * DIRECTIONALITY LOGIC:
 *   The CPG authority is the structural beneficiary and agenda-setter (low d, subsidized by the constraint's elimination of opposition). The HK SAR government and pro_establishment_camp are secondary beneficiaries (low-moderate d). Protesters and political opposition are the targets (high d, amplified extraction). The international community is excluded (analytical d). The legal profession sits near symmetric but is pushed toward target by constrained exit options within a shrinking common law space.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing the NSL from pure democratic enclosure: it acknowledges the genuine coordination function of restoring order after violent unrest, which a snare reading would deny. However, the metrics and victim declarations prevent it from being mislabeled as pure rope: the asymmetric extraction onto opposition activists is structural, not incidental. The moderate extractiveness and active enforcement requirement place it in tangled_rope rather than mountain or rope. If the founding problem (2019 unrest) is dead but the arrangement persists unchanged, the mandatrophy flag would trigger piton/snare drift; the authored status is contested, not dead, leaving the classification at tangled_rope pending further temporal data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_restoration_vs_enclosure,
    'Is the NSL a genuine restoration of pre-existing constitutional order, or the creation of a new sovereign security regime that supersedes local autonomy?',
    'Comparative constitutional analysis of whether the NSL''s provisions (NPCSC interpretation power, mainland security presence, common law overrides) restore the pre-2019 legal status quo or establish novel supra-constitutional authority.',
    'If novel authority, the sovereignty_restoration_reading collapses toward jurisdictional_capture or democratic_enclosure; if genuine restoration, the moderate extractiveness reading is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_restoration_vs_enclosure, conceptual, 'Ambiguity between restoration narrative and novel regime creation').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the compliance produced by the NSL primarily structural (enforcement threat) or internalized (self-censorship and political identity abandonment)?',
    'Measurement of expressive activity among HK residents who have not been directly threatened: if silence persists without direct enforcement contact, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates as cognitive capture; if purely structural, removal of enforcement would rapidly restore opposition activity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    coordination_extraction_boundary_nsl,
    'Does the NSL''s order-restoration function remain separable from its opposition-targeting function, or have the two fused into a single extractive apparatus?',
    'Natural experiment analysis: in policy domains where order was restored without opposition suppression (if any), versus domains where opposition suppression continues without ongoing disorder.',
    'If separable, the constraint retains genuine coordination value and stays tangled_rope; if fused, it trends toward snare as the coordination story becomes pure cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_nsl, conceptual, 'Whether order restoration and opposition suppression are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nsl__tr_t1, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 1, 0.4).
narrative_ontology:measurement(nsl__tr_t2, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 2, 0.45).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(nsl__tr_t5, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 6, 0.37).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nsl__be_t1, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 1, 0.5).
narrative_ontology:measurement(nsl__be_t2, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(nsl__be_t5, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(nsl__su_t1, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 1, 0.75).
narrative_ontology:measurement(nsl__su_t2, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 2, 0.78).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 3, 0.76).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 4, 0.74).
narrative_ontology:measurement(nsl__su_t5, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 6, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__sovereignty_restoration_reading, 0.1).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% The NSL legal text kernel decomposes into three structurally distinct constraints: the sovereignty_restoration_reading (this file, moderate epsilon, coordination-plus-extraction), the democratic_enclosure_reading (higher epsilon, pure extraction framing), and the jurisdictional_capture_reading (focus on legal system transplantation). They differ in beneficiary/victim structure and empirical claims about the NSL's function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
