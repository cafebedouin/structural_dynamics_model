% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: Article 17 Right to Erasure: Privacy Fundamental Reading
 *   domain: technology_governance/data_protection_law
 *
 * SUMMARY:
 *   Article 17 of the GDPR establishes a conditional right to erasure ('right
 *   to be forgotten') allowing individuals to request deletion of personal
 *   data under specified conditions. This constraint story instantiates the
 *   privacy-fundamental reading: the right to erasure operationalizes
 *   individual data sovereignty as a fundamental right limiting corporate
 *   data retention indefinitely. Under this reading, the kernel (the right to
 *   erasure itself) is understood as grounded in human dignity and
 *   informational self-determination, making the individual the primary
 *   beneficiary and the platform data-retention model the primary victim.
 *   However, Article 17 contains exceptions (legitimate interests, legal
 *   obligations, public interest archiving) and enforcement gaps that create
 *   a tangled rope structure: genuine coordination benefit (enabling data
 *   minimization) coexists with asymmetric extraction (platforms bear
 *   compliance costs while exceptions preserve profitable retention). The
 *   measurement trajectory shows rising theater_ratio (0.35 → 0.52) as
 *   platforms develop denial strategies and legitimate-interests
 *   interpretations expand, suggesting the constraint is drifting toward
 *   piton (performative) territory. This story instantiates one reading of a
 *   contested institutional kernel where three incompatible framings coexist:
 *   the privacy-fundamental reading (this story), the competitive-moat
 *   reading (erasure as tool for smaller platforms to constrain dominant
 *   competitors), and the censorship-mechanism reading (erasure as vector for
 *   authoritarian information suppression).
 *
 * KEY AGENTS:
 *   - Data Subject (Individual): Primary beneficiary (powerless/constrained) — individuals seeking to limit platforms' retention of their behavioral data and profiling
 *   - Digital Platform (Institutional): Primary victim (institutional/constrained) — bears infrastructure costs of erasure compliance and faces reduced data-retention periods for behavioral targeting
 *   - Behavioral Targeting Ecosystem: Secondary victim (moderate/mobile) — ad-tech and data broker networks dependent on long-term profile retention
 *   - Privacy Advocacy Coalition: Organized beneficiary (organized/arbitrage) — privacy advocates and smaller platforms using erasure rights strategically
 *   - Legal Compliance Apparatus: Institutional actor (institutional/constrained) — Data Protection Authorities and corporate compliance teams managing exception interpretations and denial processes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing negotiated institutional arrangement as fundamental law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.38).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.48).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, tangled_rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "Article 17 Right to Erasure: Privacy Fundamental Reading").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology_governance/data_protection_law").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '6b468e59-41d2-4557-a29e-006380455911').
narrative_ontology:cs_kernel_codification('6b468e59-41d2-4557-a29e-006380455911', formalized).
narrative_ontology:cs_authority_grounding('6b468e59-41d2-4557-a29e-006380455911', lineage).
narrative_ontology:cs_interpretation_layer_present('6b468e59-41d2-4557-a29e-006380455911').
narrative_ontology:cs_reading_relation('6b468e59-41d2-4557-a29e-006380455911', article17_erasure_right__competitive_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b468e59-41d2-4557-a29e-006380455911', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('6b468e59-41d2-4557-a29e-006380455911', foundational, individual_data_sovereignty_fundamental_right).
narrative_ontology:cs_axiom_status(individual_data_sovereignty_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('6b468e59-41d2-4557-a29e-006380455911', individual_data_sovereignty_fundamental_right, deontological).
narrative_ontology:cs_axiom('6b468e59-41d2-4557-a29e-006380455911', foundational, informational_self_determination_as_dignity).
narrative_ontology:cs_axiom_status(informational_self_determination_as_dignity, holdable).
narrative_ontology:cs_axiom_grounding('6b468e59-41d2-4557-a29e-006380455911', informational_self_determination_as_dignity, deontological).
narrative_ontology:cs_reference_frame('6b468e59-41d2-4557-a29e-006380455911', gdpr_recital_1_human_dignity_framework).
narrative_ontology:cs_drift_state('6b468e59-41d2-4557-a29e-006380455911', contemporary_post_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6b468e59-41d2-4557-a29e-006380455911', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, privacy_advocates).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, platform_data_retention_models).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, behavioral_targeting_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (ROPE) — Individual experiences Article 17 as genuine coordination mechanism for reclaiming informational self-determination. Erasure requests enable exit from data-driven profiling. Extraction is minimal because the constraint primarily enforces the subject's own right rather than imposing burden. High exit cost (submitting requests, navigating corporate denials) but meaningful agency and benefit.
constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: DIGITAL PLATFORM (TANGLED ROPE) — Platform experiences Article 17 as mixed constraint. Genuine coordination function: erasure enables data minimization and reduces liability surface. Asymmetric extraction: erasure requests impose infrastructure costs (database reorganization, historical deletion, ML model retraining) borne disproportionately by the platform. Constraint persists because platforms also benefit from compliance legitimacy and reduced GDPR penalty risk. Suppression through denial practices and technical friction (complex request processes, narrow interpretation).
constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: BEHAVIORAL TARGETING ECOSYSTEM (SNARE) — The ad-tech and data broker ecosystem experiences erasure as a snare: the right directly constrains the data retention and profiling models that generate their profit. Extraction is high because erasure eliminates the commodity (behavioral profiles) without proportional compensation. Suppression operates through fragmentation (data brokers outside EU scope), technical opacity (profile reconstruction), and lobbying pressure. No coordination benefit to this agent.
constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY ADVOCACY COALITION (SCAFFOLD) — Organized privacy advocates see Article 17 as a temporary scaffolding mechanism: erasure rights build individual agency while European data governance frameworks mature toward decentralized/sovereign data architectures (personal data stores, data portability alternatives). Sunset logic: as technical standards for personal data control advance, statutory erasure rights become less necessary. Low effective extraction because coalition has agency and sees exit path through alternative governance models.
constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGAL COMPLIANCE APPARATUS (PITON) — Data Protection Authorities and internal compliance teams experience Article 17 as increasingly performative. Core function (enabling erasure requests) persists, but mounting exceptions (legitimate interests, legal obligations, law enforcement) and weak enforcement produce theater_ratio ≥ 0.52. Compliance infrastructure maintains itself through institutional inertia and penalty risk rather than functional verification that erasure actually prevents data reuse.
constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FUNDAMENTAL RIGHT VIEW (MOUNTAIN) — From the civilizational/universal analytical perspective, individual data sovereignty maps to a fundamental right as immutable as freedom from bodily seizure: the right to control information about oneself is a cornerstone of human dignity and cannot be traded away. This reading treats erasure as a natural-law-like constraint that emerges from the logic of personhood itself. However, the structural data (beneficiary organization, suppression mechanisms, exceptions) reveals this as a false summit: the fundamental right framing naturalizes what is actually a negotiated institutional arrangement between competing power centers.
constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article17_erasure_right__privacy_fundamental_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, TR),
    TR >= 0.70.

:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The privacy-fundamental reading measures extraction by the cost platforms bear to comply with erasure requests (database reorganization, ML model retraining, reduced data-driven revenue) relative to the benefit individuals and privacy advocates gain (agency, data minimization, reduced targeting surface). This is lower than the competitive-moat reading (which would score higher, treating erasure as a tool for competitors to constrain dominant platforms) because the fundamental-rights reading centers individual dignity rather than competitive advantage. Suppression (0.48): Moderate-high. Platforms suppress erasure through denial strategies (claiming legitimate interest exceptions broadly, requiring individualized requests, implementing slow processing), technical opacity (difficult request interfaces, unclear data scope), and exception inflation. However, suppression is not total because regulatory oversight and judicial review constrain outright denial. Theater ratio (0.52): Rising over the interval, indicating increasing performativity. Early in the GDPR period (t=0), erasure requests were processed with genuine data deletion. As platforms developed exception interpretations and denial practices, theater increased. By t=6, a significant fraction of erasure requests are denied on legitimate interests grounds without clear evidence that profiling actually stops—compliance becomes ritually demonstrating that a request was received and considered, not verification of actual data deletion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional kernel (the right to erasure) generates structurally incompatible classifications depending on the reading adopted. The privacy-fundamental reading (this story) treats erasure as individual agency, producing a data-subject perspective that sees Rope (coordination) and a platform perspective that sees Tangled Rope (asymmetric enforcement). The competitive-moat reading would flip these: platforms at the margin would see Rope (using erasure to constrain larger competitors), while dominant platforms would see Snare (forced to weaken competitive moats). The censorship-mechanism reading would position authoritarian states as beneficiaries (erasure rights can suppress dissent) and platforms/individuals as victims. All three readings are coherent interpretations of the same legal text, but they route beneficiaries and victims in opposite directions.
 *
 * DIRECTIONALITY LOGIC:
 *   The privacy-fundamental reading derives directionality by placing the individual data subject as primary beneficiary and the platform data-retention model as primary victim. Data subjects (powerless power atom, constrained exit) have low d because they benefit from erasure—the constraint subsidizes their agency. Platforms (institutional power atom, constrained exit due to regulatory obligation) have high d because they bear compliance costs. The behavioral targeting ecosystem (moderate power, mobile exit through geographic arbitrage) has very high d because erasure directly eliminates their core commodity without compensation. The privacy advocacy coalition (organized power, arbitrage exit through alternative governance models) has moderate d because they benefit from erasure but also bear costs of maintaining the political coalition. The directionality derivation is consistent across the interval because the structural positions do not change—only the effectiveness of suppression increases, raising suppression_requirement measurements.
 *
 * MANDATROPHY ANALYSIS:
 *   The privacy-fundamental reading avoids mandatrophy by clearly identifying the coordination function (enabling data minimization and individual agency over personal data) and the asymmetric extraction (platforms bear compliance costs). The constraint is tangled rope, not snare or rope, because both elements are genuine: erasure does coordinate data governance and does extract from platforms. The false-summit risk is in the mountain perspective—treating data sovereignty as an immutable natural law rather than a negotiated institutional arrangement. The structural data (rising theater_ratio, growing legitimate-interests exceptions, suppression increases) supports the false-summit detection: what appears as inevitable law is actually a contingent arrangement between competing power centers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_interests_scope_ambiguity,
    'What constitutes a ''legitimate interest'' sufficient to override erasure rights under Article 6(1)(f), and does this exception systematically preserve the retention models Article 17 is intended to limit?',
    'Comparative analysis of DPA decisions and case law: fraction of erasure requests denied on legitimate interests grounds; correlation between legitimate interests claims and continued data-driven profiling; evidence of whether legitimate interests interpretation has drifted toward platform interests over time',
    'If legitimate interests exception is narrow and enforced consistently: Article 17 functions as intended (Rope/Tangled Rope). If exception is expansive and systematically favors platform retention: erasure becomes performative (Piton) rather than structural constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimate_interests_scope_ambiguity, empirical, 'Whether legitimate interests exception systematically preserves data retention models').

omega_variable(
    profile_reconstruction_technical_feasibility,
    'Can behavioral profiles be technically reconstructed from non-deleted residual data (logs, transaction histories, inference chains) such that erasure is functionally circumvented?',
    'Technical audit of platform architecture: whether deleted user data is fully excised or remains in derivative forms; adversarial reconstruction experiments; correlation between erasure requests and actual reduction in platform''s targeting capacity',
    'If reconstruction is technically feasible and platforms exploit it: erasure is theater (Piton), suppression is higher than measured (0.48 is floor). If deletion is thorough and prevents reconstruction: constraint is functional as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(profile_reconstruction_technical_feasibility, empirical, 'Whether behavioral profiles can be reconstructed after erasure').

omega_variable(
    fundamental_right_vs_negotiated_right_boundary,
    'Is individual data sovereignty framed as a fundamental right (immutable, inalienable, not subject to balancing) or as a negotiated right (subject to exception, legitimate interest balancing, proportionality review)?',
    'Textual and jurisprudential analysis: GDPR Article 17 language and ECJ/national court interpretation; comparison with how other fundamental rights (e.g. freedom of expression) are treated when in conflict with platform interests; whether Article 17 is positioned as absolute or as one interest among competing rights',
    'If fundamental right framing holds: Article 17 must permit no exceptions (mountain logic). If negotiated right framing holds: exceptions and balancing are legitimate (tangled rope logic). This is the central omega determining whether false summit detection applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fundamental_right_vs_negotiated_right_boundary, conceptual, 'Whether data sovereignty is fundamental right or negotiated right').

omega_variable(
    kernel_reading_ambiguity,
    'Which interpretation of Article 17''s kernel (the right to erasure itself) is institutionally dominant: the privacy-fundamental reading (individual agency as primary), the competitive-moat reading (erasure as anticompetitive tool), or the censorship-mechanism reading (erasure as information suppression vector)?',
    'Institutional mapping: which reading dominates in DPA guidance documents, ECJ rulings, and legislative discourse at the time of evaluation; corpus analysis of Article 17 case law to identify which framing appears in majority opinions vs dissents vs advocacy positions',
    'Reading dominance shifts power allocation. Privacy-fundamental reading maximizes individual beneficiaries and platform victims. Competitive-moat reading shifts beneficiaries to smaller platforms and victims to dominant platforms. Censorship-mechanism reading shifts victims to platforms and beneficiaries to authoritarian states. Dominance is not fixed—it drifts with political economy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which Article 17 reading is institutionally dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a17pf_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(a17pf_tr_t3, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement(a17pf_tr_t6, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(a17pf_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(a17pf_be_t3, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(a17pf_be_t6, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(a17pf_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(a17pf_su_t3, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 3, 0.43).
narrative_ontology:measurement(a17pf_su_t6, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__privacy_fundamental_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, gdpr_legitimate_interests_exception).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, behavioral_targeting_infrastructure_constraint).

% DUAL FORMULATION NOTE:
% Article 17 is a contested kernel with three structurally distinct readings instantiated as separate constraints. This story (privacy-fundamental reading) treats the individual data subject as primary beneficiary. The competitive-moat reading (upstream) treats smaller platforms as beneficiaries against dominant platforms. The censorship-mechanism reading (upstream) treats authoritarian information control as a possible beneficiary. Each reading has different ε, different beneficiary/victim structures, and different terminal classifications. The three are linked via network.affects_constraints because they compete for institutional dominance in Article 17 interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article17_erasure_right__privacy_fundamental_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
