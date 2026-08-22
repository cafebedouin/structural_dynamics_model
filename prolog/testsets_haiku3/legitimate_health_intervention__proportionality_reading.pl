% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Health Intervention Proportionality Constraint (Balanced Reading)
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   The proportionality reading of legitimate health intervention holds that
 *   state authority to mandate medical procedures (vaccination, isolation,
 *   quarantine) is legitimate only when intervention severity is proportional
 *   to disease threat. Threat is measured by transmissibility, case-fatality
 *   rate, hospitalization burden, and variants; legitimacy requires that
 *   autonomy costs scale with threat magnitude. This reading rejects bodily
 *   autonomy absolutism (which denies any legitimate mandate regardless of
 *   threat) and public health primacy (which permits any intervention
 *   justified by any measurable risk). The proportionality reading is
 *   contested: public health authorities claim they already apply it and
 *   accuse critics of misrepresenting threat levels; autonomy advocates claim
 *   proportionality is a mask for unjustified coercion; public health primacy
 *   proponents argue proportionality ties their hands when disease burden is
 *   severe. This JSON instantiates ONLY the proportionality reading as a
 *   self-contained constraint with its own ε, beneficiary/victim structure,
 *   and type classification. The sibling readings (bodily_autonomy_primary,
 *   public_health_primary) are separate constraint stories in the corpus,
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Public health authority: institutional agenda-setter; calibrates intervention severity to threat; collects legitimacy from proportionality standard
 *   - Population at measurable risk: beneficiary (herd immunity protection); organized, mobile exit
 *   - Vaccine-hesitant individuals: payer (autonomy constraint, medical procedures); moderate power; constrained exit
 *   - Religious objectors: payer (autonomy constraint); identity-locked, cannot exit belief framework
 *   - Individuals with medical contraindications: payer (trapped bind: intervention or disease risk); powerless, trapped exit
 *   - Healthcare system: beneficiary (reduced disease burden, planning confidence); institutional
 *   - Disease surveillance experts: observer; institutional authority for threat assessment
 *   - Judicial review: observer; polices proportionality standard
 *   - Bodily autonomy absolutists: excluded; their core claim contradicts proportionality premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.42).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.38).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Health Intervention Proportionality Constraint (Balanced Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '211e400a-a20b-41c8-9556-f665d4200182').
narrative_ontology:cs_kernel_codification('211e400a-a20b-41c8-9556-f665d4200182', formalized).
narrative_ontology:cs_authority_grounding('211e400a-a20b-41c8-9556-f665d4200182', expertise).
narrative_ontology:cs_interpretation_layer_present('211e400a-a20b-41c8-9556-f665d4200182').
narrative_ontology:cs_reading_relation('211e400a-a20b-41c8-9556-f665d4200182', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('211e400a-a20b-41c8-9556-f665d4200182', legitimate_health_intervention__public_health_primary, influences).
narrative_ontology:cs_axiom('211e400a-a20b-41c8-9556-f665d4200182', foundational, proportionality_principle_legitimate).
narrative_ontology:cs_axiom_status(proportionality_principle_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('211e400a-a20b-41c8-9556-f665d4200182', proportionality_principle_legitimate, deontological).
narrative_ontology:cs_axiom('211e400a-a20b-41c8-9556-f665d4200182', foundational, threat_level_scales_autonomy_cost).
narrative_ontology:cs_axiom_status(threat_level_scales_autonomy_cost, holdable).
narrative_ontology:cs_axiom_grounding('211e400a-a20b-41c8-9556-f665d4200182', threat_level_scales_autonomy_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('211e400a-a20b-41c8-9556-f665d4200182', evidence_based_threat_responsive_intervention).
narrative_ontology:cs_drift_state('211e400a-a20b-41c8-9556-f665d4200182', contemporary_post_pandemic, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('211e400a-a20b-41c8-9556-f665d4200182', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, population_at_measurable_risk).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, healthcare_system).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, religious_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, individuals_with_contraindications).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implements intervention policies (vaccination mandates, quarantine orders) calibrated to disease threat level. Conducts epidemiological assessment, sets proportionality thresholds, and enforces compliance through administrative and legal mechanisms. Balances population health against individual liberty claims by disease-specific risk metrics.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from herd immunity thresholds reached through interventions; disease transmission is reduced proportionally to intervention coverage. Their risk reduction is the coordination function the constraint serves. Heterogeneous: infants and immunocompromised benefit most; healthy adults benefit less directly but still gain protection from disease circulation suppression.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, population_at_measurable_risk, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of mandates (bodily autonomy constraint, medical risk from vaccines themselves) while retaining some benefit from herd immunity if threshold is reached without them. Their exit options are constrained: they can refuse and face employment loss, school exclusion, or fines; they cannot easily relocate to escape mandatory vaccination jurisdictions without major life disruption.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_individuals, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_individuals, beneficiary).

% Object to interventions on grounds of sincere religious belief (divine providence, bodily temple doctrine). Their exit is identity-locked: departing the belief system is not a real option; they bear the constraint's costs (exclusion, coercion, legal penalties) because their identity framework makes acceptance impossible. Some jurisdictions offer religious exemptions, but these are conditional and subject to challenge.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, religious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Have documented medical reasons for non-participation (severe allergies, immunological conditions, prior adverse reactions). They cannot refuse and remain safe; they cannot participate and remain safe. The constraint creates a bind: mandatory participation poses medical harm; exclusion exposes them to disease risk. Medical exemptions exist in principle but are narrowly defined and difficult to obtain; they depend on individual clinician judgment and institutional policy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, individuals_with_contraindications, payer,
    powerless, biographical, trapped, national).

% Benefits from reduced disease burden, predictable patient flow, and prevention-focused resource allocation. The constraint enables planning: systems can staff and stock for reduced infectious disease load rather than pandemic surge. Public trust in mandatory intervention also depends on proportionality credibility; aggressive mandates on low-threat diseases erode the healthcare system's legitimacy for high-threat scenarios.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, healthcare_system, beneficiary,
    institutional, generational, analytical, national).

% Monitor and assess threat levels using standardized metrics: transmissibility (R-value), case-fatality rate, hospitalization burden, variants. They produce the epidemiological basis for proportionality judgments. Their authority is central to the constraint's legitimacy: if threat assessment appears captured by political actors or disconnected from evidence, the proportionality reading itself becomes contestable.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, disease_surveillance_experts, observer,
    institutional, generational, analytical, global).

% Reviews whether implemented interventions meet proportionality standards: do restrictions fit the declared threat? Do exemptions follow from the proportionality logic or undermine it? Their role is to police the constraint's internal consistency and prevent drift from proportionality into unrestrained coercion.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, judicial_review_authority, observer,
    institutional, generational, analytical, national).

% Hold that no public health benefit justifies bodily intervention without consent. They are excluded from the constraint-setting process because the proportionality reading explicitly rejects their premise: legitimate intervention does not require absolute consent, only that intervention severity be proportional to threat. If included, they would contest the core claim of the constraint.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, bodily_autonomy_absolutists, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_authority).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches intervention severity (vaccination mandates, isolation, movement restriction) to disease threat level (transmissibility, case-fatality rate, hospitalization burden, variants). Solves the coordination problem of preventing disease transmission at population scale while respecting individual liberty to a degree proportional to threat. Both population harm and individual autonomy matter; they are weighted by disease characteristics (high-threat diseases justify aggressive intervention and limited exemptions; low-threat diseases justify minimal intervention and broad exemptions).
% TRANSFER_FUNCTION: Moves bodily autonomy costs (medical procedures, occupational exclusion, movement restriction, medical exemption gatekeeping) from the general population and from beneficiaries (those protected by herd immunity) to identified groups (vaccine-hesitant individuals, religious objectors, individuals with contraindications). The transfer is conditional on threat level: it is supposed to scale with threat magnitude. For high-threat diseases, the transfer is justified by proportionality; for low-threat diseases, the transfer is unjustified and represents extraction.
% ABSENT_VOICES: Bodily autonomy absolutists are structurally excluded from the proportionality framework because their core claim (informed consent is non-negotiable regardless of benefit) contradicts proportionality's premise (some threat levels justify coercive intervention). Anti-vaccination advocates are typically present but their claims are filtered through threat assessment: their objections gain force if threat is low or exaggerated, but are overridden if threat is high. Individuals who distrust public health institutions are present but marginalized because the constraint's legitimacy depends on institutional credibility for threat assessment. If included as full voices, they would argue that proportionality is a false balance and that threat assessment is captured.
% DISAPPEARANCE_RATIONALE: If proportionality constraint disappeared, authorities would lack a limiting principle on intervention severity. The world would reorganize around either bodily autonomy absolutism (no state medical mandate regardless of threat level) or public health primacy (any intervention justified by any measurable disease risk). Either shift substantially alters disease epidemiology, autonomy distribution, and the institutional authority of public health.
% FOUNDING_PROBLEM: Disease threats vary across orders of magnitude: measles R-value ~12-18, case-fatality ~0.1-0.2%; influenza R-value ~1.3, case-fatality ~0.1%; COVID variants R-value 4-18, case-fatality 0.5-2%; endemic pathogens endemic. Early public health responses treated high-threat and low-threat diseases identically, applying the same apparatus (quarantine, isolation, vaccination mandates) regardless of proportionality. This generated both genuine epidemiological success (eradication of high-threat pathogens, elimination of childhood diseases) and unjustified autonomy violations (mandatory vaccination for low-risk conditions, broad quarantine powers used for non-epidemiological purposes). Autonomy advocates objected to all mandates; public health authorities resisted constraints on their power. The proportionality reading emerged as a framework to calibrate intervention: severity should match threat, exemptions should reflect that match, and authorities should be limited by the principle that more severe threats justify more severe intervention.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health officials attest the founding problem is live: disease threats vary and decisions about intervention scope must account for that variation; they invoke proportionality as a guiding principle in outbreak response. Medical ethicists and constitutional law scholars corroborate: proportionality is a recognized principle in bioethics and constitutional rights review (rights-limiting measures must be necessary and proportional to the legitimate aim across democratic legal systems). Judicial decisions in multiple jurisdictions explicitly reference proportionality as a legitimacy standard (Germany's proportionality review of COVID measures; US courts reviewing vaccine mandates under rational basis review that implicitly applies proportionality logic; Canadian courts applying Oakes test proportionality to public health measures). Bodily autonomy advocates and religious objectors attest that unjustified interventions persist: exemption rules are overly restrictive, mandates persist on low-threat pathogens, and threat assessment appears to be influenced by institutional inertia rather than current evidence. Outside the public health authority, independent medical bodies (AMA, WHO) and civil rights organizations affirm that proportionality is a real constraint on legitimate intervention, not merely that it should be but that it is in fact applied as a limiting principle in actual policy and that violations of proportionality are grounds for legal challenge.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) and conditional: the constraint extracts bodily autonomy from identified groups, but the extraction is justified by threat level. At t=0 (baseline, low-threat disease scenario) extractiveness is low (0.28) because proportionality limits what can be demanded; at t=10-15 (high-threat scenario) extractiveness rises (0.42-0.45) because threat justifies more aggressive intervention; by t=25 (threat trajectory declining or controlled) extractiveness plateaus at the maintained level (0.42) because the intervention architecture persists even as acute threat recedes. Suppression requirement follows threat: it is minimal when threat is low (proportionality allows refusal) and higher when threat is severe (enforcement machinery engages to reach herd immunity threshold). Theater ratio is low throughout (0.12-0.24) because the constraint's function is genuinely coordination (disease prevention) rather than performance — the function is real even when criticized. Resistance is substantial (0.58) because three distinct groups contest the constraint: bodily autonomy absolutists reject any mandate; religious objectors refuse on grounds of conscience; and public health primacy advocates argue proportionality is too restrictive. The constraint faces real resistance because its core premise (proportionality matters) is not universally accepted. Accessibility collapse is moderate (0.61): alternatives (exit to low-intervention jurisdictions, medical exemptions, religious exemptions) exist in principle but are constrained in practice; once the constraint's proportionality logic is understood, individuals see the conditional structure and some accept it as legitimate if threat is high. The measurement series models disease threat trajectory: low (t=0-5, endemic disease phase), rising (t=5-15, outbreak phase), then stabilizing (t=15-25, either controlled or chronic phase). Extractiveness and suppression track threat; theater is low and stable because the function is genuine throughout.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute dramatically different types from the same constraint. From the public health authority's seat (beneficiary, institutional power, analytical exit): this is a rope — genuine coordination with minimal coercive overhead, calibrated to threat, consensual where threat is high and exemptions where threat is low. From the hesitant individual's seat (target, moderate power, constrained exit): this is a snare — the proportionality language is a cover for autonomy extraction that they cannot refuse; the conditional structure is not experienced as legitimate calibration but as arbitrary coercion by those who set threat thresholds they do not trust. From the individual with contraindications' seat (target, powerless, trapped exit): this is pure extraction — they bear full autonomy cost and full disease risk regardless of the constraint's intention; proportionality is meaningless to someone who cannot participate. From the judicial review seat (observer, institutional power, analytical exit): this is tangled rope — genuine coordination function (disease prevention) combined with asymmetric extraction (hesitant individuals and objectors pay), requiring active enforcement, balanced by threat-proportional limits. The engine computes per-seat classification; the claimed type (tangled_rope) represents the analytical observer's assessment. This divergence is the measurement the constraint story exists to capture: the same arrangement produces different structural classifications from different seats precisely because the constraint's legitimacy depends on proportionality, which is contested and not equally believed.
 *
 * DIRECTIONALITY LOGIC:
 *   The public health authority is the structural beneficiary and agenda-setter (d near beneficiary/controller end): they set threat assessment standards, deploy intervention apparatus, and collect legitimacy from proportionality framing. Population at risk are beneficiaries (d low, near beneficiary end): they gain herd immunity protection proportional to intervention coverage. Vaccine-hesitant individuals and religious objectors are targets (d high, near target end): they bear autonomy costs that scale with threat but whom they cannot refuse without accepting disease risk or social exclusion. Individuals with contraindications are extreme targets (d highest): they are trapped — they cannot safely participate and cannot safely refuse. The constraint distributes directionality heterogeneously by disease threat: for low-threat diseases, hesitant individuals appear as targets (extracting their autonomy) while for high-threat diseases their target status is justified by threat magnitude. Healthcare system is beneficiary (d low): it benefits from disease control and planning confidence. Judicial review and surveillance experts are analytical observers (d = 0.5 by default, pure analytical position). Bodily autonomy absolutists are excluded rather than positioned: they reject the constraint's premise, so their d is not computed within this reading — they would appear as targets in a bodily_autonomy_primary constraint story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (calibrating intervention severity to disease threat, preventing unjustified autonomy violations) is live: outbreaks still occur, threat varies by pathogen, and the risk of both under-intervention (allowing preventable disease) and over-intervention (imposing unjustified autonomy costs) is real. The constraint shows no mandatrophy: the proportionality standard is actively applied, threat assessments are regularly updated, and both high-threat and low-threat diseases receive differentiated intervention postures. However, a contestation emerges: different parties disagree on whether proportionality is actually being applied. Public health authorities cite exemptions, staged mandates, and threat-responsive policy as evidence of proportionality compliance. Autonomy advocates and religious objectors cite over-broad exemption restrictions, persistent mandates on low-threat conditions, and lack of individualized threat assessment as evidence of drift into pure extraction. Judicial review (in some jurisdictions) has found some interventions disproportionate and struck down policies that violated proportionality. The constraint is tangled rope, not piton, because the function (disease prevention) is real and the proportionality limit is genuinely operative — it constrains what authorities can do and generates resistance when violated. The constraint is not rope because the extraction is not purely consensual: hesitant individuals and objectors do not accept proportionality as a legitimate limit; they view any mandate as unjustified. The theater ratio (0.22) indicates some performative element (authorities may emphasize proportionality language to increase legitimacy even when threat assessment is not fully objective), but theater is not the primary function — the function is real coordination and the performance is secondary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_assessment_capture,
    'Is threat assessment genuinely independent and evidence-based, or is it captured by political actors, economic interests, or institutional inertia?',
    'Comparative analysis of threat assessment across jurisdictions and pathogens: do jurisdictions with institutionally independent public health agencies assess threat differently than those with direct political control? Do historical threat assessments track actual disease burden (case count, hospitalization, mortality) or do they diverge in predictable directions (over-assessment of politically salient threats, under-assessment of endemic threats)? Post-pandemic review of how threat was assessed during COVID outbreak escalation vs. de-escalation: did assessment follow epidemiological evidence or political timeline?',
    'If threat assessment is captured, proportionality is compromised: the constraint becomes extraction hiding under threat language. If threat assessment is independent, proportionality is legitimate: the constraint genuinely calibrates to real threat. This resolves whether the constraint is tangled rope (coordination + genuine proportionality limit) or snare (threat exaggeration as cover for autonomy extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_capture, empirical, 'Whether proportionality is applied based on objective threat or captured by political interests.').

omega_variable(
    autonomy_absolutism_vs_proportionality_framing,
    'Is proportionality a genuine framework for balancing population harm and individual liberty, or is it a rhetorical cover that bodily autonomy absolutists reject as fundamentally illegitimate?',
    'Phenomenological study of how different parties experience the constraint: do people who accept proportionality actually believe autonomy costs are justified by threat magnitude, or do they comply despite disbelieving proportionality? Do people who reject proportionality do so because they accept bodily autonomy absolutism, or because they distrust threat assessment and believe proportionality is a false balance? Can any intervention severity be sufficiently proportional to be accepted by autonomy absolutists, or is the framework itself rejected regardless of threat level?',
    'If proportionality is a genuine balancing framework widely accepted as legitimate, the constraint is tangled rope from most seats. If proportionality is a rhetorical frame that autonomy absolutists reject on principle and that hesitant individuals reject as false balance, the constraint is snare from those seats regardless of actual threat level. This resolves whether seat divergence is about different legitimate perspectives on the same constraint or about different parties rejecting the constraint''s basic premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_absolutism_vs_proportionality_framing, conceptual, 'Whether proportionality is accepted as a legitimate balancing framework or rejected as illegitimate autonomy violation.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the measured suppression primarily structural (external barriers like occupational exclusion) or internalized (individuals believing proportionality is legitimate and resisting is selfish)?',
    'Post-enforcement trajectory analysis: among individuals who complied with mandates during enforcement, do compliance rates remain high after enforcement mechanisms are removed? If compliance drops sharply, suppression was structural. If compliance persists, suppression is internalized. Longitudinal studies tracking belief change: do individuals shift toward accepting proportionality framing as legitimate over time, or do they remain skeptical but compliant under coercion?',
    'If suppression is internalized, the constraint may become more stable long-term (beliefs persist after external enforcement ends) but at the cost of genuine autonomy reduction. If suppression is structural, enforcement must be continuously maintained to prevent exit; reduction of enforcement capacity will break the constraint quickly. This affects the constraint''s lifecycle and the scope of true resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Whether suppression is structural coercion or internalized belief.').

omega_variable(
    reading_indeterminacy_proportionality_vs_autonomy,
    'Can the same medical intervention be described as legitimate proportionality by one reading and illegitimate autonomy violation by another reading, with no fact about the world deciding between them?',
    'Meta-level framing analysis: is the dispute about facts (threat level, intervention effectiveness, autonomy cost) that could be resolved empirically, or about the normative framework (is proportionality a legitimate principle at all) that is irreducible? If all parties agreed on threat level and autonomy cost, would they still disagree about legitimacy? If yes, the dispute is conceptual, not empirical.',
    'If proportionality is a legitimate reading that coexists with bodily autonomy absolutism, both readings remain valid. If the readings foreclose each other (one rules out the other''s core premise), then one reading will be classified as foreclosed by the engine and the constraint will bifurcate into incompatible interpretations. This affects whether the kernel (legitimate_health_intervention) admits multiple readings or whether only one can be truly legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indeterminacy_proportionality_vs_autonomy, conceptual, 'Whether the proportionality and bodily autonomy readings are genuinely coexistent or logically incompatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__proportionality_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__proportionality_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__proportionality_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__proportionality_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(legi_tr_t25, legitimate_health_intervention__proportionality_reading, theater_ratio, 25, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__proportionality_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__proportionality_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__proportionality_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__proportionality_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(legi_be_t25, legitimate_health_intervention__proportionality_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__proportionality_reading, suppression_requirement, 5, 0.31).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__proportionality_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__proportionality_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__proportionality_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(legi_su_t25, legitimate_health_intervention__proportionality_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).

% DUAL FORMULATION NOTE:
% The legitimate_health_intervention kernel decomposes into three structurally distinct constraints: bodily_autonomy_primary (no state mandate is legitimate regardless of threat), proportionality_reading (this story: legitimacy requires proportional severity), public_health_primary (any measurable benefit justifies mandate). These are not alternative measurements of the same constraint — they have different ε values, different beneficiary/victim structures, and different legitimacy premises. The proportionality reading coexists with bodily autonomy reading (both held by different parties; neither rules out the other within a single party's framework) and influences the public health reading (proportionality constraints public health authority's mandate scope). Each reading should be generated as a separate constraint story with its own ε and classification; they are linked by network.affects_constraints to indicate the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
