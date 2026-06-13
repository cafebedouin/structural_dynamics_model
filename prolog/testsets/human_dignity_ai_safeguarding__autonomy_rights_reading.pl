% ============================================================================
% CONSTRAINT STORY: human_dignity_ai_safeguarding__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_dignity_ai_safeguarding__autonomy_rights_reading, []).

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
 *   constraint_id: human_dignity_ai_safeguarding__autonomy_rights_reading
 *   human_readable: Human Dignity via Autonomy, Rationality, and Rights (AI Safeguarding Reading)
 *   domain: theological/philosophical/technological
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'human_dignity_ai_safeguarding': the autonomy-rights reading grounds
 *   dignity in demonstrable human capacities (rational agency, autonomy,
 *   rights-bearing status) rather than in metaphysical claims (divine image)
 *   or in posthuman potentiality (transhumanist enhancement vision).
 *   Regulatory frameworks aligned with this reading prioritize transparency,
 *   informed consent, labor protection, and cautious enhancement within
 *   rights-respecting constraints. The reading is coordinating (solves a
 *   genuine collective-action problem in global AI governance by providing a
 *   secular, pluralistic foundation) but asymmetrically extractive:
 *   enhancement-seekers and frontier-capability researchers bear significant
 *   compliance and opportunity costs while transparency-advocates and
 *   labor-protection bodies gain institutional legitimacy and veto power.
 *   Suppression is moderate: the constraint's enforcement depends on actively
 *   restraining enhancement research and transhumanist voices from
 *   co-authoring the framework's foundational premises, but does not require
 *   continuous coercion of individuals—the suppression is architecturally
 *   embedded in whose epistemic voice counts.
 *
 * KEY AGENTS:
 *   - liberal_rights_tradition (institutional agenda-setter): sets autonomy-rights framing; derives legitimacy from Enlightenment philosophy and international human rights law; enforces through regulation, law, and institutional gatekeeping
 *   - transparency_advocates (organized beneficiary): win regulatory mandates requiring algorithmic disclosure; benefit from systematic recognition of human autonomy as dignity-protection claim
 *   - labor_protection_constituency (organized beneficiary): gain worker-safeguarding regulations framed as dignity protection; institutional position legitimated by the framework
 *   - enhancement_seeking_individuals (moderate-power payer, identity-locked exit): experience the constraint as limit on self-modification aspiration; cannot exit jurisdictions without losing rights protections
 *   - experimental_ai_research (powerful-but-payer arbitrager): pay compliance costs within rights-respecting jurisdictions; exit to unregulated zones when overhead becomes unsustainable
 *   - transhumanist_constituencies (excluded, identity-locked): structurally prevented from co-authoring framework; their central vision (human transcendence through capability expansion) is foreclosed by the framework's dignity definition
 *   - imago_dei_advocates (excluded, mobile exit): religious traditions with different dignity grounding; can live according to their own standards but framework does not recognize their reading as legitimate in global governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.38).
domain_priors:suppression_score(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.42).
domain_priors:theater_ratio(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(human_dignity_ai_safeguarding__autonomy_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_dignity_ai_safeguarding__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(human_dignity_ai_safeguarding__autonomy_rights_reading, "Human Dignity via Autonomy, Rationality, and Rights (AI Safeguarding Reading)").
narrative_ontology:topic_domain(human_dignity_ai_safeguarding__autonomy_rights_reading, "theological/philosophical/technological").

domain_priors:requires_active_enforcement(human_dignity_ai_safeguarding__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_dignity_ai_safeguarding__autonomy_rights_reading, 'f011a40e-dc7e-4677-b841-1116c3d7dd6e').
narrative_ontology:cs_kernel_codification('f011a40e-dc7e-4677-b841-1116c3d7dd6e', formalized).
narrative_ontology:cs_authority_grounding('f011a40e-dc7e-4677-b841-1116c3d7dd6e', lineage).
narrative_ontology:cs_interpretation_layer_present('f011a40e-dc7e-4677-b841-1116c3d7dd6e').
narrative_ontology:cs_reading_relation('f011a40e-dc7e-4677-b841-1116c3d7dd6e', human_dignity_ai_safeguarding__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('f011a40e-dc7e-4677-b841-1116c3d7dd6e', human_dignity_ai_safeguarding__posthumanist_reading, influences).
narrative_ontology:cs_axiom('f011a40e-dc7e-4677-b841-1116c3d7dd6e', foundational, human_rationality_as_dignity_ground).
narrative_ontology:cs_axiom_status(human_rationality_as_dignity_ground, holdable).
narrative_ontology:cs_axiom_grounding('f011a40e-dc7e-4677-b841-1116c3d7dd6e', human_rationality_as_dignity_ground, empirically_contingent).
narrative_ontology:cs_axiom('f011a40e-dc7e-4677-b841-1116c3d7dd6e', foundational, autonomy_as_necessary_dignity_protection).
narrative_ontology:cs_axiom_status(autonomy_as_necessary_dignity_protection, holdable).
narrative_ontology:cs_axiom_grounding('f011a40e-dc7e-4677-b841-1116c3d7dd6e', autonomy_as_necessary_dignity_protection, deontological).
narrative_ontology:cs_reference_frame('f011a40e-dc7e-4677-b841-1116c3d7dd6e', enlightenment_secular_dignity_framework).
narrative_ontology:cs_drift_state('f011a40e-dc7e-4677-b841-1116c3d7dd6e', contemporary_posthuman_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f011a40e-dc7e-4677-b841-1116c3d7dd6e', '').
narrative_ontology:cs_kernel_id(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, transparency_advocates).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, labor_protection_constituency).
narrative_ontology:constraint_beneficiary(human_dignity_ai_safeguarding__autonomy_rights_reading, liberal_rights_tradition).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, enhancement_seeking_individuals).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, experimental_ai_research_programs).
narrative_ontology:constraint_victim(human_dignity_ai_safeguarding__autonomy_rights_reading, transhumanist_constituencies).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_dignity_ai_safeguarding__autonomy_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(human_dignity_ai_safeguarding__autonomy_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).
:- end_tests(human_dignity_ai_safeguarding__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint coordinates real governance-function (avoiding capability race, protecting worker agency) while simultaneously transferring decision-making power from researchers to regulators and opportunity costs from rights-constituencies to enhancement-seekers. Suppression is moderate-rising (0.25→0.42) because enforcement initially centers on positive mandates (transparency, consent procedures) but over time requires increasing active restraint of enhancement research directions and transhumanist legitimacy claims that threaten the framework's foundational assumption (dignity = rationality). Theater is low-moderate (0.28): the transparency and consent procedures are functionally real, but a growing portion of enforcement effort defends the framework's exclusion of competing dignity readings rather than serving the human-protection function directly. The measurement series show suppression rising and extractiveness plateauing as the regime matures—the coordination function is stable but the cost of maintaining framework-epistemic closure (preventing transhumanist and imago-dei readings from becoming co-legitimate) increases over time.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute as experiencing genuine coordination; payer seats (researchers, enhancement-seekers) should compute as experiencing enforced extraction with constrained exit. The transparency_advocates and labor constituencies benefit from the legitimacy apparatus itself—the constraint vindicates their institutional role as dignity-protectors. The enhancement-seekers experience the same rules as a ceiling on their own rational choices, enforced by others' conception of what dignity permits. The engine computes this divergence from the power/exit/beneficiary structural data; the divergence is the measurement the analysis exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: liberal_rights_tradition (agenda_setter + institutional power + arbitrage exit = high d toward beneficiary end); transparency_advocates (organized beneficiary + mobile exit = low d); labor_protection (organized beneficiary + constrained exit but benefiting from institutional legitimacy = low-moderate d); enhancement_seekers (moderate power + identity_locked exit + victim group = high d toward target end); researchers (powerful but payer + arbitrage exit via relocation = moderate d, mobile enough to avoid full-target status but still bearing primary compliance costs); transhumanists (moderate power + identity_locked + excluded voice = high d target-side, but secondary effect: they are excluded, so the constraint does not govern them directly—they are prevented from participating in norm-setting, not from living according to their own standards).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy (founding problem → dead problem → inert apparatus) by design: the founding problem 'how do we coordinate global AI governance on dignity protections without sectarian metaphysics' remains live because the governance apparatus must continuously defend itself against alternative dignity readings (imago-dei, posthumanist) seeking to capture the framework. The mandate has not outlived its function; rather, the constraint's persistence depends on continuous enforcement of exclusion. This is the difference between a snare (victim voices are suppressed and the constraint persists) and a tangled_rope (coordination function is real but requires active enforcement to maintain asymmetric extraction in a contested domain). If the suppression stopped—if transhumanist and imago-dei readings were admitted as co-legitimate—the framework would either dissolve or transform into a different constraint entirely (a rope with multiple dignity readings, or a bundle of separate constraints per reading). The fact that continuous enforcement is required to maintain framework-epistemic closure is what makes this tangled_rope, not rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_metaphysical_dignity,
    'Can a dignity framework grounded in human rationality and autonomy withstand challenges that rationality is contingent, historical, and not universally present (infants, persons with cognitive disabilities, persons in altered consciousness states)? Or does dignity require a non-contingent foundation (like imago-dei)?',
    'Philosophical analysis of whether capabilities-based dignity can be extended consistently to non-rational persons without retreating to a metaphysical backup; empirical study of how liberal rights frameworks actually protect non-autonomous populations and whether they rely on hidden metaphysical commitments.',
    'If rationality is shown to be an insufficient or incoherent grounding for universal dignity, the framework''s legitimacy rests on unstated metaphysical premises it claims to avoid. If capabilities-based dignity can be made coherent and inclusive, the autonomy-rights reading strengthens against the imago-dei challenge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_vs_metaphysical_dignity, conceptual, 'Whether autonomy-based dignity is conceptually sufficient or relies on hidden metaphysical assumptions.').

omega_variable(
    enhancement_foreclosure_vs_liberation,
    'Does restraining human enhancement protect human dignity (by preventing loss of autonomy to technological determinism), or does it violate dignity by denying individuals rational self-modification choices?',
    'Empirical study of enhancement outcomes in unregulated jurisdictions and their effects on autonomy and dignity; philosophical analysis of whether autonomy includes the right to transcend one''s current capacities; comparative analysis of individual outcomes in high-regulation vs. permissive enhancement regimes.',
    'If enhancement is shown to preserve or expand autonomy, the framework''s restraint becomes extractive forelosure rather than protective limitation. If enhancement erodes autonomy through technological path-dependency, the restraint is protective. The resolution determines whether the constraint''s suppression of enhancement is dignity-protecting or dignity-violating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enhancement_foreclosure_vs_liberation, empirical, 'Whether enhancement restraint protects or violates autonomy-based dignity.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (preventing enhancement research, excluding transhumanist voices) maintained primarily by structural barriers (institutional gatekeeping, regulatory exclusion) or by internalized acceptance of the autonomy-rights dignity frame?',
    'Post-exit trajectory analysis: enhancement-seekers and transhumanist researchers who relocate to unregulated zones—do their aspirations persist (suppression was structural) or fade (suppression has internalized)? Do transhumanist intellectual communities persist and thrive in private discourse, or do they decline because the frame has been internalized globally?',
    'If suppression is primarily structural, it is theoretically reversible through changing institutions while maintaining the framework. If substantially internalized, the suppression persists even after institutional barriers dissolve and may require active counter-education to reverse. This affects the constraint''s long-term sustainability: structural suppression can be challenged through institutional reform; internalized suppression is more stable (in favor of the constraint) and more difficult to dislodge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether the constraint''s suppression is maintained by institutional gatekeeping or by internalized frame acceptance.').

omega_variable(
    contested_kernel_forelosure_risk,
    'This reading (autonomy-rights) claims to provide secular, pluralistic dignity grounding, but does it actually foreclose the imago-dei and posthumanist readings by defining them as non-legitimate voices, rather than coordinating across them?',
    'Examine the international governance apparatus''s treatment of imago-dei and posthumanist advocacy: are they welcomed as legitimate alternative readings of dignity, or are they excluded from official deliberation and framed as non-serious? Do the governance bodies treat this as a pluralistic framework accommodating multiple readings, or as a universal framework that admits no legitimate rivals?',
    'If the autonomy-rights reading claims pluralism but actually operates as exclusion, the constraint is more extractive and less coordinating than authored. If it genuinely accommodates multiple readings in deliberative space, it is more purely coordinating. This is a kernel-level question about whether the framework is what it claims to be.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_kernel_forelosure_risk, empirical, 'Whether the autonomy-rights reading coordinates across dignity conceptions or forecloses legitimate rivals while claiming pluralism.').

omega_variable(
    research_arbitrage_sustainability,
    'How long can experimental AI research sustain arbitrage between high-regulation and low-regulation jurisdictions before global regulatory harmonization eliminates the exit option?',
    'Monitor international AI governance negotiations, track research relocation patterns, assess whether emerging technology powers (China, India, others) adopt autonomy-rights frameworks or maintain independent regulations. If harmonization occurs, the powerful-but-arbitraging researcher seat becomes trapped.',
    'If arbitrage exit closes, researchers move from payer→trapped and extraction increases substantially (effective d moves toward 1.0). If multiple regulatory frameworks persist globally, arbitrage remains viable and extraction stays moderate. Closure of arbitrage also increases suppression_requirement (more enforcement needed to contain capability race across jurisdictions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_arbitrage_sustainability, empirical, 'Whether researcher arbitrage exit remains viable or closes through global regulatory convergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_dignity_ai_safeguarding__autonomy_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(huma_tr_t5, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(huma_tr_t10, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(huma_tr_t15, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(huma_tr_t20, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(huma_tr_t25, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(huma_tr_t30, human_dignity_ai_safeguarding__autonomy_rights_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(huma_be_t5, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement(huma_be_t10, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(huma_be_t15, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(huma_be_t20, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(huma_be_t25, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(huma_be_t30, human_dignity_ai_safeguarding__autonomy_rights_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(huma_su_t5, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(huma_su_t10, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(huma_su_t15, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(huma_su_t20, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(huma_su_t25, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(huma_su_t30, human_dignity_ai_safeguarding__autonomy_rights_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_dignity_ai_safeguarding__autonomy_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(human_dignity_ai_safeguarding__autonomy_rights_reading, 0.12).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__imago_dei_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, human_dignity_ai_safeguarding__posthumanist_reading).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, ai_labor_displacement_safeguards).
narrative_ontology:affects_constraint(human_dignity_ai_safeguarding__autonomy_rights_reading, algorithmic_transparency_requirements).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel human_dignity_ai_safeguarding. The imago_dei_reading (divine image grounding) and posthumanist_reading (capability-nonspecific personhood) are sibling readings within the same kernel family. Each reading has distinct ε, distinct beneficiary/victim structures, and distinct type classification. All three stories must be authored separately per ε-invariance; they are linked via network.affects_constraints to enable contamination-propagation analysis. The autonomy-rights reading is foundational in global AI governance (encoded in UN instruments, regional human rights law) and influences the other readings by setting the default legitimacy frame; the imago-dei and posthumanist readings exert pressure back by challenging the frame's sufficiency. See commentary.kernel_context for the reading-relations topology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(human_dignity_ai_safeguarding__autonomy_rights_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
