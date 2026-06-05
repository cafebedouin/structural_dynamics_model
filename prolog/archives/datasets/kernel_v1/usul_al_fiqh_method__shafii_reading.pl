% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i Jurisprudential Method: Hierarchical Four-Source Authority
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The Shafi'i jurisprudential method grounds legal authority in an explicit
 *   four-source hierarchy: Qur'an (primary), Sunnah/Hadith (secondary), Ijma'
 *   (scholarly consensus, tertiary), and Qiyas (analogical reasoning,
 *   quaternary). This reading explicitly rejects istihsan (juristic
 *   preference) and 'urf (local custom) as valid sources. This constraint
 *   exemplifies how a formalized methodology can simultaneously enable
 *   scholarly coordination and extract from those whose legitimacy frameworks
 *   (customary practice, contextual reasoning) fall outside the hierarchy.
 *   The Shafi'i method was systematized by al-Shafi'i (8th century) as a
 *   response to perceived laxity in other jurisprudential schools and as an
 *   effort to anchor legal authority in documented sources rather than
 *   personal opinion. The method represents a genuine coordination
 *   achievement — it provides transmissible, defensible reasoning chains that
 *   enable schools of law to cohere across generations and geography. Yet the
 *   same mechanism that enables coordination (elevating hadith expertise and
 *   methodological strictness) creates victims: rural practitioners whose
 *   customary norms are foreclosed, jurists who see contextual injustice that
 *   istihsan could address, and moderates who find qiyas constraints too
 *   brittle for novel circumstances. The constraint exhibits six distinct
 *   classifications depending on the observer's structural position, making
 *   it a diagnostic exemplar of how commitment-system constraints generate
 *   perspectival gaps. The theater ratio (0.52) reflects that over centuries,
 *   the method became increasingly ritualized — formal invocation of
 *   'following the sources' sometimes obscured rather than enabled
 *   substantive jurisprudential reasoning. Yet the foundational axiom
 *   (exclusive authority in documented sources) remains internally coherent
 *   and held across generations of Shafi'i scholars.
 *
 * KEY AGENTS:
 *   - Hadith Scholar Class (institutional/arbitrage): Primary beneficiary — Sunnah expertise becomes the gated source of jurisprudential legitimacy. Controls authentication of Prophetic tradition and therefore shapes which rulings can be justified.
 *   - Rural Custom Practitioners (powerless/trapped): Primary victim — local 'urf is explicitly foreclosed. Face high costs to exit the hierarchy and invoke locally developed norms.
 *   - Moderate Jurists Seeking Flexibility (moderate/constrained): Secondary victim — constrained by rejection of istihsan and overly tight qiyas bounds. Work within the system but cannot invoke reasoning tools available in Hanafi/Maliki schools.
 *   - Customary Law Preservationists (organized/constrained): Organized victims — see customary legal traditions as legitimate and functional but are pressed into conformity or parallel systems.
 *   - Shafi'i School Institution (institutional/arbitrage): Maintains the methodological hierarchy through educational transmission and institutional reproduction.
 *   - Analytical Observer (analytical/analytical): At civilizational scope, risks naturalizing the four-source hierarchy as divine law rather than medieval scholarly construction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.35).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.48).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i Jurisprudential Method: Hierarchical Four-Source Authority").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, 'urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b').
narrative_ontology:cs_kernel_codification('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', formalized).
narrative_ontology:cs_authority_grounding('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', lineage).
narrative_ontology:cs_interpretation_layer_present('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b').
narrative_ontology:cs_reading_relation('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', foundational, documented_sources_exhaustive_authority).
narrative_ontology:cs_axiom_status(documented_sources_exhaustive_authority, holdable).
narrative_ontology:cs_axiom_grounding('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', documented_sources_exhaustive_authority, deontological).
narrative_ontology:cs_axiom('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', foundational, istihsan_juristic_preference_foreclosed).
narrative_ontology:cs_axiom_status(istihsan_juristic_preference_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', istihsan_juristic_preference_foreclosed, deontological).
narrative_ontology:cs_reference_frame('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', divine_source_hierarchy_quranic_primacy).
narrative_ontology:cs_drift_state('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', contemporary_islamic_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('urn:uuid:a47f8c2d-9b1e-4a3c-b2e1-7c9d3f5a8e2b', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_scholar_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, methodological_consistency_doctrine).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, customary_law_practitioners).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, local_jurisprudential_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL PRACTITIONER (SNARE) — Local custom ('urf) is rejected as invalid legal source. The practitioner faces high costs to exit: cannot invoke locally legitimate norms, must adhere to hierarchy imposed from external scholarly authority. Structurally trapped — customary practices that evolved to address local conditions are explicitly foreclosed. Maximum experienced extraction with minimal flexibility or appeal.
constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MODERATE JURIST (TANGLED ROPE) — Constrained by the four-source hierarchy but also benefits from its clarity and transmissible structure. Can work within the system through careful hadith curation or analogical reasoning (qiyas), but cannot invoke istihsan (juristic preference) or local custom. Mixed extraction and coordination — the hierarchy enables certainty and scholarly authority but forecloses adaptive reasoning in novel circumstances.
constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HADITH SCHOLAR CLASS (ROPE) — Primary beneficiaries. Hadith expertise becomes the gated source of jurisprudential legitimacy. The four-source hierarchy elevates Sunnah (Prophetic tradition) as the second pillar after Qur'an, creating institutional value for those who control authenticated hadith collections. The constraint solves the coordination problem of legitimate jurisprudential authority but concentrates power in the hadith-scholarly class. Positive-sum for the authority class — extraction runs toward this agent.
constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SHAFI'I SCHOOL INSTITUTION (PITON) — Over centuries, the four-source method became increasingly ritualized. Scholars apply the hierarchy mechanically without investigating whether its epistemic foundations hold in novel contexts. Theater ratio (0.52) reflects that the method's performative invocation of 'following the sources' sometimes obscures rather than enables substantive jurisprudential reasoning. The institution persists through educational transmission and institutional inertia, not because the method remains optimally functional.
constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HANAFI REFORMIST COALITION (TANGLED ROPE) — Organized agents (Hanafi jurists, istihsan-sympathetic schools) see the Shafi'i hierarchy as overly rigid. They benefit from the existence of a formalized jurisprudential framework but are constrained by its foreclosure of juristic preference (istihsan) and custom ('urf). The constraint creates coordination costs for cross-school reasoning. Mixed extraction and coordination — the hierarchy enables some dialogue but prevents fuller collaboration.
constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, the four-source hierarchy appears as an immutable consequence of divine law: if Qur'an is the revealed word of God, then sources derivative of it (Sunnah, consensus, analogy) naturally follow in hierarchical order. The constraint appears as natural law, not constructed institutional choice. However, the structural data reveals beneficiaries (hadith scholars) and victims (custom practitioners), signaling false summit: the 'natural' hierarchy serves specific epistemic interests.
constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usul_al_fiqh_method__shafii_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, TR),
    TR >= 0.70.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts from those whose jurisprudential legitimacy depends on rejected sources (custom practitioners, contextual reasoners) but does not extract maximally — the method enables genuine coordination within its own frame, and moderate agents can work within qiyas constraints. The value reflects that the extraction is real but mediated by the coordination function. Suppression (0.48): Moderate-high. Significant barriers to invoking alternative reasoning (explicit rejection of istihsan/urf), but suppression is not total — qiyas provides a permitted avenue for adaptation, and parallel systems persist. Theater ratio (0.52): Moderate. The method is not primarily performative (unlike some institutional applications) — hadith authentication involves genuine scholarly work, and qiyas reasoning requires substantive engagement. But the ratio rises over time (0.38 → 0.52) as ritualization increases, reflecting that later applications became more mechanical than early formulations. The baseline theater reflects that the hierarchy's invocation ('following the sources') sometimes obscures rather than enables transparent reasoning.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across six distinct positions. The hadith scholar class sees rope (pure coordination achieving scholarly authority). The rural practitioner sees snare (extraction with no alternatives). The moderate jurist sees tangled rope (mixed coordination and constraint). The Hanafi reformist coalition sees tangled rope (benefits from coordination but constrained by rigidity). The institution sees piton (degraded ritual). The analytical observer risks seeing mountain (natural law from divine sources) — but the structural data reveals beneficiaries and victims, signaling false summit. The perspectival gap reveals that the constraint's classification depends entirely on whether the observer's jurisprudential legitimacy framework falls inside or outside the four-source hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from agent power level, exit options, and beneficiary/victim status. The hadith scholar class (institutional/arbitrage) has minimal d — beneficiary status with exit options means low extraction toward this agent. The rural practitioner (powerless/trapped) has maximum d (0.95+) — victim status with no exit means the full extraction vector bears on this agent. The moderate jurist (moderate/constrained) has medium-high d — victim of foreclosure but with some adaptive capacity via qiyas, yielding medium extracted experience. The analytical observer (analytical/analytical) has high d by canonical fallback (0.73) reflecting the epistemic position of external analysis that cannot fully inhabit any single school's framework. The engine computes chi by applying f(d) and scope modifiers to base extractiveness; perspectives with high d see higher experienced extraction despite moderate base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the Shafi'i method is genuinely tangled rope — it is not possible to reduce it to pure coordination (rope) by ignoring the rejection of istihsan/urf, nor to reduce it to pure extraction (snare) by ignoring the coordination function of the four-source hierarchy. The method simultaneously solves a real coordination problem (establishing defensible jurisprudential authority across space and time) and extracts from those whose reasoning frameworks fall outside its hierarchical bounds. The mandatrophy resolution requires acknowledging both functions as structural, not dismissing one as accidental or secondary. The false summit classification at the analytical perspective reveals the risk of naturalizing the hierarchy as divine necessity — the engine flags this as a commitment-system constraint where the kernel (whether the four-source hierarchy is obligatory) is actually contested across the sibling schools.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_immutability_vs_derivative_status,
    'Is the four-source hierarchy a divinely-mandated kernel immutable from Islamic law itself, or a methodological choice by medieval jurists interpreting divine sources?',
    'Textual analysis: does the Qur''an or authenticated Sunnah explicitly mandate the four-source hierarchy? Or do medieval jurisprudential texts construct it as interpretive methodology? Historical documentation of debates among early jurists about methodological alternatives.',
    'If kernel is divinely mandated: the Shafi''i method is mountain (immutable). Rejection of istihsan/urf is obligatory, not contingent. If kernel is scholarly construction: the Shafi''i method is tangled_rope or snare (contingent, beneficiary-serving). Alternative methodologies (Hanafi, Maliki) are equally valid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_immutability_vs_derivative_status, conceptual, 'Whether the four-source hierarchy is divinely mandated or scholarly-constructed').

omega_variable(
    hadith_authenticity_verification,
    'Does the Shafi''i method''s reliance on hadith-based Sunnah rest on reliable hadith authentication standards, or do gaps in chain-of-transmission (isnad) criticism enable insertion of post-Prophetic material masquerading as authentic tradition?',
    'Comparative study of hadith authentication standards across hadith sciences (ilm al-hadith). Analysis of historically disputed hadith material and mechanisms of spurious attribution. Assessment of whether the isnad system reliably filters for Prophetic authenticity.',
    'If hadith authentication is reliable: second-source pillar is epistemically sound, and hierarchy is legitimate. If authentication gaps exist: the method concentrates power in scholars controlling narratives of authenticity, converting hadith expertise into gatekeeping authority. Extractiveness classification shifts upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authenticity_verification, empirical, 'Reliability of hadith authentication standards in isolating authentic Sunnah').

omega_variable(
    istihsan_foreclosure_justification,
    'Does the Shafi''i rejection of istihsan (juristic preference) serve a genuine epistemic function — preventing arbitrary decision-making — or does it foreclose contextual reasoning that Hanafi and Maliki jurisprudence shows is possible within disciplined constraints?',
    'Comparative analysis of istihsan-based rulings in Hanafi/Maliki schools versus Shafi''i qiyas-only decisions on identical cases. Documentation of whether istihsan rulings show systematic bias or arbitrary application. Assessment of whether the constraint against istihsan prevents genuine adaptive jurisprudence.',
    'If istihsan-without-discipline is unmanageable: Shafi''i rejection is justified, and the foreclosure serves integrity. If istihsan can be disciplined: the foreclosure is arbitrary, and the constraint is extractive (concentrating power in hadith scholars over contextual reasoners).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_foreclosure_justification, empirical, 'Whether istihsan rejection serves epistemic integrity or forecloses legitimate reasoning').

omega_variable(
    urf_local_custom_legitimacy,
    'Is the rejection of ''urf (local custom) as jurisprudential source epistemically justified (custom is unreliable, shifting, parochial) or does it eliminate a legitimate source of law-formation that reflects lived community needs?',
    'Historical analysis of societies under Shafi''i law: do rural and local communities experience the constraint as an improvement in justice, or as imposition of external reasoning disconnected from contextual conditions? Documentation of parallel legal systems (customary courts, regional variation in practice) that persist despite Shafi''i doctrine.',
    'If custom-rejection improves justice: the constraint is legitimate coordination. If custom-rejection produces parallel systems: the constraint creates victims without delivering benefits — extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_local_custom_legitimacy, empirical, 'Whether ''urf rejection improves justice or produces parallel legal systems').

omega_variable(
    ijma_consensus_authenticity,
    'How are claims of scholarly consensus (ijma'') verified and enforced within the Shafi''i framework? Does consensus genuinely represent the scholarly community, or do powerful voices claim consensus falsely while minorities are excluded?',
    'Documentation of specific ijma'' claims and the dissenting voices historically present. Analysis of consensus-formation mechanisms (e.g., how many scholars must agree, from which regions, which generations). Assessment of whether consensus terminology masks factional dominance.',
    'If ijma'' is authentically consensual: the third source is legitimate. If ijma'' masks factional dominance: the method enables false universalization of particular positions. Extractiveness and suppression increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_consensus_authenticity, empirical, 'Verification and authenticity of ijma'' (consensus) claims in Shafi''i method').

omega_variable(
    qiyas_scope_constraints,
    'What limits constrain analogical reasoning (qiyas) within the Shafi''i framework, and do these constraints prevent legitimate extension to novel cases or do they function as intended safeguards?',
    'Analysis of classical Shafi''i texts on qiyas rules. Documentation of analogical reasoning in practice: cases where qiyas produced widely accepted rulings versus cases where qiyas was challenged for overreach. Assessment of whether constraints enable or stifle responsive jurisprudence.',
    'If qiyas constraints are well-calibrated: moderate agents can adapt within the system, and tangled_rope classification holds. If qiyas constraints are overly tight: the method becomes brittle, and the hierarchy concentrates power more fully in hadith scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_scope_constraints, empirical, 'Whether qiyas constraints enable or stifle responsive jurisprudence').

omega_variable(
    sibling_reading_kernel_identity,
    'Do the Hanafi, Maliki, and Hanbali readings interpret the same kernel (usul al-fiqh methodology) or do they invoke incompatible kernels that merely share a family name?',
    'Textual comparison of foundational texts across schools. Analysis of whether disagreements are methodological (same kernel, different interpretation) or kernel-level (different commitments about what counts as valid jurisprudential source). Assessment of whether a single scholar could coherently hold positions from multiple schools.',
    'If same kernel, different readings: coexists_with relation is appropriate. If different kernels: the relation should be forecloses or influences. This affects whether the schools represent a single contested domain or incompatible frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_identity, conceptual, 'Whether sibling schools read the same kernel or invoke incompatible kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shafii_theater_early, usul_al_fiqh_method__shafii_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(shafii_theater_mid, usul_al_fiqh_method__shafii_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(shafii_theater_late, usul_al_fiqh_method__shafii_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(shafii_extract_early, usul_al_fiqh_method__shafii_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(shafii_extract_mid, usul_al_fiqh_method__shafii_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(shafii_extract_late, usul_al_fiqh_method__shafii_reading, base_extractiveness, 6, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, istihsan_juristic_preference_doctrine).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, urf_custom_law_integration).

% DUAL FORMULATION NOTE:
% This story is one reading of a shared kernel (usul_al_fiqh_method) instantiated by four distinct jurisprudential schools. Each school produces a different constraint with different epsilon values, beneficiary/victim structures, and classification. Epsilon values differ because the scope of rejected sources differs: Shafi'i rejects two (istihsan, urf), Hanafi rejects one (partial istihsan), Maliki permits both, Hanbali rejects more strictly than Shafi'i. The stories are linked via network.affects_constraints and cs_structure.reading_relations to model the kernel as contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__shafii_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
