% ============================================================================
% CONSTRAINT STORY: cultural_identity_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_identity_gatekeeping, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cultural_identity_gatekeeping
 *   human_readable: Cultural Identity Gatekeeping Mechanism
 *   domain: social/cultural/identity
 *
 * SUMMARY:
 *   Cultural identity gatekeeping comprises structural mechanisms —
 *   institutional, social, and cognitive — through which established cultural
 *   communities regulate access, define authenticity, and control
 *   transmission of practices, knowledge, and identity markers. This
 *   constraint exhibits Tangled Rope structure: it coordinates genuine
 *   transmission of coherent cultural knowledge and protects communities from
 *   appropriation and commercialization, while simultaneously extracting
 *   emotional labor from aspiring practitioners, creating hierarchies within
 *   communities, and suppressing cultural evolution and hybridity. The
 *   extractiveness has increased over 15 years (0.35 → 0.58) as cultural
 *   markets have expanded, making gatekeeping status more economically
 *   valuable and intensifying boundary enforcement. Theater ratio has risen
 *   similarly (0.48 → 0.70), indicating increasing performativity: much
 *   gatekeeping activity has shifted from functional knowledge transmission
 *   to credentialing rituals and authenticity performance. The constraint
 *   demonstrates how identity-locking works: outsiders and marginal
 *   practitioners cannot exit without abandoning the identity they seek,
 *   making their suppression internalized and persistent. From
 *   community-collective perspective, the constraint contains genuine
 *   coordination (diaspora coherence, sacred practice protection) alongside
 *   extraction (exclusion of legitimate claims, suppression of evolving
 *   forms).
 *
 * KEY AGENTS:
 *   - Aspiring Outsiders: Primary victims (powerless/identity_locked) — desire cultural participation but told they lack authenticity; identity bound to seeking inclusion
 *   - Marginal Practitioners: Secondary victims (moderate/constrained) — legitimate but partial cultural claim; face subordinate status and continuous authentication demands
 *   - Cultural Establishment: Primary beneficiary (institutional/arbitrage) — recognized authority, reputation, funding, control over cultural markets and credentials
 *   - Heritage Institutions: Secondary beneficiary (institutional/constrained) — maintain gatekeeping through accreditation but operation is increasingly performative; persist through inertia and legitimacy
 *   - Community Collectives: Mixed (organized/constrained) — perceive genuine coordination function (cultural coherence) alongside extraction; constrained by resource limits and external pressure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional gatekeeping as inherent to cultural identity itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_identity_gatekeeping, 0.58).
domain_priors:suppression_score(cultural_identity_gatekeeping, 0.68).
domain_priors:theater_ratio(cultural_identity_gatekeeping, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_identity_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(cultural_identity_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cultural_identity_gatekeeping, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_identity_gatekeeping, tangled_rope).
narrative_ontology:human_readable(cultural_identity_gatekeeping, "Cultural Identity Gatekeeping Mechanism").
narrative_ontology:topic_domain(cultural_identity_gatekeeping, "social/cultural/identity").

domain_priors:requires_active_enforcement(cultural_identity_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_identity_gatekeeping, cultural_establishment).
narrative_ontology:constraint_beneficiary(cultural_identity_gatekeeping, identity_custodians).
narrative_ontology:constraint_victim(cultural_identity_gatekeeping, cultural_outsiders).
narrative_ontology:constraint_victim(cultural_identity_gatekeeping, authenticity_claimants).
narrative_ontology:constraint_victim(cultural_identity_gatekeeping, marginal_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING CULTURAL OUTSIDER (SNARE) — Identity-locked despite nominal mobility. Outsider desires participation in cultural practice (music, cuisine, spiritual tradition) but is told they lack authenticity, heritage, or proper lineage. Cannot exit the constraint without abandoning the identity they seek. Faces suppression through delegitimization, social exclusion, and constant authentication demands. No coordination benefit perceived — only extraction of emotional labor through proving worthiness.
constraint_indexing:constraint_classification(cultural_identity_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: MARGINAL PRACTITIONER (TANGLED ROPE) — Community member with legitimate but partial claim to cultural practice (mixed heritage, adopted into tradition, self-taught). Benefits from community membership and cultural participation but constrained by subordinate status within the practice hierarchy. Bears extraction: must continuously prove authenticity, limits on teaching/transmitting knowledge, devalued contributions. Also benefits from coordination: norms around cultural transmission provide structure and meaning.
constraint_indexing:constraint_classification(cultural_identity_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CULTURAL ESTABLISHMENT (ROPE) — Institutions, organizations, gatekeepers with recognized authority (cultural councils, heritage organizations, recognized practitioners, cultural media). Experience constraint as coordination: defining authentic boundaries prevents dilution, maintains coherence, enables transmission of genuine practice. Net beneficiary through reputation, funding authority, and arbitrage options in cultural markets.
constraint_indexing:constraint_classification(cultural_identity_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HERITAGE INSTITUTION (PITON) — Museums, universities, cultural preservation organizations. Maintain gatekeeping mechanisms through accreditation, curation, and scholarly authority, but the functional coordination role (preventing authentic knowledge loss) has largely been replaced by theater (credentialing rituals, access exclusivity). The institution persists through funding and legitimacy, not through actual verification that gatekeeping preserves authentic transmission.
constraint_indexing:constraint_classification(cultural_identity_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMMUNITY COLLECTIVE (TANGLED ROPE) — Organized groups of cultural practitioners with shared heritage (diaspora communities, indigenous collectives, cultural associations). Perceive gatekeeping as hybrid: genuine coordination function (maintaining cultural coherence across diaspora, protecting sacred practices from appropriation) combined with extraction (exclusion of those with legitimate claims, suppression of evolving forms, labor extraction from boundary maintenance). Constrained by resource limitations and external pressure to authenticate.
constraint_indexing:constraint_classification(cultural_identity_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From civilizational/universal scope, gatekeeping appears inherent to cultural identity itself: boundaries constitute identity, and maintaining boundaries is a natural law of group coherence. This perspective risks naturalizing a contingent institutional arrangement as inherent to human culture. The mountain classification is diagnostically suspect — reveals that analytical frame may be capturing naturalization rather than structural inevitability.
constraint_indexing:constraint_classification(cultural_identity_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_identity_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cultural_identity_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cultural_identity_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_identity_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cultural_identity_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(cultural_identity_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Cultural gatekeeping mechanisms extract clear benefits for the establishment and custodians: status, funding authority, control over cultural legitimacy, economic returns from credentialing and market access. The extraction is substantial but not total — genuine coordination function exists (maintaining cultural coherence, preventing knowledge loss, protecting sacred practices). The increasing trajectory (0.35 → 0.58) reflects expanding cultural markets and rising economic stakes around identity authenticity. Suppression (0.68): High. Multiple suppression mechanisms: social exclusion of outsiders, delegitimization of claims, economic barriers to participation, information asymmetries about authentic practice, psychological costs of continuous authentication demands. Suppression is substantial for outsiders and marginal practitioners. Theater ratio (0.64): High, increasing. Heritage institutions increasingly maintain gatekeeping through credentialing and accreditation rituals rather than functional knowledge transmission. Museums curate 'authentic' examples; universities grant certificates; cultural organizations issue membership; much of the activity is performative boundary maintenance rather than substantive knowledge transfer. Increasing theater reflects that gatekeeping function has shifted from preventing loss of knowledge (now addressed by digital archiving, multiple transmission pathways) to controlling access and economic benefits.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The cultural establishment experiences gatekeeping as legitimate coordination (Rope) — they perceive themselves as preserving authentic tradition against dilution. The organized community collective perceives hybrid coordination-and-extraction (Tangled Rope) — they value cultural coherence but recognize exclusionary extraction. The marginal practitioner experiences mixed constraint and extraction (Tangled Rope) — benefits from community but faces subordination. The aspiring outsider experiences pure extraction and identity lock (Snare) — no coordination benefit, only suppression and cognitive capture. Heritage institutions experience their own degraded ritual (Piton) — gatekeeping persists through institutional inertia despite loss of functional role. The analytical observer risks naturalizing the entire arrangement as inherent to identity itself (Mountain) — 'cultures need boundaries' — when the boundaries and enforcement mechanisms are contingent institutional structures. The gap is maximum between beneficiary and victim perspectives, revealing that what establishment sees as legitimate coordination appears as pure extraction to the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the constraint. Cultural establishment (institutional/arbitrage) derives low d: beneficiaries with multiple exit options and alternative status sources experience low/negative effective extraction. Aspiring outsiders (powerless/identity_locked) derive high d: lack exit capacity (identity-locked to seeking participation) and bear suppression costs; experience maximum extraction. Marginal practitioners (moderate/constrained) derive moderate-high d: structurally mobile (could leave community) but constrained by belonging need and internalized hierarchy; experience significant extraction. Community collectives (organized/constrained) derive moderate d: have some organizational power and alternatives (could defend practices through other means) but constrained by resource limits and external pressure; perceive mixed coordination and extraction. The identity_locked exit option is critical: it distinguishes outsiders whose suppression is primarily internalized (they believe the gatekeeping judgment; identity fused with seeking legitimacy) from trapped agents facing purely material barriers. The piton institutional perspective derives constrained exit because heritage institutions are locked into maintaining gatekeeping through funding and legitimacy dependencies, even though the functional coordination role has degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION REQUIRED: Cultural identity gatekeeping should be decomposed into at least three structurally distinct constraints with different ε values: (1) Knowledge transmission coordination (ε ≈ 0.15, Rope) — genuine coordination function of passing skills and understanding within a coherent tradition; (2) Boundary maintenance mechanism (ε ≈ 0.45, Tangled Rope) — mixed coordination (preventing sacred practice misuse) and extraction (controlling economic/status benefits of cultural authority); (3) Credential gatekeeping (ε ≈ 0.72, Snare) — largely extractive theater where gatekeepers control access to cultural legitimacy for economic/status benefit with minimal coordination function. Each would have different beneficiaries/victims and would classify differently. This narrative presents the unified constraint story but mandatrophy resolution requires decomposition. The unified version (ε=0.58, Tangled Rope) correctly captures the hybrid structure and prevents misclassifying a clearly extractive mechanism (gatekeeping) as pure coordination (Rope) — the classical mandatrophy error where institutional beneficiaries' framing (we're preserving culture) is accepted uncritically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_verification_impossibility,
    'Can authenticity of cultural practice actually be verified objectively, or is all authenticity assessment inherently subjective and power-dependent?',
    'Analysis of verification criteria used by different gatekeeping institutions; identification of contradictions between criteria; examination of how criteria change over time and across communities',
    'If objective verification possible: gatekeeping may be legitimate coordination. If purely subjective: gatekeeping is necessarily extractive (classification shifts from Tangled Rope toward Snare). If verification is community-dependent: different communities'' gatekeeping mechanisms constitute separate constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authenticity_verification_impossibility, conceptual, 'Whether authenticity assessment is verifiable or inherently subjective').

omega_variable(
    identity_lock_mechanism,
    'Is the observed suppression of outsiders due to material barriers (economic access, geographic isolation, information access) or primarily due to internalized cognitive frames (identity fusion with gatekeeping judgments, belief in illegitimacy)?',
    'Longitudinal study of outsiders who gained cultural access; measurement of suppression trajectory post-access; analysis of whether suppression beliefs persist after material barriers removed',
    'If primarily material: reclassify exit_options from identity_locked to trapped or constrained. If primarily cognitive: identity_locked is accurate; suppression is internalized and carried post-exit. Mixed case indicates that suppression is layered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether suppression is structural or internalized cognitive capture').

omega_variable(
    cultural_evolution_vs_authenticity,
    'Does gatekeeping prevent cultural evolution and adaptation (extraction mechanism) or enable preservation of coherent tradition (coordination mechanism)?',
    'Comparative analysis of gated vs non-gated cultural traditions; measurement of innovation rates, transmission success, and community satisfaction; examination of historical evolution within traditions during periods of strict gatekeeping vs periods of boundary loosening',
    'If gatekeeping prevents evolution: strengthens Snare classification. If gatekeeping enables stable transmission: strengthens Rope classification. If mixed: Tangled Rope classification is correct, but ratio of coordination to extraction varies by community and time period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_evolution_vs_authenticity, empirical, 'Whether gatekeeping enables or prevents cultural evolution').

omega_variable(
    appropriation_real_harm,
    'Does commercial/disrespectful appropriation of gated cultural practices actually cause measurable harm to originating communities, or is the harm primarily epistemic/symbolic?',
    'Longitudinal economic and social impact analysis of appropriation events; measurement of community member outcomes; comparison with counterfactual scenarios where appropriation occurred but gatekeeping did not',
    'If material harm substantial: gatekeeping may be justified protection (shifts toward Rope). If primarily symbolic/epistemic: gatekeeping appears extractive (shifts toward Snare). If both: the constraint is defense-oriented but enforced through extraction mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriation_real_harm, empirical, 'Whether appropriation causes material or primarily symbolic harm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_identity_gatekeeping, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cultig_tr_t0, cultural_identity_gatekeeping, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cultig_tr_t5, cultural_identity_gatekeeping, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cultig_tr_t10, cultural_identity_gatekeeping, theater_ratio, 10, 0.64).
narrative_ontology:measurement(cultig_tr_t15, cultural_identity_gatekeeping, theater_ratio, 15, 0.7).

% Extraction over time
narrative_ontology:measurement(cultig_be_t0, cultural_identity_gatekeeping, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cultig_be_t5, cultural_identity_gatekeeping, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cultig_be_t10, cultural_identity_gatekeeping, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cultig_be_t15, cultural_identity_gatekeeping, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_identity_gatekeeping, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_identity_gatekeeping, 0.12).
narrative_ontology:affects_constraint(cultural_identity_gatekeeping, cultural_appropriation_dynamics).
narrative_ontology:affects_constraint(cultural_identity_gatekeeping, diaspora_cohesion_maintenance).
narrative_ontology:affects_constraint(cultural_identity_gatekeeping, professional_credentialing_extraction).

% DUAL FORMULATION NOTE:
% Cultural gatekeeping is upstream of specific appropriation dynamics and downstream of diaspora coherence needs. Decomposition into knowledge_transmission_coordination, boundary_maintenance_hybrid, and credential_gatekeeping_snare recommended for precise classification. Each member of the family has different ε values, different beneficiary/victim structures, and different Boltzmann compliance signatures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_identity_gatekeeping, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
