% ============================================================================
% CONSTRAINT STORY: relationship_pathologization_discourse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_relationship_pathologization_discourse, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: relationship_pathologization_discourse
 *   human_readable: Relationship Pathologization Discourse as Extractive Framing
 *   domain: psychology/interpersonal/social
 *
 * SUMMARY:
 *   Relationship pathologization discourse is the institutional practice of
 *   reframing relationship dynamics, conflicts, and adaptive responses as
 *   individual psychological disorders or pathological attachment styles.
 *   This constraint manifests as a tangled rope: genuine coordination
 *   function (enables conversation about relational dysfunction, creates
 *   framework for intervention) embedded in asymmetric extraction (displaces
 *   contextual and systemic understanding, medicalizes normal relationship
 *   variation, justifies professional authority and pharmaceutical
 *   intervention, creates identity-locked targets who internalize
 *   pathological self-concepts). The constraint has intensified over 20 years
 *   (theater_ratio rising from 0.45 to 0.68) as diagnostic categories have
 *   proliferated (anxious attachment, avoidant attachment, codependency, love
 *   addiction, relationship addiction) and therapeutic framings have
 *   colonized increasingly normal relational phenomena. The pathologized
 *   partner experiences the constraint as snare: identity-locked despite
 *   structural mobility (could leave the relationship but cannot imagine
 *   themselves outside the identity framework created by the
 *   pathologization), facing high suppression (alternative narratives are
 *   delegitimized by professional authority). The relationship complexity
 *   recognition movement experiences it as tangled rope: the discourse
 *   enables recognition of genuine relational harm while simultaneously
 *   displacing understanding of systemic and structural factors. Mental
 *   health professionals and diagnostic framework maintainers experience it
 *   as rope: genuine coordination function for their work alongside arbitrage
 *   exit options if paradigms shift.
 *
 * KEY AGENTS:
 *   - Pathologized Partner: Primary victim (powerless/identity_locked) — internalizes diagnostic frame as identity; structurally mobile but identity-fused with the pathological role
 *   - Partner Outside the Frame: Secondary victim (moderate/constrained) — experiences both benefits (explanation for partner's behavior) and costs (managing emotional labor, bearing extracted responsibility for relationship success)
 *   - Mental Health Professional: Primary beneficiary (institutional/arbitrage) — receives steady demand for services, validated expertise, reimbursable diagnoses; experiences constraint as coordination
 *   - Diagnostic Framework Maintainer: Primary beneficiary (institutional/arbitrage) — derives revenue and authority from maintaining and updating pathologization categories
 *   - Relationship Complexity Recognition Movement: Organized agent (organized/constrained) — advocates for systemic understanding; benefits from and constrained by the discourse simultaneously
 *   - Medicalization Apparatus: Institutional actor (institutional/arbitrage) — maintains pathologization through inertia despite reduced functional efficacy; sees own process as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (diagnostic authority, professional expertise) as inevitable features of understanding relationships
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(relationship_pathologization_discourse, 0.58).
domain_priors:suppression_score(relationship_pathologization_discourse, 0.65).
domain_priors:theater_ratio(relationship_pathologization_discourse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(relationship_pathologization_discourse, extractiveness, 0.58).
narrative_ontology:constraint_metric(relationship_pathologization_discourse, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(relationship_pathologization_discourse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(relationship_pathologization_discourse, tangled_rope).
narrative_ontology:human_readable(relationship_pathologization_discourse, "Relationship Pathologization Discourse as Extractive Framing").
narrative_ontology:topic_domain(relationship_pathologization_discourse, "psychology/interpersonal/social").

domain_priors:requires_active_enforcement(relationship_pathologization_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(relationship_pathologization_discourse, mental_health_professionals).
narrative_ontology:constraint_beneficiary(relationship_pathologization_discourse, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(relationship_pathologization_discourse, diagnostic_framework_maintainers).
narrative_ontology:constraint_victim(relationship_pathologization_discourse, relationship_complexity_recognition).
narrative_ontology:constraint_victim(relationship_pathologization_discourse, contextual_relationship_understanding).
narrative_ontology:constraint_victim(relationship_pathologization_discourse, partners_in_relationships).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATHOLOGIZED PARTNER (SNARE) — A person whose relationship behavior is reframed as a symptom or disorder bears extraction with minimal exit. The pathologization label becomes part of their identity ('I have anxious attachment,' 'I'm codependent'). Structural mobility exists (could leave the relationship) but identity fusion prevents perception of this option. The person internalizes the diagnostic frame as truth about themselves rather than as one possible interpretation of relational dynamics. Suppression is high: alternative narratives (the relationship involves legitimate conflicts, the person's responses are context-appropriate, the problem is structural misalignment not personal pathology) are displaced by the diagnostic one.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PARTNER OUTSIDE THE FRAME (TANGLED ROPE) — The other partner in the relationship experiences both coordination and extraction. The pathologization discourse enables communication about relational problems (coordination function: 'we can talk about your attachment anxiety') while simultaneously justifying asymmetric treatment ('your pathology is the problem, not our dynamic'). They benefit from the frame — it explains their partner's behavior in terms favorable to their position — while also bearing costs if they internalize responsibility for managing the 'pathologized' partner's emotional regulation. Constrained exit: leaving looks like abandoning someone with a diagnosed condition; staying requires managing the extracted emotional labor.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MENTAL HEALTH PROFESSIONAL (ROPE) — Therapists, counselors, and psychiatric practitioners see pathologization discourse as a coordination mechanism: it enables structured conversation about relational dysfunction and provides frameworks for intervention. The professional benefits from the discourse (steady demand for services, validated expertise, reimbursable diagnostic codes) while experiencing the constraint as net-positive coordination. Arbitrage exit: therapists can pivot between domains (coaching, organizational consulting, life advice) if needed. The discourse feels like pure coordination from inside this position — it solves real problems and enables helpful work.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DIAGNOSTIC FRAMEWORK MAINTAINER (ROPE) — DSM-5 publishers, classification system updaters, and academic psychology establish and maintain the categories. They experience pathologization as coordination: it enables standardized communication, research, and treatment matching. The constraint benefits them (revenue, authority, career advancement) while appearing to coordinate the field. Arbitrage exit: these actors can shift frameworks (ICD-11, dimensional models, alternative taxonomies) without major loss. The discourse from this position feels like pure infrastructure.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RELATIONSHIP COMPLEXITY RECOGNITION (TANGLED ROPE) — Organized advocates (attachment researchers, relationship educators, trauma-informed practitioners) see pathologization discourse as both necessary (it enables recognition of relational harm) and extractive (it displaces systemic understanding with individual pathology). They benefit from the discourse — it legitimates their expertise — while being constrained by its limitations. Constrained exit: challenging pathologization risks being seen as dismissing real suffering. The movement sees the constraint as having both genuine coordination function and real asymmetric extraction that needs to be reformed rather than eliminated.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE MEDICALIZATION APPARATUS (PITON) — From a civilizational view, the pathologization discourse is largely theatrical: diagnostic categories for relational dysfunction (dependent personality disorder, anxious attachment, codependency) are applied to normal relationship variation and repackaged as individual-level pathology requiring expert intervention. The apparatus persists through institutional inertia — insurance requires diagnoses, training programs teach categories, careers depend on them — despite evidence that relational problems are primarily structural (power imbalance, contextual stress, misalignment of values) rather than individual pathology. Theater ratio (0.68): much of the activity around pathologization is performative classification rather than functional treatment.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a detached analytical perspective, some pathologization of relationship dysfunction may appear inevitable: humans have cognitive biases, attachment is neurobiologically real, and some individuals do have genuine psychological conditions that affect relationships. The constraint appears as natural law — we cannot escape the reality that some relationship problems reflect individual psychopathology. However, the structural data contradicts the mountain gate: the pathologization discourse is institutionally maintained through extraction mechanisms (professional credentialing, pharmaceutical marketing, reimbursement structures), not through natural law. The 'natural' framing naturalizes contingent institutional arrangements.
constraint_indexing:constraint_classification(relationship_pathologization_discourse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(relationship_pathologization_discourse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(relationship_pathologization_discourse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(relationship_pathologization_discourse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(relationship_pathologization_discourse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(relationship_pathologization_discourse, TR),
    TR >= 0.70.

:- end_tests(relationship_pathologization_discourse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The pathologization discourse extracts from pathologized partners through identity-locking and from the field through displacement of systemic understanding. But the extraction is not total — the discourse has genuine coordination function (it does enable some people to understand and address relational problems), and many practitioners use it primarily as a communication tool rather than as a means of extraction. The value reflects genuine mixing of coordination and extraction rather than pure rent-seeking. Suppression (0.65): High. Multiple barriers prevent rejection of the pathologization frame: professional authority (therapists, psychiatrists are credentialed experts), reimbursement structures (insurance requires diagnostic codes), social epistemology (the public has been taught that relationship problems are individual pathology), and identity-lock (targets have internalized the frame). The suppression has increased over time as the pathologization apparatus has colonized more domains of relationship discourse. Theater ratio (0.68): High and rising. Much of the activity around pathologization is performance: diagnostic classification, therapeutic ritual, pharmaceutical branding, credentialing theater. The underlying coordination function (helping people understand and improve relationships) could operate with much lower theater, but the institutional apparatus maintains high performative content to justify extraction.
 *
 * PERSPECTIVAL GAP:
 *   The pathologized partner and the mental health professional see the same constraint as snare vs rope respectively. This gap reflects genuine structural difference in their positions: the professional has exit options and benefits; the target is locked into an identity frame and bears costs. The gap is not perceptual bias — it reflects real asymmetry. The relationship complexity movement occupies the tangled rope middle ground: they see both the real coordination (the discourse does help some people) and the real extraction (it displaces systemic understanding). The analytical observer's mountain perspective is a false summit: it naturalizes the professional authority and diagnostic categories as inevitable rather than recognizing them as institutional choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The pathologized partner derives high d (near 1.0: full target) from victim status plus identity_locked exit options. The professional derives low d (near 0.1: beneficiary) from beneficiary status plus arbitrage exit options. The apparatus derives very low d (near -0.05: institutional beneficiary) from beneficiary status plus arbitrage options. The relationship complexity movement derives moderate d (near 0.55) from organized power but constrained exit — they are partially victims (their understanding is suppressed) and partially beneficiaries (the discourse legitimates their expertise). These directionalities feed the chi formula: the pathologized partner experiences maximum effective extraction; the professional experiences negative extraction (the constraint subsidizes them); the organized movement experiences moderate extraction. The identity_locked exit option is critical for the victim perspective — it produces a biographical-time rope rather than mountain because the agent could in principle perceive mutability if their identity frame shifted, whereas trapped/constrained exit options produce mountain (perception of immutability regardless of framing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through decomposition. The tangled rope classification at the analytical level correctly identifies both genuine coordination (the discourse enables conversation and some beneficial interventions) and genuine extraction (it displaces systemic understanding and creates identity-locked victims). The snare perspective from the pathologized partner's position reveals the extraction mechanism most clearly. The rope perspective from the professional's position reveals the coordination mechanism most clearly. Neither perspective alone captures the structure — the mandatrophy is resolved by recognizing that both are true: the constraint genuinely coordinates while genuinely extracting. The false mountain from the civilizational analytical perspective reveals the risk of naturalizing contingent institutional arrangements. If the analyst accepts the institutional framing (relationship dysfunction is inherent psychology, thus pathologization is inevitable), they miss the extractive mechanisms that maintain the discourse. The decomposition into separate coordination and extraction stories is analytically possible but not recommended for a single story: the tangled rope already captures both functions in a single type. The theater ratio rising from 0.45 to 0.68 indicates that the coordination function is being displaced by extraction over time — the apparatus is Goodharting: diagnostic theater is replacing actual relational understanding as the primary output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_versus_relational_etiology,
    'Does relationship dysfunction primarily reflect individual pathology or relational/systemic factors?',
    'Longitudinal studies of individuals across multiple relationships; outcome data comparing individual-focused vs relationship-systems interventions; analysis of whether ''pathologized'' individuals show consistent patterns across relationships or context-dependent variation',
    'If individual: pathologization discourse is legitimate coordination (Rope from all perspectives). If relational/systemic: pathologization discourse is extractive mislabeling that displaces systemic intervention (Snare from pathologized partner perspective). If both: genuine mixed extraction requiring decomposition into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_versus_relational_etiology, empirical, 'Whether relationship dysfunction is individually or systemically rooted').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) structural (external barriers to rejecting diagnosis) or internalized (target has adopted the pathological identity frame)?',
    'Post-exit interviews with individuals who left pathologized relationships; analysis of whether identity-lock persists after leaving (suggests internalization) or dissolves when external constraints removed (suggests structural suppression); comparison of suppression persistence across different therapeutic approaches',
    'If structural: suppression reflects genuine institutional barriers (professional authority, reimbursement requirements, social belief in expertise). If internalized: target carries the suppression with them — the constraint''s actual suppression is higher than measured because the frame persists after structural removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in pathologization discourse').

omega_variable(
    therapeutic_benefit_versus_iatrogenic_harm,
    'Does pathologization discourse produce net therapeutic benefit or net iatrogenic harm?',
    'RCTs comparing outcomes: individual-pathology-focused therapy vs systems-focused therapy vs relationship education vs untreated relational dysfunction; meta-analysis of relationship satisfaction, divorce rates, and long-term adjustment; analysis of whether diagnostic labels predict treatment response better than dimensional symptom measures',
    'If benefit > harm: pathologization is a genuinely mixed Tangled Rope (real coordination with asymmetric extraction). If harm > benefit: pathologization is pure Snare repackaged as therapeutic (extraction disguised as help). If equivalent: constraint decomposes into separate coordination and extraction stories (one for legitimate mental health intervention, one for pathologization theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(therapeutic_benefit_versus_iatrogenic_harm, empirical, 'Net therapeutic benefit vs iatrogenic harm of pathologization discourse').

omega_variable(
    identity_lock_escape_trajectories,
    'What proportion of individuals with internalized pathological relationship identities successfully exit the identity lock versus remaining bound?',
    'Longitudinal qualitative analysis of identity narratives over 5-10 years post-intervention; identification of what conditions enable identity reframing; comparison of escape rates across different therapeutic modalities and contexts',
    'If escape rate > 70%: identity_locked exit option may mischaracterize the binding as primarily cognitive when structural factors enable relatively easy exit. If escape rate < 30%: identity lock is durable and represents a genuine structural trap independent of material relationship exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_escape_trajectories, empirical, 'Durability and escape probability of identity-locked pathological relationship frames').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(relationship_pathologization_discourse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rela_tr_t0, relationship_pathologization_discourse, theater_ratio, 0, 0.45).
narrative_ontology:measurement(rela_tr_t10, relationship_pathologization_discourse, theater_ratio, 10, 0.58).
narrative_ontology:measurement(rela_tr_t20, relationship_pathologization_discourse, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(rela_be_t0, relationship_pathologization_discourse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rela_be_t10, relationship_pathologization_discourse, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(rela_be_t20, relationship_pathologization_discourse, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(relationship_pathologization_discourse, attachment_coordination).
narrative_ontology:affects_constraint(relationship_pathologization_discourse, pharmaceutical_medicalization_expansion).
narrative_ontology:affects_constraint(relationship_pathologization_discourse, mental_health_professional_credentialing).
narrative_ontology:affects_constraint(relationship_pathologization_discourse, relationship_system_theory_suppression).

% DUAL FORMULATION NOTE:
% Relationship pathologization discourse decomposes structurally into two constraints: (1) attachment_coordination_genuine — legitimate mechanisms for understanding attachment patterns and relational dynamics (ε~0.15, Rope); (2) medicalization_extraction_overlay — institutional apparatus that displaces systemic understanding with individual pathology framing (ε~0.72, Snare). These are presented as a single tangled rope story because they are institutionally fused — the coordination function is the justification for the extraction mechanism. Decomposition into separate stories would clarify but would also lose the critical insight that the extraction hides behind genuine coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(relationship_pathologization_discourse, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
