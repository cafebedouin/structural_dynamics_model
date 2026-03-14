% ============================================================================
% CONSTRAINT STORY: psychiatric_diagnostic_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_psychiatric_diagnostic_authority, []).

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
 *   constraint_id: psychiatric_diagnostic_authority
 *   human_readable: Psychiatric Diagnostic Authority and the DSM Consensus Framework
 *   domain: mental_health/institutional_authority
 *
 * SUMMARY:
 *   The psychiatric diagnostic authority — operationalized through the DSM
 *   (Diagnostic and Statistical Manual) and its international equivalent the
 *   ICD — represents a system of cognitive classification that distributes
 *   explanatory power asymmetrically across institutional actors. The
 *   constraint exhibits the full range of DR types from different
 *   perspectives, making it a critical exemplar for how institutional
 *   authority becomes naturalized. The same structural arrangement — the
 *   vesting of naming and diagnostic authority in a professional
 *   establishment — appears as coordination mechanism (rope from psychiatric
 *   establishment), mixed coordination-extraction (tangled rope from industry
 *   and families), identity-constituting extraction (snare from diagnosed
 *   subjects), degraded ritual maintained by administrative inertia (piton
 *   from billing systems), temporary institutional arrangement being
 *   superseded by alternative epistemologies (scaffold from peer support
 *   movements), and immutable necessity of medical classification (mountain
 *   from analytical view). The constraint's theater_ratio (0.68) reflects
 *   that psychiatric diagnostics involve substantial performative content:
 *   extensive diagnostic interviews, symptom checklists, and observational
 *   protocols exist partially to satisfy documentation requirements rather
 *   than to improve treatment. The extractiveness trajectory (0.35 → 0.62
 *   over 60 years) maps the expansion of diagnostic categories from DSM-I
 *   (106 diagnoses) through DSM-5 (300+ diagnoses), driven by pharmaceutical
 *   marketing, insurance billing incentives, and professional credentialing
 *   rather than by new empirical discoveries of discrete disease entities.
 *   The constraint's defining feature is that it grounds institutional
 *   authority in the claim that subjects cannot accurately perceive their own
 *   mental states — only the professional establishment can name what the
 *   subject experiences.
 *
 * KEY AGENTS:
 *   - Diagnosed Subjects: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with diagnostic categories; experience maximum extraction of autonomous self-interpretation
 *   - Psychiatric Establishment: Primary beneficiary (institutional/arbitrage) — monopolizes explanatory authority; experiences constraint as pure coordination for professional communication and treatment standardization
 *   - Pharmaceutical Industry: Secondary beneficiary (powerful/arbitrage) — profits from diagnostic expansion; coordinates drug development with disease-creation marketing; has high power and exit options but structural incentives to deepen extraction
 *   - Insurance and Billing Systems: Beneficiary (institutional/constrained) — requires categorical buckets for administrative processing; maintains categories through inertia despite recognition of their pragmatic-fictional status
 *   - Family and Community Members: Secondary victims (moderate/constrained) — benefit from diagnostic language enabling coordination of support; constrained by framework's authority and informal pressure to align with psychiatric interpretation
 *   - Peer Support and Mad Pride Movements: Organized challengers (organized/constrained) — building alternative epistemic frameworks; organizing around lived experience expertise; developing exit pathways through cultural change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to medicine itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(psychiatric_diagnostic_authority, 0.58).
domain_priors:suppression_score(psychiatric_diagnostic_authority, 0.65).
domain_priors:theater_ratio(psychiatric_diagnostic_authority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(psychiatric_diagnostic_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(psychiatric_diagnostic_authority, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(psychiatric_diagnostic_authority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(psychiatric_diagnostic_authority, tangled_rope).
narrative_ontology:human_readable(psychiatric_diagnostic_authority, "Psychiatric Diagnostic Authority and the DSM Consensus Framework").
narrative_ontology:topic_domain(psychiatric_diagnostic_authority, "mental_health/institutional_authority").

domain_priors:requires_active_enforcement(psychiatric_diagnostic_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(psychiatric_diagnostic_authority, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(psychiatric_diagnostic_authority, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(psychiatric_diagnostic_authority, insurance_billing_systems).
narrative_ontology:constraint_victim(psychiatric_diagnostic_authority, diagnostic_subjects).
narrative_ontology:constraint_victim(psychiatric_diagnostic_authority, excluded_lived_experience_epistemologies).
narrative_ontology:constraint_victim(psychiatric_diagnostic_authority, structural_psychiatry_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIAGNOSED SUBJECT (SNARE) — Structurally mobile (can refuse treatment, change providers) but identity-fused with the diagnostic category. The subject's self-concept is constituted through the psychiatric label — 'I am bipolar,' 'I am schizophrenic' — making exit unthinkable from within the identity frame. The diagnostic framework provides narrative coherence but extracts autonomy through medicalization of distress. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: FAMILY AND COMMUNITY MEMBERS (TANGLED ROPE) — Benefit from shared diagnostic language that enables coordination of support; constrained by the framework's authority and by informal pressure to align with psychiatric interpretation of distress. The constraint coordinates caregiving while asymmetrically extracting alternative explanatory frameworks (spiritual crisis, social suffering, grief, existential despair reframed as pathology).
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PSYCHIATRIC ESTABLISHMENT (ROPE) — Experiences the DSM framework as pure coordination mechanism. Shared diagnostic criteria enable treatment standardization, research comparability, and professional communication. The establishment has arbitrage options (can modify criteria, can resist external pressure) and experiences the constraint as solving a genuine collective action problem without significant extractive overhead.
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (TANGLED ROPE) — Benefits from diagnostic expansion (wider disease categories create larger markets). The framework coordinates drug development and FDA approval processes while asymmetrically extracting from subjects through disease creep, medicalization of normal variation, and marketing-driven diagnostic inflation. High power actor with genuine exit options but structured incentives to deepen the constraint.
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER SUPPORT AND MAD PRIDE MOVEMENTS (SCAFFOLD) — Organized actors (peer support networks, neurodiversity advocates, mad pride coalitions) see diagnostic authority as a temporary institutional arrangement with a visible sunset. These movements are building alternative epistemic frameworks (social models of disability, neurodiversity, lived experience expertise) that bypass traditional psychiatric authority. Constrained by medical system's power but with agency and a exit path through cultural change.
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INSURANCE AND BILLING SYSTEMS (PITON) — The DSM provides categorical buckets required for billing and coverage determination. The system maintains these categories through institutional inertia despite widespread recognition that diagnostic categories are pragmatically useful fictions rather than natural kinds. Theater ratio is high: extensive diagnostic rituals exist primarily to satisfy billing and coverage documentation requirements, not to improve treatment outcomes. The constraint persists because alternatives (dimensional systems, contextual formulations) break the administrative machinery.
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NOSOLOGICAL NECESSITY VIEW (MOUNTAIN) — From the civilizational view, some form of diagnostic categorization is inherent to any medical system: any attempt to treat suffering requires naming patterns. This perspective sees psychiatric classification as a natural law of clinical medicine itself. However, the structural data contradicts this — the DSM constraint is contingent on institutional choices (insurance billing requirements, pharmaceutical incentive structures, professional credentialing) that are not inherent to medicine. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(psychiatric_diagnostic_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(psychiatric_diagnostic_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(psychiatric_diagnostic_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(psychiatric_diagnostic_authority, TR),
    TR >= 0.70.

:- end_tests(psychiatric_diagnostic_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The DSM framework extracts explanatory authority from subjects through medicalization of normal variation, diagnostic expansion, and the systematic privileging of professional interpretation over lived experience. The extraction is not maximal (subjects retain access to non-psychiatric frameworks, can refuse medication, can seek alternative support) but is structurally embedded in institutional power. The trajectory from 0.35 to 0.58 reflects diagnostic category expansion independent of empirical evidence — a hallmark of extraction dynamics. Suppression (0.65): High. Multiple barriers prevent alternatives: insurance systems are hardwired to DSM categories; professional licensing requires psychiatric training; marketing from the pharmaceutical industry saturates public narratives; media representation naturalizes psychiatric interpretation; institutional momentum keeps the framework in place despite widespread recognition of its limitations. These are not absolute barriers (alternative frameworks persist) but are substantial. Theater ratio (0.68): High. Psychiatric diagnosis involves extensive performative ritual: DSM-V structured interviews, symptom checklists, observational protocols, and documentation procedures exist partially to satisfy administrative requirements rather than to improve clinical outcomes. The theater has increased as diagnostic categories have proliferated — more diagnoses require more differential diagnostic work. Claimed type (Tangled Rope): The constraint exhibits genuine coordination function (shared diagnostic language enables professional communication, research standardization, treatment planning) alongside asymmetric extraction (monopoly on explanatory authority, medicalization of distress, diagnostic category expansion driven by commercial incentives). Both are structurally real.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the establishment (Rope) and the subject (Snare). From the establishment perspective, the DSM is a coordination achievement — shared diagnostic language solved a real problem of incommensurability in 1950s psychiatry. From the subject perspective, the framework extracts autonomy through medicalization, identity-fusion, and systematic privileging of professional interpretation. The gap reveals that 'coordination' from above becomes 'extraction' from below when authority is asymmetric. The piton classification of insurance systems reveals that bureaucratic machinery maintains categories not because they work but because alternatives would require administrative restructuring. The scaffold classification of peer support movements reveals that the constraint's extraction power depends on monopoly of explanatory authority — distributed expertise (lived experience) bypasses the entire system. The mountain classification at the analytical level is a false summit — the framework naturalizes institutional choices (insurance billing structure, pharmaceutical business models, professional credentialing) as inherent to medicine.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity_locked exit for diagnosed subjects is the critical diagnostic marker. The constraint does not trap subjects through material barriers (they retain legal freedom to refuse treatment, change providers, seek alternatives). Instead, it locks them through identity fusion — the subject's self-concept is constituted through the diagnostic category. Breaking the constraint would require becoming a different person, not just paying a material cost. This is distinct from constrained exit (high-cost external barriers) and trapped exit (insurmountable external barriers). The identity_lock reveals that the constraint's extraction mechanism operates through internalization of the diagnostic frame — the subject enforces the constraint on themselves. This appears in the measurements as theater_ratio rising faster than extractiveness: the system's ability to extract grows not from increased coercion (suppression stays stable) but from increased internalization (subjects enforce the diagnostic frame on themselves). The pharmaceutical industry's directionality is noteworthy: they are powerful with arbitrage options, but the constraint's structure provides incentives to deepen extraction. This is where institutional incentives align with extraction mechanisms — diagnostic expansion increases market size, so profit-maximization drives disease-creation.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The psychiatric diagnostic authority demonstrates why indexical classification matters. The mandatrophy questions whether the system is coordination-with-extraction (Tangled Rope) or pure extraction dressed as coordination (Snare at a hidden analytical level). The resolution comes from examining whose interests are served and whether alternatives exist. The psychiatric establishment truthfully experiences coordination — they solved a real incommensurability problem. The pharmaceutical industry truthfully pursues both coordination and extraction — they coordinate drug development while extracting through disease-creation marketing. The diagnosed subject experiences pure extraction because their autonomy is systematically subordinated to professional interpretation, and alternatives (peer support, non-psychiatric frameworks) exist but are suppressed by institutional power. The mandatrophy resolves by showing that 'coordination' is perspective-dependent. The establishment coordinates itself; the subject experiences extraction. The Tangled Rope classification correctly captures both: the constraint has a genuine coordination function (professional communication, research standardization) AND asymmetric extraction (medicalization of distress, loss of explanatory authority, identity-fusion). No single type is wrong — they are all partial truths from different structural positions. The engine's job is to recognize that the perspectival spectrum itself is the diagnostic data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_category_realism,
    'Do DSM diagnostic categories map to natural kinds (real disease entities) or are they pragmatic conventions created to serve administrative and commercial needs?',
    'Longitudinal neurobiology research testing whether diagnostic categories show distinct biological markers; examination of how diagnosis has changed with DSM revisions independent of empirical findings; analysis of pharmaceutical industry influence on diagnostic threshold expansion',
    'If natural kinds: the diagnostic authority is epistemically justified (mountain view correct). If pragmatic conventions: the constraint is institutional extraction with false summit naturalization. Current evidence suggests mixed — some categories show biological coherence (schizophrenia, bipolar disorder) while others reflect diagnostic inflation (ADHD, generalized anxiety disorder).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_category_realism, empirical, 'Whether DSM categories represent natural kinds or pragmatic administrative categories').

omega_variable(
    identity_lock_mechanism,
    'Is the identity fusion of diagnosed subjects (''I am bipolar'') a direct effect of the diagnostic framework or a projection of the subjects'' lived experience that would persist with or without the label?',
    'Comparison of identity narratives across diagnostic traditions (psychiatric vs non-Western healing frameworks, peer support communities); analysis of subjects who have rejected or reclaimed diagnostic labels; longitudinal studies of whether identity fusion changes when diagnostic certainty decreases',
    'If the framework creates the identity lock: the constraint is more extractive than the moderate identity_locked classification suggests. If the framework merely names pre-existing identity fusion: the framework may be useful despite extraction. The distinction determines whether identity_locked exit is genuinely constrained or genuinely trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity fusion is created by diagnostic framework or pre-exists it').

omega_variable(
    alternative_epistemology_sufficiency,
    'Can lived experience expertise, peer support models, and non-psychiatric frameworks (spiritual, social, contextual) provide equivalent guidance for helping distressed people without the extractive machinery of the DSM?',
    'Outcome studies comparing peer support networks with psychiatric treatment; qualitative analysis of how communities without psychiatric infrastructure address mental health crises; longitudinal tracking of subjects who have exited psychiatric care and sustained recovery through alternative frameworks',
    'If alternatives are equivalent: scaffold perspective is correct — psychiatric authority has a clear sunset. If alternatives fail or are incomplete: diagnostic authority may be necessary despite extraction. Current evidence suggests partial equivalence — peer support and alternative frameworks excel in certain domains (recovery narratives, identity reclamation, community belonging) while psychiatric care has advantages in acute crisis intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemology_sufficiency, empirical, 'Whether non-psychiatric approaches can provide equivalent help without DSM authority').

omega_variable(
    pharma_incentive_contamination,
    'To what extent have pharmaceutical marketing and profit incentives directly shaped DSM diagnostic criteria and thresholds independent of clinical evidence?',
    'Archival analysis of DSM revision process — documented influence of pharma-sponsored advisors; comparison of diagnostic threshold changes to patent expirations and market saturation; analysis of how symptoms have been reclassified as discrete disorders during periods of drug development',
    'High contamination: diagnostic authority is structurally compromised by extraction mechanisms; constraint should be classified as higher suppression and theater ratio. Low contamination: diagnostic authority may be epistemically sound despite institutional flaws. Evidence strongly supports high contamination (e.g., ADHD expansion, bipolar disorder expansion in adolescence, gender dysphoria reclassification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharma_incentive_contamination, empirical, 'Extent of pharmaceutical industry influence on DSM revision decisions').

omega_variable(
    billing_necessity_assumption,
    'Is the DSM categorical framework strictly necessary for insurance billing and coverage determination, or is it maintained for administrative convenience despite availability of superior alternatives?',
    'Analysis of healthcare systems using dimensional or transdiagnostic coding (ICD-11 implementation); comparison of billing complexity and coverage outcomes across coding systems; examination of barriers to adopting alternative coding systems in the US insurance context',
    'If necessary: piton classification is correct — categories persist because administrative machinery requires them. If convenience: the constraint is maintained for institutional inertia and could be replaced with lower-extraction alternatives. This is likely — dimensional systems and problem-focused coding would reduce diagnostic theater but require administrative restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(billing_necessity_assumption, empirical, 'Whether DSM categorical framework is administratively necessary or merely convenient').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(psychiatric_diagnostic_authority, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psych_diag_tr_t0, psychiatric_diagnostic_authority, theater_ratio, 0, 0.48).
narrative_ontology:measurement(psych_diag_tr_t20, psychiatric_diagnostic_authority, theater_ratio, 20, 0.58).
narrative_ontology:measurement(psych_diag_tr_t40, psychiatric_diagnostic_authority, theater_ratio, 40, 0.68).
narrative_ontology:measurement(psych_diag_tr_t60, psychiatric_diagnostic_authority, theater_ratio, 60, 0.7).

% Extraction over time
narrative_ontology:measurement(psych_diag_be_t0, psychiatric_diagnostic_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psych_diag_be_t20, psychiatric_diagnostic_authority, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(psych_diag_be_t40, psychiatric_diagnostic_authority, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(psych_diag_be_t60, psychiatric_diagnostic_authority, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(psychiatric_diagnostic_authority, identity_coordination).
narrative_ontology:affects_constraint(psychiatric_diagnostic_authority, pharmaceutical_disease_creation).
narrative_ontology:affects_constraint(psychiatric_diagnostic_authority, psychiatric_medication_coercion).
narrative_ontology:affects_constraint(psychiatric_diagnostic_authority, mental_illness_stigma).
narrative_ontology:affects_constraint(psychiatric_diagnostic_authority, peer_support_legitimacy).

% DUAL FORMULATION NOTE:
% The psychiatric diagnostic authority decomposes into at least three structurally distinct constraints: (1) nosological classification (ε≈0.25, Mountain/Rope — genuine coordination problem), (2) pharmaceutical incentive alignment with disease expansion (ε≈0.62, Snare/Tangled Rope — extraction mechanism), (3) professional credentialing and authority monopoly (ε≈0.48, Tangled Rope — mixed coordination-extraction). This story addresses the unified authority framework; decomposed stories would separately address classification rigor vs. expansion dynamics vs. professional gatekeeping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(psychiatric_diagnostic_authority, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
