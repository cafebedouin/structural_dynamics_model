% ============================================================================
% CONSTRAINT STORY: liability_attribution__developer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__developer_liability, []).

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
 *   constraint_id: liability_attribution__developer_liability
 *   human_readable: Developer Liability Attribution in Technology Governance
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   Developer liability attribution is a contested kernel governing
 *   technology governance and legal responsibility allocation. This story
 *   instantiates ONE reading: the frame in which developers, as creators of
 *   underlying technological capabilities, bear primary legal and regulatory
 *   liability for downstream harms, regardless of how those capabilities are
 *   deployed, monitored, or contextualized. This reading has structured
 *   technology regulation across US, EU, and most common-law jurisdictions.
 *   Under this frame, developers must manage or disclose risks; deployers are
 *   protected through liability caps and safe harbor clauses; regulators
 *   enforce developer accountability. The constraint exhibits hybrid
 *   coordination-extraction dynamics: it creates genuine incentives for
 *   developers to build safer systems (coordination function) while
 *   concentrating liability asymmetrically on developers who lack full
 *   control over deployment context (extraction function). The measurement
 *   trajectory shows extractiveness rising (0.32 → 0.58) and theater rising
 *   (0.38 → 0.55) as regulatory enforcement intensifies and deployers
 *   increasingly weaponize the developer-liability frame to shift
 *   responsibility for deployment-context failures. Suppression rises (0.45 →
 *   0.62) as developers face increasing compliance burden with limited exit
 *   options.
 *
 * KEY AGENTS:
 *   - Developer Community: Primary victim (powerless/trapped) — bears primary liability despite limited control over deployment; no exit option; perpetually exposed to downstream harms they cannot prevent
 *   - Deploying Organizations: Primary beneficiary (institutional/arbitrage) — externalize deployment-context risk onto developers while controlling deployment decisions; minimal legal exposure
 *   - Regulatory Authority: Secondary actor (moderate/constrained) — holds developers accountable while managing political pressure to preserve innovation; conflicting mandates create regulatory burden
 *   - Developer Coalition: Organized victim (organized/constrained) — industry associations, standards bodies, open-source foundations coordinate on liability caps, safe harbor provisions, transparency requirements
 *   - End-User Protection Interest: Secondary beneficiary (powerless/trapped) — abstract collective good of user safety; protected through developer accountability but with externalized enforcement costs
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes causation limits and the false-summit risk of naturalizing developer liability as necessary rather than contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__developer_liability, 0.58).
domain_priors:suppression_score(liability_attribution__developer_liability, 0.62).
domain_priors:theater_ratio(liability_attribution__developer_liability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__developer_liability, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(liability_attribution__developer_liability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__developer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__developer_liability, "Developer Liability Attribution in Technology Governance").
narrative_ontology:topic_domain(liability_attribution__developer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__developer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__developer_liability, 'e21ca879-77bc-483a-aba2-7ba027776be7').
narrative_ontology:cs_kernel_codification('e21ca879-77bc-483a-aba2-7ba027776be7', formalized).
narrative_ontology:cs_authority_grounding('e21ca879-77bc-483a-aba2-7ba027776be7', lineage).
narrative_ontology:cs_interpretation_layer_present('e21ca879-77bc-483a-aba2-7ba027776be7').
narrative_ontology:cs_reading_relation('e21ca879-77bc-483a-aba2-7ba027776be7', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_reading_relation('e21ca879-77bc-483a-aba2-7ba027776be7', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('e21ca879-77bc-483a-aba2-7ba027776be7', foundational, capability_creator_bears_responsibility).
narrative_ontology:cs_axiom_status(capability_creator_bears_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('e21ca879-77bc-483a-aba2-7ba027776be7', capability_creator_bears_responsibility, deontological).
narrative_ontology:cs_axiom('e21ca879-77bc-483a-aba2-7ba027776be7', secondary, deployer_exoneration_via_liability_concentration).
narrative_ontology:cs_axiom_status(deployer_exoneration_via_liability_concentration, holdable).
narrative_ontology:cs_axiom_grounding('e21ca879-77bc-483a-aba2-7ba027776be7', deployer_exoneration_via_liability_concentration, conventional).
narrative_ontology:cs_reference_frame('e21ca879-77bc-483a-aba2-7ba027776be7', developer_accountability_regime).
narrative_ontology:cs_drift_state('e21ca879-77bc-483a-aba2-7ba027776be7', contemporary_regulatory_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e21ca879-77bc-483a-aba2-7ba027776be7', '').
narrative_ontology:cs_kernel_id(liability_attribution__developer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, deployer_organizations).
narrative_ontology:constraint_beneficiary(liability_attribution__developer_liability, end_user_market_protection).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, developer_community).
narrative_ontology:constraint_victim(liability_attribution__developer_liability, innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPER TEAM (SNARE) — Developers bear primary liability as creators of underlying capability despite having limited control over deployment context, user behavior, and downstream harms. No meaningful exit: once capability is released, developers remain perpetually liable. Suppression is total: indemnity clauses shift risk nominally but liability exposure remains. Maximum experienced extraction — developer bears costs of harms they did not cause and cannot prevent.
constraint_indexing:constraint_classification(liability_attribution__developer_liability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPLOYING ORGANIZATION (ROPE) — Deployers benefit from externalizing liability: they select deployment context, decide enforcement policies, and choose whether to monitor use, but bear minimal legal consequences when harms occur. The constraint coordinates genuine problem-solving (incentivizing developers to build safer systems) with near-zero cost to deployers. Coordination function is real; extraction is the asymmetry in risk allocation.
constraint_indexing:constraint_classification(liability_attribution__developer_liability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (TANGLED ROPE) — Regulators face contradictory mandates: hold developers accountable (protecting public) while preserving innovation incentives (economic growth). The constraint provides coordination (establishes responsibility chain) and extracts from regulators themselves through institutional burden of managing the developer-deployer conflict. Constrained exit: cannot simply abandon developer liability framework without political/technical backlash, but can modulate enforcement.
constraint_indexing:constraint_classification(liability_attribution__developer_liability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEVELOPER COALITION (TANGLED ROPE) — Organized developers (standards bodies, open-source foundations, industry associations) coordinate on shared defense: liability caps, safe harbor clauses, transparency requirements that shift burden toward deployer verification. Coalition has agency and negotiating power, but remains constrained by regulatory asymmetry. Generates both coordination (industry standards for safe development) and extraction (standards become developer protection, not user protection).
constraint_indexing:constraint_classification(liability_attribution__developer_liability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY LIABILITY REGIME (PITON) — Product liability doctrine (designed for manufactures of physical goods) is theatrically applied to software/algorithm developers. The machinery persists despite fundamental category mismatch: software has no production variance, no physical breakdown, no natural wear — the doctrinal framework is performative. Theater ratio high (0.55): courts apply causation standards from product liability to algorithmic harm, generating spectacle of traditional jurisprudence applied to novel harms. Regime persists through institutional inertia, not functional adequacy.
constraint_indexing:constraint_classification(liability_attribution__developer_liability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: EMERGING LIABILITY REALLOCATION (SCAFFOLD) — New frameworks (GDPR liability rules, algorithmic impact assessments, deployer-side obligations) are building alternative accountability pathways that distribute liability across the developer-deployer-regulator triad rather than concentrating on developers. These are temporary coordination scaffolds with sunset logic: as deployer accountability norms mature and impact assessment becomes standard practice, the pure developer-liability regime becomes architecturally obsolete. Estimated sunset: 15-25 years for norms to settle across major jurisdictions.
constraint_indexing:constraint_classification(liability_attribution__developer_liability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / CAUSATION LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, developers cannot be held fully liable for harms caused by deployer choices, user behavior, and contextual factors outside developer control. Causal responsibility is technically bounded: developers control the capability design, not its use. This perspective classifies the developer-liability framework as a false summit — it naturalizes a policy choice (concentrated liability) as a causal necessity. The structural data contradicts this: liability allocation is a contingent regulatory design, not a law of nature or logic.
constraint_indexing:constraint_classification(liability_attribution__developer_liability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__developer_liability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liability_attribution__developer_liability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liability_attribution__developer_liability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__developer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liability_attribution__developer_liability, TR),
    TR >= 0.70.

:- end_tests(liability_attribution__developer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Developers bear significant liability burden through indemnification pressure, regulatory compliance costs, and perpetual exposure to downstream harms. The extraction is not maximal (0.70+) because developers retain some negotiating power through industry coalition activity and regulatory capture dynamics. The trajectory rising from 0.32 → 0.58 reflects intensifying enforcement over the decade: early-stage tech regulation (2015-2020) relied lightly on developer liability; mature regulation (2020-2026) treats developer accountability as standard. Suppression (0.62): High. Barriers to developers exiting the liability framework are substantial: once a capability is released, developers remain perpetually liable in many jurisdictions; indemnity clauses shift nominal responsibility but not actual risk; international regulatory fragmentation prevents forum-shopping. Suppression rises (0.45 → 0.62) as enforcement mechanisms mature and cross-border liability exposure increases. Theater ratio (0.55): Moderate-high. Traditional product liability doctrine (designed for physical manufactures) is theatrically applied to software and algorithms: courts invoke causation standards from defective-product cases to algorithmic-harm cases, generating jurisprudential spectacle. The doctrinal machinery persists despite fundamental category mismatch between product variance (manufacturing defects, wear, failure modes) and software behavior (no variance, no natural wear, emergent properties from deployment context). Theater has risen (0.38 → 0.55) as novel harm categories (algorithmic bias, emergent capabilities, adversarial exploitation) expose the category-mismatch problem. The legacy regime (piton perspective) maintains theater through institutional inertia — courts apply familiar frameworks to novel problems, generating appearance of coherence while lacking actual explanatory power.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. Developers experience maximum extraction (snare): they bear perpetual liability for factors outside their control. Deployers experience near-zero extraction (rope): they coordinate safer systems while externalizing deployment-context risk. Regulators experience conflicting mandates (tangled_rope): held responsible for public safety while unable to shift liability to deployers without political backlash. The developer coalition (organized tangled_rope) negotiates from asymmetric power: they can influence standards and safe harbor clauses but cannot escape the underlying frame in which they are accountable. The legacy liability regime (piton) sees the doctrinal machinery as performative: applying product-liability standards to software creates spectacle of jurisdiction without functional coverage. The scaffold perspective reveals emerging liability reallocation: GDPR, algorithmic impact assessments, and deployer-side obligations are building alternative accountability pathways that distribute liability across the developer-deployer-regulator triad. The analytical observer (mountain) risks naturalizing developer liability as a causal necessity (developers created the capability, so they are responsible for its effects) when it is actually a contingent regulatory choice that reflects deployer power rather than causal structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from structural position in the liability attribution frame. Developers are victims (high d): they bear liability despite lack of deployment control and contextual awareness. Deployers are beneficiaries (low d): they externalize risk while retaining deployment decision authority. The regulatory authority is caught between beneficiary mandate (protect users) and victim mandate (regulate deployers fairly), producing moderate d. The developer coalition is organized but constrained by the underlying frame, producing constrained exit options and moderate-high d. Suppression measurement is not scaled by these context dimensions — it reflects raw structural barriers to exiting the developer-liability regime (international fragmentation, perpetual exposure, indemnity clause opacity). Extractiveness is scaled by f(d) and spatial scope: developer victims in global scope experience amplified extraction through international liability exposure; deployers with arbitrage options experience dampened effective extraction through jurisdictional forum-shopping.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (how can it be both coordination and extraction?) by showing that the developer-liability frame IS a hybrid: it coordinates safer development (genuine coordination function) through developer incentives while simultaneously extracting from developers by concentrating risk asymmetrically (extraction function). The mandatrophy is resolved at the level of the beneficiary-victim structure: developers experience extraction because deployers experience coordination benefit without bearing deployment-context costs. The false-summit risk is acute here: the analytical observer can misclassify developer liability as a natural law ('developers created the capability, so they are responsible') when it is actually a contingent regulatory choice reflecting deployer power and political economy rather than causal necessity or logical requirement. The piton perspective (legacy liability regime) shows that the doctrinal machinery persists through institutional inertia despite category mismatch. The scaffold perspective (emerging liability reallocation) shows the frame is not immutable — alternative distributions of accountability are being constructed in real time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deployer_negligence_causation,
    'How much causal responsibility should developers bear for harms that deployers could have prevented through adequate monitoring, access controls, or deployment constraints?',
    'Comparative fault analysis: identify specific cases where deployer negligence was the primary cause; determine what precautions deployers could have taken; establish baseline deployer duties in regulatory frameworks',
    'If deployer negligence frequently causes harm the developer could not have predicted: developer liability becomes unjust and innovation suffers. If deployer negligence is rare and preventable only by developers: developer liability is justified. Impacts snare vs. tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deployer_negligence_causation, empirical, 'Causal responsibility allocation between developers and deployers').

omega_variable(
    opacity_disclosure_asymmetry,
    'Should developers be liable for harms from capabilities they disclosed to deployers but deployers failed to understand or implement safely?',
    'Analysis of disclosure adequacy: what information developers provided; what deployers knew about risks; what deployers actually did with systems; tracking harm outcomes correlated with disclosure quality vs. deployer choice',
    'If disclosure is adequate but deployers ignore it: liability should shift toward deployers. If developers obscured capability scope or failure modes: developer liability is justified. Affects suppression measurement and victim set composition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_disclosure_asymmetry, empirical, 'Opacity and disclosure adequacy as drivers of liability attribution').

omega_variable(
    capability_vs_context_boundary,
    'Is liability grounded in the developer''s capability design choices or in the deployer''s context-specific decisions about how to deploy the capability?',
    'Causal decomposition of specific harms: isolate capability-side factors (design flaws, incomplete specifications, unintended behaviors) from context-side factors (deployment constraints, user population, integration choices). Identify cases where the same capability causes harm in one deployment context and no harm in another.',
    'If harms are primarily context-driven: deployer (who controls context) should bear primary liability. If harms are primarily capability-driven: developer should bear primary liability. This is the foundational omega for the reading itself — it determines whether the developer-liability frame is coherent or whether liability must be redistributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_vs_context_boundary, conceptual, 'Boundary between capability design responsibility and deployment context responsibility').

omega_variable(
    kernel_reading_contest,
    'Which sibling reading (deployer_liability, shared_liability) better reflects the actual harm causation structure in contemporary technology deployment?',
    'Historical case analysis of major technology harms: trace causal chains from capability design through deployment decisions to final harm outcome. Identify inflection points where deployer choice, not developer design, was the critical factor. Map liability doctrine against actual causal responsibility.',
    'If developers are consistently blamed for deployer choices: the developer_liability reading naturalizes injustice and should be superseded. If developers'' design choices are the primary causal factor: developer_liability reading is structurally sound. If causation is genuinely distributed: the shared_liability reading is more coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Relative coherence of developer_liability vs. sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__developer_liability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_dev_tr_t0, liability_attribution__developer_liability, theater_ratio, 0, 0.38).
narrative_ontology:measurement(liab_dev_tr_t5, liability_attribution__developer_liability, theater_ratio, 5, 0.48).
narrative_ontology:measurement(liab_dev_tr_t10, liability_attribution__developer_liability, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(liab_dev_be_t0, liability_attribution__developer_liability, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(liab_dev_be_t5, liability_attribution__developer_liability, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(liab_dev_be_t10, liability_attribution__developer_liability, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(liab_dev_su_t0, liability_attribution__developer_liability, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(liab_dev_su_t5, liability_attribution__developer_liability, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(liab_dev_su_t10, liability_attribution__developer_liability, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__developer_liability, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, liability_attribution__shared_liability).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, algorithmic_transparency_requirement).
narrative_ontology:affects_constraint(liability_attribution__developer_liability, safe_harbor_regulatory_capture).

% DUAL FORMULATION NOTE:
% The liability_attribution kernel encompasses three structurally distinct constraints with different ε values and beneficiary/victim structures. developer_liability (this story, ε=0.58) concentrates liability on developers. deployer_liability and shared_liability are sibling readings with different ε values and causal models. All three are linked as members of the liability_attribution constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__developer_liability, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
