% ============================================================================
% CONSTRAINT STORY: medical_labor_market_monopsony
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_labor_market_monopsony, []).

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
 *   constraint_id: medical_labor_market_monopsony
 *   human_readable: Medical Labor Market Monopsony
 *   domain: healthcare_economics/labor
 *
 * SUMMARY:
 *   The medical labor market in the United States exhibits monopsony
 *   characteristics — concentrated employer power over physician wages and
 *   employment terms — particularly acute in rural regions and specialty
 *   markets with single dominant employers. This constraint combines genuine
 *   coordination requirements (integrated healthcare systems require
 *   physician staffing, capital infrastructure, administrative overhead) with
 *   extractive mechanisms (wage suppression, non-competes, geographic
 *   lock-in). The monopsony is not total (many physicians have multi-employer
 *   options, and some can practice independently), but it is structural
 *   enough to significantly suppress wages relative to medical training costs
 *   and productivity. The constraint affects three distinct populations: (1)
 *   physicians in single-buyer regions who face true entrapment; (2)
 *   specialists with some geographic mobility facing constrained exit
 *   options; and (3) patients in underserved regions experiencing supply
 *   constraints driven by workforce discouragement. The extractiveness has
 *   increased over the 20-year interval (0.38 → 0.58) as hospital
 *   consolidation has accelerated. Theater ratio remains moderate (0.48)
 *   because hospital justifications for consolidation have legitimate
 *   coordination components; the performance is not purely theatrical, but
 *   increasingly functional consolidation is being justified through and
 *   channeled via administratively intensive processes rather than market
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Physicians in monopsony regions: Primary victims (powerless/trapped) — face geographic and credential lock-in; cannot exit without severe relocation costs
 *   - Specialist physicians with multi-market access: Secondary victims (moderate/constrained) — face high relocation costs and non-compete barriers but retain some optionality
 *   - Hospital systems: Primary beneficiaries (institutional/arbitrage) — consolidate to achieve coordination gains but capture monopsony rents through wage suppression
 *   - Insurance companies: Secondary beneficiaries (powerful/arbitrage) — benefit from wage suppression through lower provider reimbursement but face extraction from consolidated hospitals
 *   - Patients in underserved regions: Tertiary victims (powerless/trapped) — cannot exit healthcare market; experience access constraints driven by physician supply discouragement
 *   - Medical associations and organized labor: Organized agents (organized/constrained) — attempting to reduce monopsony through policy reform, union organizing, and regulatory change
 *   - State medical boards and licensing system: Institutional actors (institutional/arbitrage) — maintain credential non-portability justified as quality assurance but functionally supporting monopsony
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangements (state licensure, consolidation incentives) as structural inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_labor_market_monopsony, 0.58).
domain_priors:suppression_score(medical_labor_market_monopsony, 0.65).
domain_priors:theater_ratio(medical_labor_market_monopsony, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_labor_market_monopsony, extractiveness, 0.58).
narrative_ontology:constraint_metric(medical_labor_market_monopsony, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(medical_labor_market_monopsony, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_labor_market_monopsony, tangled_rope).
narrative_ontology:human_readable(medical_labor_market_monopsony, "Medical Labor Market Monopsony").
narrative_ontology:topic_domain(medical_labor_market_monopsony, "healthcare_economics/labor").

domain_priors:requires_active_enforcement(medical_labor_market_monopsony).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_labor_market_monopsony, hospital_systems).
narrative_ontology:constraint_beneficiary(medical_labor_market_monopsony, insurance_companies).
narrative_ontology:constraint_victim(medical_labor_market_monopsony, physician_workforce).
narrative_ontology:constraint_victim(medical_labor_market_monopsony, patient_access_to_care).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PHYSICIAN IN MONOPSONY (SNARE) — A physician in a region with one dominant employer (hospital system) faces genuine entrapment. Exit requires geographic relocation, retraining, or accepting severe income loss. Suppression mechanisms are structural: capital requirements for independent practice, licensing locality specificity, student debt burden, and family relocation costs. The physician perceives maximal extraction — wage suppression relative to productivity, mandatory call requirements, and loss of clinical autonomy packaged as 'integration' and 'patient coordination.'
constraint_indexing:constraint_classification(medical_labor_market_monopsony, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SPECIALIST WITH GEOGRAPHIC OPTIONS (TANGLED ROPE) — A cardiologist with national reputation faces high but surmountable relocation costs. Multiple hospital systems compete for specialists, but the labor market still exhibits monopsony characteristics at the specialty/region intersection. Coordination function exists: physicians benefit from employment benefits (malpractice insurance, EMR infrastructure), but extraction persists through enforced productivity targets, RVU-based compensation pressure, and mobility constraints (non-competes, credential transfers). Experiences both genuine coordination and asymmetric extraction.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HOSPITAL SYSTEM (ROPE) — Experiences the constraint as coordination: employing physicians enables service integration, centralized administrative overhead, and coordinated capital investment. The monopsony position is a side effect of legitimate coordination needs (scheduling, billing, compliance). The hospital system has high exit optionality: can adjust staffing, recruit from out-of-region, or shift to mid-level provider models. Experiences the constraint as coordination with minimal extraction burden.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PATIENT ACCESS (SNARE) — Physician labor monopsony depresses wages relative to medical school training costs, reducing workforce supply. This constrains patient access, particularly in rural and underserved regions where monopsony is most severe. The patient population cannot organize, has no advocate, and bears the cost through longer wait times, geographic gaps in specialty access, and physician burnout-driven exits from practice. Pure extraction with no capacity to exit or negotiate.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: INSURANCE COMPANY (TANGLED ROPE) — Insurance companies benefit from physician wage suppression (lower labor costs reduce provider reimbursement claims). They also benefit from hospital consolidation (fewer, larger negotiating partners reduce administrative overhead). Insurance companies maintain coordination with hospitals through selective contracting and quality metrics. But they face extraction pressure from hospital systems as consolidation increases provider market power in hospital-insurer negotiations. Bidirectional: they extract from physicians but are extracted from by consolidated hospital systems.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ORGANIZED MEDICINE & REFORM (SCAFFOLD) — Medical associations, labor organizing (physicians' unions), and regulatory interventions (anti-trust enforcement, non-compete restrictions) represent organized agents attempting to reduce monopsony extraction. These movements have sunset logic: enforcement of non-competes, telemedicine mobility, and workforce diversification (nurse practitioners, physician assistants) are gradually lowering geographic and credential barriers. Organized medicine experiences the constraint as a temporary coordination failure with structural pathways to exit through policy reform and market fragmentation.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: MEDICAL GUILD (PITON) — Medical boards, state licensing, and the professional credentialing system serve genuine coordination functions (quality assurance, public safety) but have become largely performative. Licensing requirements are set at state level and protect against credential portability across state lines, formally justified as quality assurance but functionally reducing labor supply elasticity and supporting monopsony. The guild system persists through institutional inertia despite being undermined by telemedicine and mid-level provider expansion. Theater ratio high (0.65): substantial performative licensing requirements relative to safety benefit.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational view, physician labor monopsony might appear immutable: hospitals require integrated infrastructure, training is geographically concentrated, and capital requirements for practice create natural barriers. This perspective risks naturalizing contingent institutional arrangements (state licensure, credential non-portability, hospital consolidation incentives) as structural inevitabilities. The engine's false summit detector will identify this as misattribution of institutional design choices to natural law.
constraint_indexing:constraint_classification(medical_labor_market_monopsony, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_labor_market_monopsony_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_labor_market_monopsony, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_labor_market_monopsony, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_labor_market_monopsony, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_labor_market_monopsony, TR),
    TR >= 0.70.

:- end_tests(medical_labor_market_monopsony_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Hospital consolidation and regional monopsony create genuine wage suppression relative to physician productivity and training costs. The extractiveness is not at snare levels (0.66+) because: (1) some physicians have exit options through geographic mobility, independent practice, or career changes; (2) hospital systems provide genuine coordination value (integrated EMRs, malpractice insurance pooling, administrative overhead reduction); and (3) workforce supply has not collapsed despite suppression (indicating suppression is within the 'bargaining range' of a non-catastrophic extraction). The 20-year trend shows increasing extractiveness (0.38 → 0.58) correlating with hospital consolidation waves, suggesting extractive mechanisms are accumulating faster than countervailing reforms. Suppression (0.65): High. Physicians face multiple structural barriers to exit: (1) student debt (average $200k+) creates financial lock-in; (2) geographic credential non-portability (state licensure) reduces outside options; (3) non-compete agreements restrict independent practice; (4) capital requirements for solo practice are prohibitive; (5) family considerations (spouse employment, children's schooling) create relocation costs. Suppression is enforced by institutional structures rather than physical coercion, but the effect is real. Theater ratio (0.48): Moderate. Hospital justifications for consolidation invoke legitimate coordination benefits (integrated care, reduced administrative overhead, unified compliance systems), and these benefits are partially real. However, the framing increasingly emphasizes administrative integration and IT standardization over genuine care coordination, suggesting theater is rising. The theater_ratio increase from 0.35 to 0.48 reflects the shift toward justifying consolidation through administrative efficiency claims rather than care quality claims.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across four distinct structural positions. Physicians in trapped monopsony regions see pure extraction (Snare). Specialists with geographic mobility see mixed coordination and extraction (Tangled Rope) — they genuinely benefit from hospital infrastructure while also experiencing wage suppression. Hospital systems see coordination (Rope) — the monopsony position is incidental to legitimate service integration. Organized medicine and reform movements see a temporary problem with structural pathways to resolution (Scaffold) — telemedicine, non-compete restrictions, and workforce diversification (NP/PA expansion) are reducing geographic lock-in. Patients and the general public might see a natural law of healthcare (Mountain) — integrated systems require physician employment, and efficiency requires consolidation — but this is a false summit: the institutional design choices (state licensure, consolidation tax incentives, funding models) are contingent, not inevitable. The perspectival gap reveals that different stakeholders experience the same structural phenomenon (hospital consolidation + physician employment) as coordination (hospital perspective), mixed (specialist perspective), extraction (trapped physician perspective), and reform opportunity (organized medicine perspective).
 *
 * DIRECTIONALITY LOGIC:
 *   The monopsony is a multi-agent extraction network. Physicians experience high d (0.85+) as trapped or constrained victims; hospital systems experience low d (0.10-0.20) as beneficiaries with arbitrage options; insurance companies experience intermediate d (0.45-0.55) as partial beneficiaries who are themselves extracted from by hospital consolidation; patients experience maximum d (0.95) as abstract powerless populations with no exit options. The derived directionality values produce perspectival divergence: powerless/trapped physicians classify the constraint as snare (high χ), moderate/constrained specialists classify as tangled_rope (moderate χ), institutional/arbitrage hospitals classify as rope (low/negative χ), and organized reform movements classify as scaffold (low χ with sunset logic). The insurance company perspective is particularly instructive: they appear as beneficiaries at the physician level (monopsony suppresses wages, reducing reimbursement pressure) but as victims at the hospital level (consolidation gives hospitals pricing power). This two-level extraction is captured through separate perspectives with different directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   MONOPSONY CLASSIFICATION TRAP: The primary mandatrophy risk is misclassifying the constraint as Rope (pure coordination) because hospital consolidation has legitimate coordination functions. Many hospital administrators and health policy analysts describe consolidation as necessary for integrated care, reduced administrative overhead, and unified compliance. This framing is partially true — there are real coordination gains — but it obscures the monopsony extraction layer. The tangled_rope classification resolves this by asserting that both the coordination function AND the asymmetric extraction are structurally real. Suppression ≥ 0.65 and beneficiary/victim differentiation confirm this is not pure coordination. The engine's mandatrophy detector will flag the false mountain perspective (analytical observer naturalizing consolidation as inevitable coordination) as a false summit: institutional design choices (regulatory incentives for consolidation, state licensure non-portability, consolidation tax benefits) create the monopsony, and are therefore changeable. The constraint is tangled_rope, not mountain and not rope. SECONDARY RISK: Misclassifying as snare at all perspectives because of the high suppression score (0.65). The snare classification is accurate ONLY for trapped physicians in single-buyer regions; it fails for specialists with multi-market access (tangled_rope), hospital systems (rope), and organized reform movements (scaffold). The perspectival differentiation is essential to prevent monolithic snare classification that would miss the coordination dimensions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monopsony_vs_market_efficiency,
    'Is the physician labor market concentrated due to structural coordination requirements or due to regulatory barriers and consolidation incentives that could be reformed?',
    'Comparative analysis of physician markets with different regulatory regimes (US state variation, international comparison); natural experiment analysis of non-compete enforcement changes and telemedicine policy shifts',
    'If structural: monopsony is inherent and physician wages reflect coordination costs (higher acceptable suppression). If regulatory: monopsony is extractive and policy reform could reduce it (higher mandatrophy risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopsony_vs_market_efficiency, empirical, 'Whether physician labor concentration is structural or regulatory').

omega_variable(
    wage_suppression_magnitude,
    'How much of observed physician wage suppression reflects monopsony extraction vs. legitimate coordination costs (integration, EMR infrastructure, malpractice insurance pooling)?',
    'Comparison of employed vs. independent physician earnings, controlling for practice overhead; analysis of hospital profit margins and wage-setting behavior; econometric decomposition of wage equations',
    'If extraction-driven (>60%): more physicians exit to independent practice, reducing supply. If coordination-driven (<40%): monopsony persists as stable coordination. Critical for extractiveness threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_magnitude, empirical, 'Proportion of wage suppression attributable to extraction vs. coordination').

omega_variable(
    patient_outcome_causation,
    'Does physician wage suppression and monopsony-driven burnout causally reduce patient access and quality outcomes, or are these decoupled?',
    'Regional analysis of monopsony severity (Herfindahl index) vs. patient access metrics (wait times, specialist availability); physician burnout rates vs. patient outcome data; time-series before/after hospital consolidation events',
    'If causal: patient harm is direct consequence of monopsony (victim status legitimate). If decoupled: monopsony is primarily physician extraction (smaller victim population). Classification of patient access as victim changes if decoupled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(patient_outcome_causation, empirical, 'Whether monopsony causally harms patient access and outcomes').

omega_variable(
    non_compete_enforceability,
    'What is the true enforcement rate and financial severity of non-compete agreements in physician employment contracts, and how much do they suppress physician mobility?',
    'Survey data on non-compete terms; analysis of physician exit rates conditional on non-compete presence; state-level variation in non-compete enforceability vs. physician migration patterns',
    'If high enforcement: non-competes are a primary suppression mechanism (support monopsony classification). If low enforcement: physicians have more exit optionality (reclassify from trapped to constrained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_compete_enforceability, empirical, 'Enforcement and mobility impact of non-compete agreements').

omega_variable(
    telemedicine_market_opening,
    'Will telemedicine and remote work normalize in physician practice, reducing geographic lock-in and supporting labor market fragmentation?',
    'Post-COVID longitudinal data on telemedicine adoption rates; employer restrictions on remote practice; regulatory barriers to cross-state telemedicine (licensing); patient demand for remote options vs. in-person care',
    'If telemedicine scales: sunset mechanism activates (scaffold becomes real). If regulatory barriers persist: geographic lock-in remains (monopsony stable). Critical for scaffold classification credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(telemedicine_market_opening, empirical, 'Whether telemedicine will fragment physician labor markets geographically').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_labor_market_monopsony, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medlab_tr_t0, medical_labor_market_monopsony, theater_ratio, 0, 0.35).
narrative_ontology:measurement(medlab_tr_t10, medical_labor_market_monopsony, theater_ratio, 10, 0.42).
narrative_ontology:measurement(medlab_tr_t20, medical_labor_market_monopsony, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(medlab_be_t0, medical_labor_market_monopsony, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(medlab_be_t10, medical_labor_market_monopsony, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(medlab_be_t20, medical_labor_market_monopsony, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_labor_market_monopsony, resource_allocation).
narrative_ontology:affects_constraint(medical_labor_market_monopsony, physician_workforce_supply).
narrative_ontology:affects_constraint(medical_labor_market_monopsony, healthcare_access_rural_regions).
narrative_ontology:affects_constraint(medical_labor_market_monopsony, hospital_consolidation_incentives).

% DUAL FORMULATION NOTE:
% Medical labor monopsony is upstream of physician workforce supply constraints and rural healthcare access gaps. Separate constraint stories address the downstream effects (workforce discouragement, access constraints) which have different ε values reflecting the cumulative impact of wage suppression on supply-side decisions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medical_labor_market_monopsony, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
