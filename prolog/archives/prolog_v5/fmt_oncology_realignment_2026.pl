% ============================================================================
% CONSTRAINT STORY: fmt_oncology_realignment_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fmt_oncology_realignment_2026, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fmt_oncology_realignment_2026
 *   human_readable: FMT Immunotherapy Realignment in Oncology
 *   domain: health/medical/immunotherapy
 *
 * SUMMARY:
 *   The January 2026 Phase I/II trial data announcing FMT oral pills'
 *   efficacy in reducing immunotherapy toxicity and improving cancer response
 *   rates triggers a structural realignment in oncology therapeutics. This
 *   constraint captures the moment when microbiota-based intervention
 *   transitions from exploratory science (2020-2025) into clinical
 *   application, forcing institutional reorganization around three competing
 *   claims: (1) FMT is a complementary coordination mechanism that improves
 *   immunotherapy function (Rope view), (2) FMT is a profit opportunity for
 *   microbiota companies enabled by immunotherapy pharma's intellectual
 *   property constraints (Tangled Rope), and (3) FMT is a biomarker-dependent
 *   intervention that leaves some patients worse off (patients without
 *   responsive microbiota) and extracts value from others (companies
 *   controlling stratification expertise). The constraint exhibits Tangled
 *   Rope structure: genuine coordination function (integration of microbiota
 *   management into oncology) coexists with asymmetric extraction (fecal
 *   microbiota companies capturing market entry, immunotherapy firms losing
 *   pricing control, early-cohort patients bearing trial risk without
 *   guaranteed benefit). Theater ratio is decreasing (0.52 → 0.38) as
 *   clinical validation reduces procedural complexity and diagnostic
 *   uncertainty. Extractiveness is increasing (0.28 → 0.52) as market
 *   opportunity becomes clear and IP boundaries harden around
 *   biomarker-stratified products.
 *
 * KEY AGENTS:
 *   - Fecal Microbiota Companies: Primary beneficiary (institutional/arbitrage) — FDA Breakthrough Therapy status, market entry enabled by 2026 trials, pricing power over donor screening
 *   - Patients Without Responsive Microbiota: Primary victim (powerless/trapped) — bear trial risk and implementation cost without efficacy benefit; no alternative pathway
 *   - Immunotherapy Pharmaceutical Firms: Secondary victim (moderate/constrained) — forced into co-development partnerships, lose pricing leverage on combination therapy, constrained by IP boundaries on biological material
 *   - Early Trial Cohorts: Secondary victim (moderate/constrained) — early adopters bear concentration of adverse event risk during Phase II/III expansion
 *   - Regulatory and Clinical Integration Bodies (NIH, FDA, ASCO, ESMO): Organized actors (organized/constrained) — manage temporary coordination problem of integrating FMT into standards; sunset clause: 3-5 years
 *   - Analytical Observer: Generational view (analytical/analytical) — assesses mechanism uncertainty and market structure evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fmt_oncology_realignment_2026, 0.52).
domain_priors:suppression_score(fmt_oncology_realignment_2026, 0.48).
domain_priors:theater_ratio(fmt_oncology_realignment_2026, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fmt_oncology_realignment_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(fmt_oncology_realignment_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fmt_oncology_realignment_2026, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fmt_oncology_realignment_2026, tangled_rope).
narrative_ontology:human_readable(fmt_oncology_realignment_2026, "FMT Immunotherapy Realignment in Oncology").
narrative_ontology:topic_domain(fmt_oncology_realignment_2026, "health/medical/immunotherapy").

domain_priors:requires_active_enforcement(fmt_oncology_realignment_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fmt_oncology_realignment_2026, fecal_microbiota_companies).
narrative_ontology:constraint_beneficiary(fmt_oncology_realignment_2026, cancer_patients_with_responsive_microbiota).
narrative_ontology:constraint_beneficiary(fmt_oncology_realignment_2026, oncology_research_programs).
narrative_ontology:constraint_victim(fmt_oncology_realignment_2026, immunotherapy_pharma_firms).
narrative_ontology:constraint_victim(fmt_oncology_realignment_2026, patients_without_appropriate_microbiota).
narrative_ontology:constraint_victim(fmt_oncology_realignment_2026, early_trial_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENTS WITHOUT RESPONSIVE MICROBIOTA (SNARE) — Cannot exit the constraint; the 2026 trial data suggests FMT efficacy is microbiota-dependent, meaning some patients lack the biological substrate for benefit. These patients are trapped between immunotherapy toxicity (without FMT) and the cost/complexity of screening for microbiota compatibility (with FMT). No alternative pathway visible. Maximum extraction: they bear the burden of implementation complexity without the benefit.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IMMUNOTHERAPY PHARMA FIRMS (TANGLED ROPE) — Face significant market realignment. FMT combination therapy potentially reduces toxicity-driven treatment discontinuation, which increases patient persistence and ultimately benefits immunotherapy efficacy metrics — coordination function. However, firms are also constrained by intellectual property boundaries (FMT is an open-source biological material) and cannot capture pricing power on the microbiota component. Extract from constrained position: forced into co-development partnerships, publication of efficacy data that enables generic FMT combination, reduced pricing leverage. Exit options exist (focus on toxicity-management pharmaceuticals, biomarker-guided selection) but are costly.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FECAL MICROBIOTA COMPANIES (ROPE) — Primary beneficiary from realignment. The January 2026 trials establish FMT oral pills as a legitimate therapeutic class, enabling market entry and regulatory approval pathways (FDA Fast Track, Breakthrough Therapy status). These firms coordinate the integration of microbiota therapeutics into oncology protocols — a genuine coordination function. Net beneficiary: no extraction flows away from them. Exit options abundant (arbitrage across other indications, geographic expansion, licensing to larger pharma).
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AND CLINICAL INTEGRATION BODIES (SCAFFOLD) — NIH, FDA, ASCO, ESMO face the temporary coordination problem of integrating FMT into standard immunotherapy protocols. This requires clinical pathway updates, insurance coverage harmonization, and training protocols. The constraint is temporary: once FMT integration is operationalized (3-5 year horizon), the coordination function sunsets and complexity migration to routine clinical practice occurs. Suppression is moderate because pathway clarity is being established through 2026 trials. Theater ratio is low: the work is primarily functional integration, not performative compliance.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IMMUNOTHERAPY TOXICITY MANAGEMENT PROTOCOLS (PITON) — The existing protocols for managing immunotherapy toxicity (steroids, immunosuppressants, dose reduction, treatment discontinuation) constitute a degraded ecosystem. The 2026 FMT trials reveal that toxicity management has relied on symptom suppression rather than biological root-cause correction. The protocols persist through institutional inertia — they work well enough, insurance covers them, training exists — but their functional adequacy is lower than realized. Theater ratio is high (0.65+) because much of the current management is reactive ritual rather than addressing microbiota-mediated immune dysregulation. As FMT integration proceeds, these protocols migrate toward supportive care (lower theater) rather than primary management.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational analytical perspective, the 2026 FMT trials represent a moment of scientific realignment where the immunotherapy/microbiota link shifts from exploratory science to clinical intervention. The constraint reflects genuine uncertainty about mechanism (which microbiota features are causal? is the benefit due to short-chain fatty acid production, barrier function restoration, or antigen presentation?), which creates real coordination dependencies (need for biomarker development, need for stratification protocols) alongside genuine extraction (firms capturing value from biomarker patents, therapists capturing value from interpretation expertise). Theater ratio is moderate (0.38): the trials are substantive, but mechanistic speculation will drive unnecessary complexity in early adoption phases.
constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fmt_oncology_realignment_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fmt_oncology_realignment_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fmt_oncology_realignment_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fmt_oncology_realignment_2026, TR),
    TR >= 0.70.

:- end_tests(fmt_oncology_realignment_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderately high and increasing. The 2026 trial data shifts FMT from exploratory (ε~0.15) to applied (ε~0.52) status. Fecal microbiota companies gain extraction capacity through market entry and biomarker control. However, extraction is moderate rather than severe because: (a) FMT is a biological material with weak IP protection (open-source donor pools are feasible), and (b) immunotherapy remains the primary therapeutic (FMT is an adjunct). The increase from 0.28 to 0.52 over the interval reflects hardening of market opportunity and IP positioning. Suppression (0.48): Moderate. Multiple barriers exist: mechanism uncertainty (which microbiota features matter?), responder prevalence unknown (some patients trapped), safety signal surveillance ongoing, insurance coverage policies emerging (2026-2027). Suppression is not total because trials provide substantive efficacy data and pathway clarity is increasing. Theater ratio (0.38, decreasing): Low and declining. The 2026 trials are substantive scientific work with measurable toxicity and response outcomes. Theater is not zero because mechanistic understanding remains incomplete, biomarker validation is ongoing, and clinical integration will involve procedural ritual (pretreatment microbiota screening, donor matching, follow-up monitoring). The decline from 0.52 to 0.38 reflects transition from speculative science (2020-2025, high theater) toward standardized clinical protocol (2026 onwards, lower theater).
 *
 * PERSPECTIVAL GAP:
 *   Five distinct classification outcomes from the same structural data: (1) Patients without responsive microbiota classify as Snare — trapped without alternative, bearing full cost of implementation without benefit. (2) Immunotherapy pharma classify as Tangled Rope — forced coordination partners who lose pricing leverage. (3) Fecal microbiota companies classify as Rope — pure beneficiary from market realignment, solving coordination problem of microbiota integration. (4) Regulatory/clinical bodies classify as Scaffold — temporary coordination with sunset as integration operationalizes. (5) Existing toxicity management protocols classify as Piton — degraded ecosystem persisting through institutional inertia but losing functional primacy. The perspectival gap reflects different structural relationships to the extraction flow: beneficiaries with exit see Rope, victims with exit see Tangled Rope, victims without exit see Snare, temporary coordinators see Scaffold, and degraded incumbents see Piton. This is not ambiguity in the constraint itself — it is the constraint's structure as a multi-agent extraction system where power asymmetries and exit options create genuinely different experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   No overrides required. The structural derivation accurately captures all relationships: beneficiary status (fecal microbiota companies) + arbitrage exit → low d → Rope. Victim status (immunotherapy pharma forced into partnerships) + constrained exit → moderate-high d → Tangled Rope. Victim status (patients without responsive microbiota) + trapped exit → very high d → Snare. Organized agents (regulatory bodies) + constrained exit → scaffold with sunset clause. Incumbent agents (toxicity protocols) with arbitrage exit + degraded theater → Piton. The directionality chain is transparent and requires no post-hoc correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying what 'FMT-immunotherapy realignment' actually is from each agent's structural perspective. The language 'realignment' conflates coordination (beneficial) with extraction (harmful). The mandate is to separate them: (1) Coordination is real — microbiota-immune integration genuinely reduces toxicity and improves efficacy. (2) Extraction is real — market realignment concentrates pricing power and biomarker control. Both occur simultaneously. From the powerless patient's view (Snare), the constraint is pure extraction: they bear implementation cost without benefit. From fecal microbiota companies' view (Rope), it is pure coordination: they solve a genuine problem and capture appropriate return. From immunotherapy pharma's view (Tangled Rope), it is hybrid: they benefit from improved outcomes but lose control of the therapeutic combination. The mandatrophy resolution shows that all six types are legitimate readings of the same event — classification depends entirely on structural position, not on objective facts about FMT. The realignment is not inherently 'good' (Rope) or 'bad' (Snare) — it is both, from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    microbiota_causality_mechanism,
    'Which specific microbiota features (taxa, metabolites, immune phenotypes) are mechanistically causal for immunotherapy toxicity reduction and cancer response improvement?',
    'Longitudinal mechanistic studies: correlate microbiota composition/function pre-treatment with toxicity outcome and response rate; prospective validation of biomarker panels in Phase II/III cohorts; in vitro and mouse models testing causal mechanisms',
    'If single robust pathway identified: enables targeted FMT formulations and biomarker-driven patient selection (reduces patient population trapped by incompatible microbiota). If multiple redundant pathways: FMT works broadly but mechanism remains opaque, increasing theater (ritualistic microbiota screening). If mechanism is largely stochastic: classification shifts from Tangled Rope toward Snare for patients without effective microbiota.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(microbiota_causality_mechanism, empirical, 'Microbiota mechanistic causality for immunotherapy optimization').

omega_variable(
    microbiota_responder_prevalence,
    'What fraction of patients can meaningfully benefit from FMT in combination with immunotherapy? Does prevalence vary by cancer type, immunotherapy class, or patient demographics?',
    'Phase II trial data analysis: stratification by baseline microbiota composition and response outcome; meta-analysis of emerging trials across cancer indications; long-term follow-up cohorts establishing durable responder rates',
    'If >70% responder rate: FMT becomes standard-of-care additive (Rope classification dominates). If 30-50% responder rate: market segmentation and biomarker-driven selection becomes critical (Tangled Rope dominates, with Snare populations remaining). If <30% responder rate: FMT remains niche intervention, extraction dynamics shift toward small-population capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(microbiota_responder_prevalence, empirical, 'Prevalence of FMT-responsive patient populations').

omega_variable(
    intellectual_property_capture,
    'Will FMT be regulated as a drug (with patent-protected biomarker-stratified formulations) or as a biologic (with generic/open-source donor material pools)?',
    'FDA/EMA regulatory guidance documents; court challenges to patent claims on FMT formulations; emergence of donor screening standards (open vs proprietary); insurance coverage policy decisions',
    'If drug-like IP framework: fecal microbiota companies gain pricing power and extraction capacity (Snare for patients; Rope benefits firms). If biologic open-source framework: FMT becomes commodity input, immunotherapy pharma faces stronger constraints (Tangled Rope compression; fecal companies see reduced arbitrage). If hybrid (stratified formulations with open-source donor pools): Tangled Rope remains stable with moderate extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_property_capture, conceptual, 'IP and regulatory framework for FMT in oncology').

omega_variable(
    insurance_coverage_adoption_timeline,
    'How quickly will insurance systems worldwide adopt coverage for FMT-based immunotherapy combination therapy? Will coverage be conditional on biomarker testing?',
    'CMS coverage policy decisions; international health system policy documents (NICE, HAS, G-BA); claims database analysis of FMT utilization rates post-policy; cost-effectiveness studies driving coverage decisions',
    'If rapid adoption (2026-2027): scaffold sunset accelerates, FMT integration becomes routine clinical practice, extraction dynamics stabilize. If slow/conditional adoption (2027-2029): complexity persists, theater ratio remains high due to authorization/appeal rituals, patients without coverage access face trapped status (Snare expansion). If coverage denied in some jurisdictions: geographic fragmentation creates international IP extraction opportunities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_coverage_adoption_timeline, empirical, 'Insurance coverage adoption timeline for FMT in oncology').

omega_variable(
    adverse_event_signal_detection,
    'Will post-marketing surveillance identify serious adverse events associated with FMT in immunotherapy cohorts? Specifically, will FMT increase opportunistic infection risk or alter immune checkpoint dynamics in unexpected ways?',
    'FDA post-marketing surveillance reports; electronic health record signal detection algorithms; international pharmacovigilance databases; long-term follow-up cohorts tracking infection rates and immune safety',
    'If significant safety signal emerges: FMT classification could degrade from Rope/Tangled Rope toward Snare (extraction through enforced risk-benefit calculations). If safety profile remains favorable: Rope/Tangled Rope classifications stabilize and scaffold sunset proceeds on schedule. Safety uncertainty is currently a major suppression driver.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adverse_event_signal_detection, empirical, 'Post-market adverse event signal for FMT-immunotherapy combination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fmt_oncology_realignment_2026, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmt_tr_t0, fmt_oncology_realignment_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(fmt_tr_t2, fmt_oncology_realignment_2026, theater_ratio, 2, 0.45).
narrative_ontology:measurement(fmt_tr_t4, fmt_oncology_realignment_2026, theater_ratio, 4, 0.38).

% Extraction over time
narrative_ontology:measurement(fmt_be_t0, fmt_oncology_realignment_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fmt_be_t2, fmt_oncology_realignment_2026, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(fmt_be_t4, fmt_oncology_realignment_2026, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fmt_oncology_realignment_2026, resource_allocation).
narrative_ontology:affects_constraint(fmt_oncology_realignment_2026, immunotherapy_toxicity_market).
narrative_ontology:affects_constraint(fmt_oncology_realignment_2026, microbiota_biomarker_stratification).
narrative_ontology:affects_constraint(fmt_oncology_realignment_2026, cancer_treatment_combination_protocols).

% DUAL FORMULATION NOTE:
% FMT-oncology realignment is downstream of microbiota mechanistic discoveries (2015-2025) and dependent on immunotherapy efficacy establishment (2010-2025). The constraint represents the moment when those upstream claims crystallize into clinical intervention and market structure. Two sibling constraints should be decomposed if mechanistic uncertainty or safety signals produce fundamentally different ε profiles: (a) FMT efficacy in toxicity reduction (current ε=0.52, Tangled Rope), and (b) FMT safety profile in immunotherapy cohorts (ε currently ~0.35, but could shift to 0.60+ if serious adverse events emerge). Current story combines these; if safety signal emerges, decompose into separate stories with affects_constraints link.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
