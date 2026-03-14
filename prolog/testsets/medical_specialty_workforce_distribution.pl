% ============================================================================
% CONSTRAINT STORY: medical_specialty_workforce_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_specialty_workforce_distribution, []).

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
 *   constraint_id: medical_specialty_workforce_distribution
 *   human_readable: Medical Specialty Workforce Distribution Constraint
 *   domain: healthcare/labor_markets/institutional_governance
 *
 * SUMMARY:
 *   The medical specialty workforce distribution constraint governs how
 *   medical trainees are allocated across specialties and geographic regions.
 *   This structural arrangement coordinates medical training infrastructure,
 *   prevents market chaos in residency placement, and maintains professional
 *   standards. Simultaneously, it extracts from primary care workers and
 *   rural/underserved populations through systematic specialty scarcity,
 *   geographic concentration, and prestige hierarchy that reproduces
 *   wealth-based opportunity inequality. The constraint exhibits genuine
 *   coordination (residency matching prevents chaos) and genuine extraction
 *   (specialty supply is restricted, which maintains specialist income and
 *   concentrates care in profitable markets). This is not a simple market
 *   failure — it is a Tangled Rope: institutional coordination that achieves
 *   its goals precisely by extracting from those outside the specialty
 *   pipeline. The theater ratio (0.55) reflects that accreditation and
 *   training governance discourse emphasizes quality assurance and
 *   professional standards, while mechanisms actually enforce supply
 *   restriction and prestige hierarchy. Over the 45-year interval measured
 *   (approximately 1980-2025), extractiveness has increased from 0.38 to
 *   0.54, driven by growing specialty-primary care income gaps, consolidation
 *   of specialist training in major academic centers, and acceleration of
 *   subspecialization reducing primary care prestige. Theater has increased
 *   from 0.42 to 0.58, indicating that quality-assurance justifications have
 *   become more elaborate even as the actual function (scarcity maintenance)
 *   remains constant.
 *
 * KEY AGENTS:
 *   - High-Income Specialists and Specialty Gatekeepers: Primary beneficiaries (institutional/arbitrage) — capture income premiums through supply restriction, prestige hierarchy, and concentrated practice locations. Have full exit option (could enter alternative high-income professions if specialty income dropped).
 *   - Rural and Underserved Populations: Primary victims (powerless/trapped) — geographically immobile, economically constrained, cannot exit medical need. Bear full extraction: pay more for specialist care through travel/emergency services, or receive degraded primary-care-only outcomes.
 *   - Primary Care Workforce: Secondary victim (powerless/trapped or moderate/constrained depending on career stage) — trapped in lower-status, lower-income specialty. Generational trap prevents secondary-generation exit (debt from medical school prevents specialty fellowship investment).
 *   - Medical Trainees from Non-Wealthy Backgrounds: Secondary victim (moderate/constrained) — face barriers to specialty pathway entry through social capital requirements, unpaid research year financing, geographic proximity to training centers.
 *   - Primary Care Advocacy Organizations: Organized secondary victim (organized/constrained) — gained voice but structurally limited exit: specialty prestige and reimbursement structure remain entrenched.
 *   - Medical Education Accreditation Bodies: Institutional actor (institutional/arbitrage) — maintain specialty-scarcity structure through accreditation processes; benefit from status quo institutional arrangement.
 *   - Specialty Training Gatekeepers (Program Directors, Department Chairs): Institutional beneficiaries (institutional/arbitrage) — exercise power to allocate scarce residency positions; extract prestige and mentorship value from training gatekeeping.
 *   - Pharmaceutical and Medical Device Industries: Secondary beneficiaries (institutional/arbitrage) — benefit from specialty concentration in major medical centers where commercial relationships are consolidated; support specialty-prestige hierarchy through industry partnerships and continuing medical education.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_specialty_workforce_distribution, 0.52).
domain_priors:suppression_score(medical_specialty_workforce_distribution, 0.65).
domain_priors:theater_ratio(medical_specialty_workforce_distribution, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_specialty_workforce_distribution, extractiveness, 0.52).
narrative_ontology:constraint_metric(medical_specialty_workforce_distribution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(medical_specialty_workforce_distribution, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_specialty_workforce_distribution, tangled_rope).
narrative_ontology:human_readable(medical_specialty_workforce_distribution, "Medical Specialty Workforce Distribution Constraint").
narrative_ontology:topic_domain(medical_specialty_workforce_distribution, "healthcare/labor_markets/institutional_governance").

domain_priors:requires_active_enforcement(medical_specialty_workforce_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_specialty_workforce_distribution, high_income_specialists).
narrative_ontology:constraint_beneficiary(medical_specialty_workforce_distribution, specialty_training_gatekeepers).
narrative_ontology:constraint_beneficiary(medical_specialty_workforce_distribution, medical_device_and_pharmaceutical_industries).
narrative_ontology:constraint_victim(medical_specialty_workforce_distribution, primary_care_workforce).
narrative_ontology:constraint_victim(medical_specialty_workforce_distribution, rural_and_underserved_populations).
narrative_ontology:constraint_victim(medical_specialty_workforce_distribution, medical_trainees_outside_wealthy_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AND UNDERSERVED POPULATIONS (SNARE) — Trapped without medical specialist access. Geographic immobility (cannot relocate for care), economic constraints (cannot afford travel), and structural medical need (cannot choose health status) create zero exit capacity. Bear full extraction: pay more for equivalent care through travel costs and emergency services, or receive degraded primary-care-only outcomes. Maximum perceived extractiveness.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIMARY CARE WORKFORCE (SNARE) — Trapped in lower-status, lower-income work despite equal or greater cognitive demand than many specialties. Training pathways narrow: fewer primary care residencies exist, and cultural prestige flows toward specialization. Medical trainees from non-wealthy backgrounds face particular barriers (no family network to fund fellowship applications, less access to mentorship in prestigious specialties). Generational trap: earnings differential compounds, making specialty debt financing impossible for trainees from disadvantaged backgrounds.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-INCOME SPECIALISTS (ROPE) — Experience the constraint as pure coordination: medical school restricts specialty positions, which prevents market saturation and maintains specialty income levels. Specialty residency gatekeepers benefit from bottleneck — they have institutional power to allocate scarce positions and extract prestige/mentorship value. Pharmaceutical and device manufacturers benefit: specialists concentrate in major medical centers where commercial relationships are consolidated. Net beneficiary — extraction flows toward this agent. Low experienced chi because they have full arbitrage: if specialty income dropped, they could exit to alternative careers.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICAL TRAINEES FROM MODERATE-INCOME BACKGROUNDS (TANGLED ROPE) — Coordination function genuine: residency matching prevents market chaos and enables training infrastructure. But extraction is real and asymmetric: specialty access depends on family wealth (unpaid research years, away rotations), geography (family proximity to major medical centers), and social capital (mentorship access). Constrained exit: switching to primary care is financially rational but status-penalizing; staying in specialty pipeline requires accumulating debt or family subsidy. Moderate extraction because they have some agency through debt financing and because some specialists do emerge from non-wealthy backgrounds (but at lower rates).
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIMARY CARE ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Organized but constrained. These groups have gained voice (AAFP, AAPA expanding primary care roles) and some agency (residency expansion, rural loan forgiveness programs), but their exit options are limited by entrenched institutional structures (specialty prestige is reinforced by centuries of medical hierarchy, insurance reimbursement favors procedures over evaluation-and-management). They benefit from coordination of training pathways but bear extraction through systematic underfunding of primary care residency positions. Moderate-high extraction because they have organizing capacity but structural barriers to exit remain steep.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MEDICAL EDUCATION ACCREDITATION BODIES (PITON) — Maintain specialty-limited training structures through accreditation processes that enforce scarcity (residency caps, subspecialty requirements). Theater is high: accreditation claims to ensure quality through selective training, but the actual function (quality assurance) is orthogonal to the structural effect (maintaining specialty scarcity and prestige hierarchy). The accreditation system persists through institutional inertia — alternatives (direct skills assessment, competency-based progression) exist but haven't displaced the hierarchy-reinforcing model. Theater ratio reflects that accreditation discourse emphasizes quality while mechanisms enforce quantity restriction.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, specialty income premiums might appear as inevitable market signals reflecting training investment and cognitive demand — the market produces the distribution naturally. However, structural data contradicts the mountain classification: specialty positions are administratively capped (not determined by demand), training pathways are gatekept (not open to all qualified applicants), and prestige is socially constructed through institutional hierarchy (not inherent to the work). The engine will identify this as a false summit — naturalizing what is a contingent institutional constraint.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL VIEW (TANGLED ROPE) — The constraint genuinely coordinates medical training (prevents market chaos, enables infrastructure) AND genuinely extracts (concentrates specialists in profitable urban markets, restricts primary care access in underserved areas, reproduces wealth-based opportunity inequality). Both functions are structural. Extraction is not incidental — it is intrinsic to how the coordination mechanism operates. The constraint achieves its coordination goal precisely by restricting supply and maintaining prestige hierarchy.
constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_specialty_workforce_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_specialty_workforce_distribution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_specialty_workforce_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_specialty_workforce_distribution, TR),
    TR >= 0.70.

:- end_tests(medical_specialty_workforce_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint restricts specialty supply through administrative caps on residency positions, creates prestige hierarchy that devalues primary care, and concentrates specialists in profitable urban markets while limiting rural access. The beneficiaries (high-income specialists) experience this as coordination (prevents market chaos), but the victims experience extraction. Over 45 years, extractiveness has grown from 0.38 to 0.52 as specialty-primary care income gaps have widened and subspecialization has accelerated. Suppression (0.65): High. Barriers to exit include: (1) administrative residency position caps (structural barrier), (2) gatekeeping through subjective prestige-hierarchy judgments in admissions (power barrier), (3) social capital requirements for specialty pathways (network barrier), (4) career-path credentialism that makes switching to primary care status-penalizing (institutional barrier), (5) geographic clustering of specialist training in major centers (infrastructure barrier). For primary care workers and rural populations, suppression is near-total. For trainees from non-wealthy backgrounds, suppression is high but not absolute (some do navigate into specialty pathways). Theater ratio (0.55): Moderate-high. Accreditation discourse emphasizes quality assurance ('we carefully select the most capable trainees'), professional standards ('specialties require rigorous training'), and educational excellence. Yet the actual mechanism is supply restriction and prestige maintenance — the quality-assurance justifications are secondary to the scarcity function. Theater has increased from 0.42 to 0.58 because quality narratives have become more elaborate while scarcity mechanisms remain constant (residency caps have not expanded proportionally with medical school class sizes).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival gap: identical base properties (ε=0.52, suppression=0.65, theater=0.55) generate classifications ranging from Rope (high-income specialists) to Snare (rural populations). This gap is not measurement ambiguity — it is structural: the constraint genuinely coordinates training infrastructure and genuinely extracts from those outside the benefit loop. The gap reveals what the constraint actually is: an institutional arrangement that solves a coordination problem (how to allocate scarce training resources without market chaos) while distributing the costs of the solution asymmetrically (beneficiaries capture prestige/income, victims bear scarcity/access penalties). The prestige hierarchy creates a secondary extraction layer: specialty prestige is socially constructed through accreditation/institutional endorsement, not inherent to the work. Primary care is deskilled narratively ('primary care doesn't require the same intellectual rigor as cardiology') even as primary care's actual cognitive demands are often higher. The geographic concentration of specialists in profitable urban markets is path-dependent (medical centers were built near urban centers decades ago) but is now justified through efficiency narratives. The perspectival gap exposes how institutional constraints use coordination-function legitimacy to mask extraction-function reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: High-income specialists benefit from scarcity restriction (d ≈ 0.15-0.20), have institutional power, and have arbitrage exit options → low d → negative f(d) → they experience the constraint as beneficial coordination. Primary care workers are victims of prestige suppression (d ≈ 0.75-0.85), have moderate-to-powerless power, and face constrained-to-trapped exit options → high d → high f(d) → they experience the constraint as extraction. Rural populations are victims with zero exit (d ≈ 0.95-1.00), are powerless, and are trapped → maximum d → maximum f(d) → they experience maximum extraction. Medical trainees from non-wealthy backgrounds are partly victims (constrained access to specialty pathways through social capital barriers, d ≈ 0.60-0.70) and partly beneficiaries of coordination function → moderate d → moderate f(d) → they experience mixed coordination and extraction. These directionality values are derived from beneficiary/victim declarations: beneficiaries (specialists, gatekeepers, device manufacturers) have low d; victims (primary care, rural, underserved, underprivileged trainees) have high d. The constraint's effective chi varies by perspective because f(d) transforms the base ε through the sigmoid.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL PLURALISM: The constraint is Tangled Rope — it genuinely coordinates AND genuinely extracts, and this is not a contradiction or error in classification. The mandatrophy is resolved by recognizing that different agents have legitimately different classifications because their structural positions within the constraint are different. The high-income specialist sees Rope (they are the beneficiary, and the constraint genuinely solves a coordination problem for training). The primary care worker sees Snare (they are the victim, and the constraint offers no alternative pathway). The accreditation body sees Piton (the formal quality-assurance function is secondary to the scarcity-maintenance mechanism). These are not competing hypotheses about a single objective reality — they are descriptions of different agents' structural positions within the same constraint. The analytical observer who claims the constraint is Mountain ('specialty distribution is natural/market-driven') is advancing a false natural law — this is the false summit the engine's diagnostic should detect. The analytical observer who recognizes Tangled Rope is seeing the constraint's true structure: genuine coordination layered with genuine extraction, inseparable within the current institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specialty_scarcity_vs_quality,
    'Do specialty residency caps primarily serve quality assurance (ensuring selective training of highly capable specialists) or scarcity maintenance (preserving specialty income through supply restriction)?',
    'Comparative analysis: do specialties with higher caps show worse outcomes? Do quality metrics correlate with training selectivity or with supply restriction? Analysis of historical residency expansion events and outcome changes.',
    'If quality-driven: specialty scarcity is legitimate coordination cost, not extraction. If scarcity-driven: specialty scarcity is extraction mechanism disguised as quality assurance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialty_scarcity_vs_quality, empirical, 'Whether specialty caps serve quality or scarcity maintenance').

omega_variable(
    primary_care_selection_demand,
    'Do trainees avoid primary care due to intrinsic preference (specialty work is more intellectually engaging) or due to extracted opportunity inequality (primary care career path is financially and socially penalized)?',
    'Controlled analysis: primary care career satisfaction among trainees from wealthy vs non-wealthy backgrounds; counterfactual income equalization scenarios; international comparison (countries with different specialty-primary care prestige hierarchies); post-debt-forgiveness specialty choice changes.',
    'If intrinsic preference: specialty distribution reflects genuine market signals and is efficient. If extracted inequality: specialty distribution reproduces class barriers and is misallocative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_care_selection_demand, empirical, 'Whether specialty avoidance reflects preference or opportunity inequality').

omega_variable(
    geographic_specialty_concentration_driver,
    'Does specialty concentration in major urban medical centers reflect economic efficiency (where patient population and training infrastructure justify density) or path-dependent institutional clustering with extractive lock-in?',
    'Analysis of specialty distribution across international systems with different institutional histories; correlation between historical training center location and current specialty distribution; identification of path dependencies vs rational market allocation.',
    'If efficient: geographic concentration is justified coordination. If path-dependent: geographic concentration is institutional inertia with extractive rural consequences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_specialty_concentration_driver, empirical, 'Whether geographic concentration is efficient or path-dependent').

omega_variable(
    social_capital_access_determinant,
    'What fraction of the specialty-access inequality is explained by explicit gatekeeping (admissions committees discriminating by background) vs implicit social capital mechanisms (network effects, mentorship access, unpaid-work feasibility)?',
    'Demographic analysis of specialty residency cohorts; longitudinal tracking of mentorship networks; identification of residency program social composition; analysis of programs with explicit diversity initiatives vs baseline programs.',
    'If explicit gatekeeping: discrimination is addressable through policy. If implicit social capital: inequality is deeper (reproduced through informal mechanisms) and requires systemic intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_capital_access_determinant, empirical, 'Explicit vs implicit mechanisms of specialty access inequality').

omega_variable(
    rural_provider_supply_elasticity,
    'If primary care income approached specialty income (through loan forgiveness, incentive payments, or relative prestige increase), would rural provider supply shift? What is the elasticity of rural practice entry with respect to income and prestige parity?',
    'Controlled policy experiments (loan forgiveness programs, income guarantee pilots); analysis of rural provider supply response to existing incentive programs; international comparison of countries with higher primary care prestige/income.',
    'If high elasticity: constraint''s extraction is primary mechanism preventing rural access (policy-addressable). If low elasticity: other structural barriers (infrastructure, isolation, work intensity) dominate (requires different interventions).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rural_provider_supply_elasticity, empirical, 'Rural provider supply response to income/prestige equalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_specialty_workforce_distribution, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medspe_tr_t0, medical_specialty_workforce_distribution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(medspe_tr_t15, medical_specialty_workforce_distribution, theater_ratio, 15, 0.5).
narrative_ontology:measurement(medspe_tr_t30, medical_specialty_workforce_distribution, theater_ratio, 30, 0.55).
narrative_ontology:measurement(medspe_tr_t45, medical_specialty_workforce_distribution, theater_ratio, 45, 0.58).

% Extraction over time
narrative_ontology:measurement(medspe_be_t0, medical_specialty_workforce_distribution, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(medspe_be_t15, medical_specialty_workforce_distribution, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(medspe_be_t30, medical_specialty_workforce_distribution, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(medspe_be_t45, medical_specialty_workforce_distribution, base_extractiveness, 45, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_specialty_workforce_distribution, resource_allocation).
narrative_ontology:affects_constraint(medical_specialty_workforce_distribution, healthcare_geographic_access_inequality).
narrative_ontology:affects_constraint(medical_specialty_workforce_distribution, primary_care_workforce_pipeline).
narrative_ontology:affects_constraint(medical_specialty_workforce_distribution, medical_training_debt_accumulation).

% DUAL FORMULATION NOTE:
% The specialty workforce distribution is upstream of three downstream constraints: geographic access inequality (specialty scarcity concentrates care in wealthy urban areas), primary care pipeline degradation (prestige suppression reduces primary care training slots), and training debt accumulation (specialty fellowships require unpaid research years that create financing barriers for non-wealthy trainees). Each downstream constraint has its own ε value reflecting domain-specific extractiveness; the upstream constraint sets structural conditions enabling their extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medical_specialty_workforce_distribution, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
