% ============================================================================
% CONSTRAINT STORY: colorado_sbe_decentralization_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colorado_sbe_decentralization_friction, []).

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
 *   constraint_id: colorado_sbe_decentralization_friction
 *   human_readable: Colorado SBE Institutional Preservation (Educational Decentralization Friction)
 *   domain: political/regulatory/education
 *
 * SUMMARY:
 *   The Colorado State Board of Education functions as a institutional
 *   gatekeeper for educational legitimacy, creating structural friction
 *   between centralized regulatory authority and decentralized educational
 *   innovation. The SBE maintains gatekeeping power over curriculum approval,
 *   accreditation standards, and transcript legitimacy — conferring or
 *   withholding educational credibility based on conformance to state-defined
 *   standards. This creates a hybrid coordination-extraction dynamic: the SBE
 *   solves a genuine collective action problem (preventing educational
 *   balkanization and ensuring minimum quality standards) while
 *   simultaneously extracting institutional rents through regulatory
 *   authority concentration. Charter school operators, homeschooling
 *   families, and alternative pedagogy providers face SBE-imposed
 *   suppression: they must satisfy state standards even when applying
 *   evidence-based alternatives, must navigate approval processes that
 *   privilege traditional models, and face credentialing friction when
 *   seeking college admission or employer recognition. Traditional district
 *   superintendents benefit from standardization (reduced overhead, shared
 *   resources, predictable funding) but remain constrained by SBE curriculum
 *   mandates. The constraint exhibits all measurable characteristics of a
 *   Tangled Rope: genuine coordination function (standardization), asymmetric
 *   extraction (SBE authority concentration), and active enforcement
 *   (regulatory compliance requirements). The theater ratio (0.58) reflects
 *   that SBE review processes include substantial performative elements —
 *   board meetings emphasize credential compliance over substantive
 *   educational quality assessment. Over the 14-year interval, extractiveness
 *   has risen from 0.38 to 0.52 and theater ratio from 0.42 to 0.58,
 *   suggesting institutional focus has shifted toward legitimacy gatekeeping
 *   and away from educational outcome optimization (theater drift).
 *   Simultaneously, alternative credentialing pathways (competency-based
 *   transcripts, interstate reciprocity agreements, employer-recognized
 *   credentials) are emerging as parallel legitimacy systems, creating a
 *   scaffold structure: organized education reform coalitions perceive SBE
 *   gatekeeping as a temporary institutional bottleneck with a visible sunset
 *   clause as alternative systems mature.
 *
 * KEY AGENTS:
 *   - Colorado State Board of Education: Primary beneficiary (institutional/arbitrage) — concentrates authority over educational legitimacy, controls funding allocation, insulates institutional practices from competitive pressure
 *   - Traditional District Superintendents: Secondary beneficiary (moderate/constrained) — benefit from standardization but constrained by SBE mandates; have significant influence over SBE policy direction
 *   - Charter School Operators: Primary victim (powerless/trapped) — operationally independent but legally subordinate; must satisfy SBE standards despite evidence-based alternatives; no meaningful exit within Colorado
 *   - Homeschooling Families: Secondary victim (powerless/trapped) — legally permitted to homeschool but face SBE assessment and transcript legitimacy requirements; high exit friction
 *   - Alternative Pedagogy Providers: Tertiary victim (moderate/constrained) — constrained by SBE curriculum standards even when applying evidence-based methods; can exit through relocation or by accepting legitimacy deficit
 *   - Education Reform Coalition: Organized agents (organized/mobile) — charter networks, competency-based learning advocates, interstate credentialing consortia creating alternative legitimacy pathways
 *   - Analytical Observer: Civilizational context (analytical/analytical) — assesses whether SBE gatekeeping is necessary quality assurance or rent-extraction dressed as coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, 0.52).
domain_priors:suppression_score(colorado_sbe_decentralization_friction, 0.68).
domain_priors:theater_ratio(colorado_sbe_decentralization_friction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(colorado_sbe_decentralization_friction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colorado_sbe_decentralization_friction, tangled_rope).
narrative_ontology:human_readable(colorado_sbe_decentralization_friction, "Colorado SBE Institutional Preservation (Educational Decentralization Friction)").
narrative_ontology:topic_domain(colorado_sbe_decentralization_friction, "political/regulatory/education").

domain_priors:requires_active_enforcement(colorado_sbe_decentralization_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colorado_sbe_decentralization_friction, state_board_of_education).
narrative_ontology:constraint_beneficiary(colorado_sbe_decentralization_friction, traditional_district_superintendents).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, charter_school_operators).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, homeschooling_families).
narrative_ontology:constraint_victim(colorado_sbe_decentralization_friction, alternative_pedagogy_providers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHARTER SCHOOL OPERATOR (SNARE) — Operationally independent but legally subordinate. Must satisfy SBE curriculum standards, accreditation requirements, and approval processes even when applying evidence-based alternative methods. Exit requires leaving Colorado or accepting permanent educational legitimacy deficit. No meaningful alternative to SBE validation exists within state boundaries. High suppression, high experienced extraction.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HOMESCHOOLING FAMILY (SNARE) — Legally permitted to homeschool but faces SBE-defined assessment requirements, transcript verification standards, and college admissions friction when transcripts lack SBE-recognized credentials. Trapped by Colorado's regulatory framework; exit requires relocation or acceptance of institutional illegitimacy in educational records.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: DISTRICT SUPERINTENDENT (TANGLED ROPE) — Benefits from SBE standardization: shared curricula reduce overhead, common assessment enables comparison, state funding tied to SBE compliance provides predictable resource. But constrained by SBE curriculum mandates, assessment timelines, and regulatory compliance costs. Genuine coordination function (standardization) paired with asymmetric control over local autonomy.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COLORADO STATE BOARD OF EDUCATION (ROPE) — Primary beneficiary. SBE gatekeeping centralizes legitimacy conferral, protecting institutional authority, funding allocation, and career pathways for state-level administrators. Experiences constraint as coordination mechanism: maintaining standardization solves legitimate collective action problem (preventing educational balkanization). Low experienced extraction — constraint subsidizes this agent through authority and resource concentration.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY BOARD MEMBER (PITON) — Long-tenured SBE member sees institutional mission (ensuring educational quality) as largely theater: board meetings focus on credential review and process compliance rather than substantive educational outcomes. SBE's actual function (gatekeeper for educational legitimacy) persists due to bureaucratic inertia and stakeholder entrenchment, not because the review process reliably improves educational quality. Theater ratio elevated by performative compliance rituals.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: EDUCATION REFORM COALITION (SCAFFOLD) — Organized actors (charter networks, competency-based learning advocates, interstate credential reciprocity consortia) perceive SBE gatekeeping as a temporary institutional bottleneck being bypassed. Competency-based transcripts, interstate diploma reciprocity agreements, and employer-recognized credentials create parallel legitimacy pathways. Exit path visible and achievable within 10-15 years as alternative credentialing systems mature. Sunset clause implicit: as alternative legitimacy mechanisms scale, SBE authority naturally depreciates.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (standardization prevents educational fragmentation, enables funding mechanisms) legitimately paired with asymmetric extraction (SBE authority concentrates decision-making, insulates institutional practices from competitive pressure, creates credentialing rents). Not a pure extraction mechanism, but also not pure coordination. The constraint exhibits both functions simultaneously — this is the defining characteristic of tangled rope.
constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colorado_sbe_decentralization_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colorado_sbe_decentralization_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colorado_sbe_decentralization_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colorado_sbe_decentralization_friction, TR),
    TR >= 0.70.

:- end_tests(colorado_sbe_decentralization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The SBE extracts institutional rents through gatekeeping authority — charter operators must seek approval, homeschoolers must satisfy assessment requirements, alternative providers must conform to state standards. The extraction is real but not total because (1) charter schools and homeschoolers can legally operate outside traditional district structures, and (2) alternative credentialing systems are emerging as partial substitutes. The value reflects that extraction is conditional on SBE authority persistence, not structural inevitability. Suppression (0.68): High. Significant barriers exist to competitive legitimacy systems: state law mandates SBE curriculum standards, college admissions systems recognize SBE-traditional transcripts as primary legitimacy signal, employer credential recognition defaults to SBE-recognized education. Exit requires either leaving Colorado or accepting permanent legitimacy deficit. Theater ratio (0.58): Moderate-high. SBE review processes emphasize credential compliance and procedural correctness over substantive educational outcome assessment. Board meetings focus on transcript verification and approval timelines rather than analysis of whether approved curricula actually produce superior learning outcomes. The ratio has increased from 0.42 to 0.58 over the interval, indicating theater drift — institutional focus shifting toward legitimacy gatekeeping and away from outcome optimization.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence between institutional and powerless agents. The SBE sees pure coordination (Rope): gatekeeping solves the collective action problem of preventing educational balkanization. District superintendents see mixed coordination-extraction (Tangled Rope): standardization provides real benefits but SBE authority is constraining. Charter operators and homeschoolers see pure extraction (Snare): trapped by regulatory requirements with no exit. The education reform coalition sees temporary institutional friction (Scaffold): alternative legitimacy systems are building pathways that will bypass SBE authority entirely. The legacy board member sees degraded institutional theater (Piton): the SBE's legitimacy-conferring function persists through inertia rather than demonstrated educational quality gains. The analytical observer sees genuine tangled rope with unresolved mandatrophy: the constraint solves a real coordination problem (preventing credential fraud, enabling resource sharing) while simultaneously extracting institutional rents (concentrating authority, insulating practices from competition). No single classification resolves the tension — the constraint exhibits authentic coordination function paired with authentic extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position within the extraction flow. The SBE (institutional/arbitrage) experiences low or negative d because gatekeeping authority subsidizes this agent — they derive career, budgetary, and institutional power from their gatekeeper role. The district superintendent (moderate/constrained) experiences moderate d because they both benefit (standardization reduces overhead) and pay (constrained by SBE mandates) — their exit options are constrained by political and operational dependencies on state system participation. Charter operators and homeschoolers (powerless/trapped) experience high d because they must satisfy SBE authority with no meaningful exit within Colorado — they are structural targets of extraction. The analytical observer (analytical/analytical) occupies a civilizational context where d reflects the structural question: is SBE authority a necessary quality assurance mechanism (d → low/negative) or rent-extraction disguised as coordination (d → high)?
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy by showing that SBE gatekeeping cannot be classified as pure coordination (Rope) because the structure includes asymmetric authority concentration that benefits the gatekeeper; nor can it be classified as pure extraction (Snare) because the structure includes genuine coordination benefits (standardization, fraud prevention, resource pooling). The mandatrophy is resolved by recognizing the tangled rope classification: the constraint is BOTH a coordination mechanism AND an extraction mechanism simultaneously. The SBE's authority concentration is not incidental to solving the coordination problem — it is the mechanism through which the coordination solution is delivered. But that same authority concentration is also the mechanism through which institutional rents are extracted. The constraint cannot be reformed to pure coordination without losing the regulatory centralization that enables standardization; it cannot be reformed to pure extraction without losing the coordination benefits. The resolution path is not constraint purification but constraint sunset: alternative legitimacy systems (competency-based credentials, interstate reciprocity, employer recognition) are emerging that can provide coordination benefits (fraud prevention, credential verification) without SBE authority concentration (rent extraction). The scaffold perspective shows this pathway: organized coalitions perceive SBE gatekeeping as temporary institutional friction with a visible sunset clause as alternative systems mature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_legitimacy_threshold,
    'At what penetration rate of non-SBE credentialing systems does SBE authority structurally collapse?',
    'Historical tracking of employer acceptance rates for non-traditional credentials, college admissions data for alternative transcript formats, job placement outcomes for charter vs district graduates',
    'If threshold < 30% market penetration: SBE constraint degrades rapidly (scaffold sunset confirmed). If threshold > 60%: alternative legitimacy requires institutional anchoring by something other than market forces, suggesting SBE preservation despite alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_legitimacy_threshold, empirical, 'Critical threshold for alternative credential acceptance').

omega_variable(
    regulatory_capture_extent,
    'To what degree is SBE enforcement shaped by district superintendent interests rather than educational quality outcomes?',
    'Policy analysis: comparison of SBE standards enforcement against empirical educational outcomes; interview data on superintendent influence over SBE decision-making; regulatory pattern analysis (are stricter rules applied to charter vs district schools for equivalent infractions?)',
    'If capture is high: constraint is primarily extractive snare with coordination theater. If capture is low: constraint has genuine mixed coordination-extraction (true tangled rope) and is not primarily a gatekeeping tool.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Degree of district superintendent influence over SBE regulation').

omega_variable(
    decentralization_institutional_maturity,
    'Can decentralized educational credentialing systems achieve comparable quality assurance and fraud prevention without SBE-level centralization?',
    'Comparative analysis of fraud and credential fraud rates in centralized vs decentralized credentialing regimes (e.g., Colorado SBE vs interstate reciprocity consortia); longitudinal tracking of educational outcomes for credentials issued through alternative systems',
    'If decentralization succeeds: SBE authority is contingent institutional preservation, not structural necessity. If decentralization fails: SBE gatekeeping may be legitimate coordination function preventing fraud/balkanization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_institutional_maturity, empirical, 'Quality assurance capacity of decentralized credentialing systems').

omega_variable(
    urban_rural_extraction_asymmetry,
    'Does SBE gatekeeping extract differentially from urban charter networks vs rural homeschooling populations?',
    'Disaggregated analysis of SBE approval timelines, compliance costs, and regulatory burden by geographic region and institution type; cost-of-compliance burden as percentage of per-student revenue',
    'If asymmetric: constraint functions as targeted extraction mechanism against specific populations (snare classification strengthened). If symmetric: suggests non-extractive regulatory burden, supporting tangled rope mixed classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(urban_rural_extraction_asymmetry, empirical, 'Geographic and institutional asymmetry in SBE regulatory burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colorado_sbe_decentralization_friction, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cosbe_tr_t0, colorado_sbe_decentralization_friction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cosbe_tr_t7, colorado_sbe_decentralization_friction, theater_ratio, 7, 0.52).
narrative_ontology:measurement(cosbe_tr_t14, colorado_sbe_decentralization_friction, theater_ratio, 14, 0.58).

% Extraction over time
narrative_ontology:measurement(cosbe_be_t0, colorado_sbe_decentralization_friction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cosbe_be_t7, colorado_sbe_decentralization_friction, base_extractiveness, 7, 0.46).
narrative_ontology:measurement(cosbe_be_t14, colorado_sbe_decentralization_friction, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colorado_sbe_decentralization_friction, information_standard).
narrative_ontology:affects_constraint(colorado_sbe_decentralization_friction, charter_school_regulatory_capture).
narrative_ontology:affects_constraint(colorado_sbe_decentralization_friction, homeschool_credentialing_legitimacy).
narrative_ontology:affects_constraint(colorado_sbe_decentralization_friction, interstate_education_reciprocity).

% DUAL FORMULATION NOTE:
% Colorado SBE institutional preservation comprises two related but distinct constraints: (1) SBE curriculum gatekeeping (extractiveness ≈ 0.52, primarily affects charter operators and alternative providers) and (2) SBE transcript legitimacy conferral (extractiveness ≈ 0.48, primarily affects homeschoolers and employers). Both constraints share the SBE as gatekeeper but have different victim populations and different upstream causes. This story treats them as a unified constraint family because they share a single institutional mechanism (SBE authority) and a single alternative pathway (decentralized credentialing systems). Decomposition into separate stories would be warranted if extractiveness values differed significantly (>0.15) or if resolution mechanisms were structurally independent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colorado_sbe_decentralization_friction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
