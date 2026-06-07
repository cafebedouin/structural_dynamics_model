% ============================================================================
% CONSTRAINT STORY: genetic_parenthood_valuation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genetic_parenthood_valuation, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: genetic_parenthood_valuation
 *   human_readable: Genetic Parenthood Valuation in Reproductive Medicine
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The genetic parenthood valuation constraint describes the cultural,
 *   institutional, and economic weight assigned to genetic relatedness in
 *   reproductive decision-making. This constraint operates across multiple
 *   domains: patient preferences in fertility treatment, insurance coverage
 *   structures, legal definitions of parenthood, cultural narratives about
 *   'real' family bonds, and the market structure of the fertility industry.
 *   The constraint exhibits coordination function (genetic information has
 *   genuine medical utility for hereditary disease screening; genetic
 *   relatedness is a legitimate preference for some prospective parents)
 *   layered with extraction (the preference is culturally amplified beyond
 *   its functional basis, creating stigma hierarchies for non-genetic parents
 *   and resource allocation distortions favoring expensive genetic
 *   technologies over adoption or gamete donation). The constraint's
 *   extractiveness has increased modestly over the 30-year interval (0.18 to
 *   0.28) as reproductive technologies have advanced and genetic testing has
 *   become more accessible, creating new markets and intensifying the
 *   cultural emphasis on genetic connection. Theater ratio remains low (0.22)
 *   because the constraint operates through genuine preference expression
 *   rather than performative compliance, though some theater exists in the
 *   fertility industry's marketing narratives. Suppression has increased
 *   (0.25 to 0.35) as the constraint has become more institutionally embedded
 *   through insurance structures, legal frameworks, and professional norms
 *   that privilege genetic family-building pathways.
 *
 * KEY AGENTS:
 *   - Genetic Continuity Seekers: Primary beneficiaries (moderate/mobile) — their preferences are institutionally supported and culturally validated
 *   - Ambivalent Prospective Parents: Mixed position (moderate/constrained) — benefit from coordination but face asymmetric choice architecture
 *   - Stigmatized Non-Genetic Parents: Primary victims (powerless/identity_locked) — adoptive parents, step-parents, gamete recipients face cultural devaluation and legal vulnerability
 *   - Fertility Clinics: Institutional actors (institutional/constrained) — profit from genetic-emphasis services but also provide genuine coordination
 *   - Genetic Testing Industry: Primary beneficiaries (institutional/arbitrage) — the constraint is their market foundation
 *   - Adoption Advocacy Coalition: Organized resistance (organized/mobile) — building alternative pathways with sunset logic
 *   - Analytical Observer: Sees both coordination and extraction (analytical/analytical) — genuine medical utility layered with cultural amplification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genetic_parenthood_valuation, 0.28).
domain_priors:suppression_score(genetic_parenthood_valuation, 0.35).
domain_priors:theater_ratio(genetic_parenthood_valuation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genetic_parenthood_valuation, extractiveness, 0.28).
narrative_ontology:constraint_metric(genetic_parenthood_valuation, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(genetic_parenthood_valuation, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genetic_parenthood_valuation, rope).
narrative_ontology:human_readable(genetic_parenthood_valuation, "Genetic Parenthood Valuation in Reproductive Medicine").
narrative_ontology:topic_domain(genetic_parenthood_valuation, "bioethics/reproductive_medicine/genetic_engineering").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genetic_parenthood_valuation, genetic_continuity_seekers).
narrative_ontology:constraint_beneficiary(genetic_parenthood_valuation, fertility_industry).
narrative_ontology:constraint_beneficiary(genetic_parenthood_valuation, genetic_testing_providers).
narrative_ontology:constraint_victim(genetic_parenthood_valuation, non_genetic_parents).
narrative_ontology:constraint_victim(genetic_parenthood_valuation, adoptive_families).
narrative_ontology:constraint_victim(genetic_parenthood_valuation, gamete_recipients).
narrative_ontology:constraint_vindicates(genetic_parenthood_valuation, genetic_essentialism_doctrine).
narrative_ontology:constraint_vindicates(genetic_parenthood_valuation, biological_primacy_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENETIC CONTINUITY SEEKER (ROPE) — Moderate power, mobile exit. Experiences the constraint as coordination: the fertility industry's emphasis on genetic relatedness aligns with their preferences and provides clear pathways (IVF, GGM, genetic screening). Can exit to adoption or gamete donation if costs become prohibitive. Net beneficiary — the constraint coordinates resources toward their values.
constraint_indexing:constraint_classification(genetic_parenthood_valuation, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: AMBIVALENT PROSPECTIVE PARENT (TANGLED ROPE) — Moderate power, constrained exit. Values both genetic and non-genetic pathways but faces asymmetric institutional support: fertility clinics emphasize genetic options, insurance coverage favors IVF over adoption, cultural narratives privilege biological parenthood. Benefits from the coordination function (clear protocols exist) but bears extraction through constrained choice architecture and financial barriers to alternatives.
constraint_indexing:constraint_classification(genetic_parenthood_valuation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STIGMATIZED NON-GENETIC PARENT (SNARE) — Powerless, identity-locked. Adoptive parents, step-parents, and gamete recipients face persistent cultural devaluation of non-genetic parenthood. Identity is constituted through the parenting role, but the constraint continuously signals that their bond is 'less real' than genetic parenthood. Cannot exit the identity frame without abandoning the parenting relationship. Experiences maximum extraction through social stigma, legal vulnerability (weaker parental rights in some jurisdictions), and internalized hierarchy.
constraint_indexing:constraint_classification(genetic_parenthood_valuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 4: FERTILITY CLINIC (TANGLED ROPE) — Institutional power, constrained exit. Benefits financially from genetic-emphasis services (IVF, PGD, GGM generate higher revenue than adoption referrals) but also provides genuine coordination: helping patients achieve genetic parenthood when medically possible. Constrained by professional norms, insurance structures, and patient demand. Mixed beneficiary-victim: profits from the constraint but also bound by it (cannot easily pivot to non-genetic family-building services without losing market position).
constraint_indexing:constraint_classification(genetic_parenthood_valuation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GENETIC TESTING INDUSTRY (ROPE) — Institutional power, arbitrage exit. Primary beneficiary. The constraint creates sustained demand for ancestry testing, carrier screening, PGD, and embryo selection services. Experiences the constraint as pure coordination: genetic valuation is the market foundation. Can arbitrage across jurisdictions with different regulatory environments. Net beneficiary with minimal extraction.
constraint_indexing:constraint_classification(genetic_parenthood_valuation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ADOPTION ADVOCACY COALITION (SCAFFOLD) — Organized power, mobile exit. Sees the genetic-primacy norm as a temporary cultural artifact being actively dismantled through legal reform (equal parental rights for adoptive parents), cultural campaigns (normalizing diverse family structures), and institutional change (insurance parity for adoption). Sunset logic: as non-genetic family forms gain legal and cultural recognition, the genetic-valuation constraint loses normative force. Estimated sunset: 15-25 years for substantial norm shift in Western contexts.
constraint_indexing:constraint_classification(genetic_parenthood_valuation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Analytical context. The constraint exhibits genuine coordination (genetic information is medically relevant for hereditary conditions; genetic relatedness is a legitimate preference for some) AND asymmetric extraction (the preference is culturally amplified beyond its functional basis, creating stigma hierarchies and resource allocation distortions). The coordination function is real but not sufficient to explain the constraint's intensity — cultural narratives, industry incentives, and identity politics layer extraction onto the coordination substrate.
constraint_indexing:constraint_classification(genetic_parenthood_valuation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genetic_parenthood_valuation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genetic_parenthood_valuation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genetic_parenthood_valuation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(genetic_parenthood_valuation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint extracts through cultural stigma against non-genetic parenthood, asymmetric institutional support (insurance coverage favors IVF over adoption), and industry profit from genetic-emphasis services. However, extraction is not severe — the coordination function is real (genetic information has medical utility; genetic relatedness is a legitimate preference for some), and alternatives exist (adoption, gamete donation, step-parenting). The modest increase over time (0.18 to 0.28) reflects technology-driven market expansion and cultural intensification rather than fundamental structural change. Suppression (0.35): Low-moderate. Barriers to non-genetic parenthood include adoption costs and bureaucracy, legal vulnerability for non-genetic parents in some jurisdictions, cultural narratives that privilege biological bonds, and insurance structures that favor genetic family-building. But suppression is not high — non-genetic pathways are legally available, cultural acceptance is increasing in many contexts, and organized advocacy exists. The increase over time (0.25 to 0.35) reflects institutional embedding of genetic-primacy norms as reproductive technology has matured. Theater ratio (0.22): Low. The constraint operates primarily through genuine preference expression and institutional coordination rather than performative compliance. Some theater exists in fertility industry marketing (genetic connection framed as essential rather than optional) and cultural narratives (biological parenthood as 'natural'), but most of the constraint's operation is functional. The modest increase (0.15 to 0.22) reflects growing gap between marketed genetic essentialism and actual parenting outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon — the weight assigned to genetic relatedness — appears differently from different structural positions. Genetic continuity seekers see coordination (Rope) — the fertility industry aligns with their preferences. Ambivalent prospective parents see mixed coordination and extraction (Tangled Rope) — they benefit from clear protocols but face asymmetric institutional support. Stigmatized non-genetic parents see pure extraction (Snare) — the constraint continuously devalues their bonds. Fertility clinics see mixed coordination and extraction (Tangled Rope) — they profit but are also constrained. The genetic testing industry sees pure coordination (Rope) — genetic valuation is their market foundation. The adoption advocacy coalition sees a temporary problem with a sunset (Scaffold) — cultural norms are shifting. The analytical observer sees the full structure (Tangled Rope) — genuine coordination layered with extraction. The perspectival gap is not about disagreement over facts but about structural position: beneficiaries experience coordination, victims experience extraction, and the analytical observer sees both.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Genetic continuity seekers are declared beneficiaries with mobile exit — they experience low effective extraction (the constraint coordinates resources toward their preferences). The genetic testing industry is a declared beneficiary with arbitrage exit — they experience negative effective extraction (the constraint subsidizes them). Stigmatized non-genetic parents are declared victims with identity_locked exit — they experience high effective extraction (the constraint extracts through stigma and legal vulnerability, and they cannot exit the identity frame without abandoning the parenting relationship). Ambivalent prospective parents are both beneficiaries (coordination function) and victims (constrained choice architecture) with constrained exit — they experience moderate extraction. Fertility clinics are mixed beneficiaries-victims with constrained exit — they profit from the constraint but are also bound by it. The adoption advocacy coalition is organized with mobile exit — they experience low extraction because they have agency and see an exit path (the sunset). The analytical observer sees the full structure: genuine coordination layered with cultural amplification and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that genetic parenthood valuation is neither pure coordination (Rope from all perspectives) nor pure extraction (Snare from all perspectives). The coordination function is real: genetic information has medical utility for hereditary disease screening, and genetic relatedness is a legitimate preference for some prospective parents. But the preference is culturally amplified beyond its functional basis, creating stigma hierarchies for non-genetic parents and resource allocation distortions. The analytical classification (Tangled Rope) captures this hybrid structure: genuine coordination exists, but extraction is layered on top through cultural narratives, industry incentives, and institutional arrangements that privilege genetic family-building pathways. The constraint is not a false summit (it does not claim to be a natural law), but it does exhibit the tangled_rope signature: active enforcement (cultural stigma, insurance structures, legal frameworks), identifiable beneficiaries (genetic continuity seekers, fertility industry), and identifiable victims (non-genetic parents).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_preference_origin,
    'Is the preference for genetic parenthood a human universal (evolutionary adaptation) or a culturally contingent norm amplified by modern reproductive technology?',
    'Cross-cultural anthropological data on parenting norms in societies without access to genetic testing; historical analysis of adoption and fosterage practices; evolutionary psychology evidence on kin recognition and investment',
    'If universal: the constraint is closer to mountain (coordination around an immutable preference). If culturally contingent: the constraint is closer to snare (extraction through manufactured preference).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_preference_origin, empirical, 'Whether genetic parenthood preference is universal or culturally contingent').

omega_variable(
    medical_utility_threshold,
    'What proportion of the genetic information sought in reproductive contexts has genuine medical utility (hereditary disease screening) versus identity/ancestry interest?',
    'Analysis of genetic testing uptake patterns; comparison of actionable medical findings vs non-actionable ancestry information; patient decision-making studies on test result impact',
    'If high medical utility: coordination function is stronger, extraction lower. If low medical utility: the constraint is more extractive (industry profits from non-functional testing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_utility_threshold, empirical, 'Proportion of genetic testing with genuine medical utility').

omega_variable(
    stigma_internalization_mechanism,
    'Do non-genetic parents internalize the genetic-primacy hierarchy, or do they resist it while facing external stigma?',
    'Qualitative interviews with adoptive parents, step-parents, and gamete recipients; longitudinal studies of parental identity formation; comparison of self-reported vs externally-imposed stigma',
    'If internalized: identity_locked classification is accurate and extraction is higher (the constraint operates through self-enforcement). If resisted: constrained classification is more accurate and extraction is lower (external barriers only).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stigma_internalization_mechanism, empirical, 'Whether non-genetic parents internalize genetic-primacy stigma').

omega_variable(
    sunset_plausibility,
    'Is the adoption advocacy coalition''s sunset timeline realistic, or is genetic valuation intensifying with advancing reproductive technology?',
    'Trend analysis of cultural attitudes toward non-genetic parenthood over past 30 years; uptake rates of genetic vs non-genetic family-building pathways; legal reform trajectory; GGM and embryo selection demand curves',
    'If sunset is real: scaffold classification is structural. If genetic valuation is intensifying: the constraint is hardening into tangled_rope or snare from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sunset_plausibility, empirical, 'Whether genetic-primacy norm is weakening or intensifying').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genetic_parenthood_valuation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gen_parent_theater_1990, genetic_parenthood_valuation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gen_parent_theater_2000, genetic_parenthood_valuation, theater_ratio, 10, 0.18).
narrative_ontology:measurement(gen_parent_theater_2010, genetic_parenthood_valuation, theater_ratio, 20, 0.2).
narrative_ontology:measurement(gen_parent_theater_2020, genetic_parenthood_valuation, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(gen_parent_extract_1990, genetic_parenthood_valuation, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gen_parent_extract_2000, genetic_parenthood_valuation, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(gen_parent_extract_2010, genetic_parenthood_valuation, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(gen_parent_extract_2020, genetic_parenthood_valuation, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gen_parent_suppress_1990, genetic_parenthood_valuation, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gen_parent_suppress_2000, genetic_parenthood_valuation, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(gen_parent_suppress_2010, genetic_parenthood_valuation, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(gen_parent_suppress_2020, genetic_parenthood_valuation, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genetic_parenthood_valuation, identity_coordination).
narrative_ontology:affects_constraint(genetic_parenthood_valuation, embryo_selection_ethics).
narrative_ontology:affects_constraint(genetic_parenthood_valuation, gamete_donation_anonymity).
narrative_ontology:affects_constraint(genetic_parenthood_valuation, adoption_legal_framework).

% DUAL FORMULATION NOTE:
% The genetic parenthood valuation constraint is upstream of specific reproductive technology policies (embryo selection, gamete donation, adoption law) but represents a distinct cultural-institutional structure. The downstream constraints have their own extractiveness values reflecting specific policy choices; the genetic parenthood valuation constraint has its own extractiveness reflecting the broader cultural weight assigned to genetic relatedness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
