% ============================================================================
% CONSTRAINT STORY: educational_access_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_educational_access_bottleneck, []).

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
 *   constraint_id: educational_access_bottleneck
 *   human_readable: Educational Access Bottleneck
 *   domain: education/economic_mobility
 *
 * SUMMARY:
 *   The educational access bottleneck structures opportunity and mobility
 *   across generations. Credential gatekeeping creates a tension between
 *   genuine coordination (skill certification enabling employment matching)
 *   and asymmetric extraction (credential scarcity enabling rent-seeking by
 *   credential holders and institutions). The constraint exhibits
 *   tangled_rope structure at the systemic level: active enforcement of tier
 *   hierarchies and credential monopolies maintains extraction flows while
 *   providing real coordination benefits (standardized skill signals, quality
 *   assurance). Suppression is high (0.68) because alternatives to
 *   credentialed pathways are structurally blocked: licensing requirements
 *   demand degrees for professional access, employers use credentials as
 *   first-pass filters regardless of actual job requirements, and government
 *   funding for higher education is tiered by prestige. Theater has increased
 *   over the 50-100 year interval as credential inflation has outpaced skill
 *   differentiation — GPA calculations, test scores, and accreditation
 *   rituals proliferate while their actual predictive power for job
 *   performance has stagnated or declined. The constraint's classification
 *   varies dramatically across perspectives: a low-income student trapped by
 *   cost sees a snare with no exit; a community college sees a tangled rope
 *   (coordination with embedded extraction); an elite university sees a rope
 *   (coordination); an alternative credentialing platform sees a scaffold
 *   with a sunset (traditional degree monopoly is being dismantled by
 *   employer acceptance of alternative credentials). The false mountain
 *   perspective reveals the naturalization mechanism: 'credentialing is
 *   inherently difficult' becomes cover for 'we maintain artificial
 *   scarcity.'
 *
 * KEY AGENTS:
 *   - Low-income/first-generation students: Primary victims (powerless/trapped, moderate/constrained) — bear suppression costs of debt, geographic isolation, information gaps, and credential inflation without corresponding benefit increase
 *   - Elite research universities: Primary beneficiaries (institutional/arbitrage) — capture prestige, credential scarcity rents, and selective access advantages
 *   - Credential gatekeepers (professional licensing bodies, accreditation): Institutional beneficiaries (institutional/arbitrage) — maintain enforcement of degree requirements, capture standard-setting rents
 *   - Community college system: Secondary victim/constrained actor (organized/constrained) — provides genuine coordination but subordinated by tier hierarchies and funding disparity
 *   - Alternative credentialing coalition (bootcamps, online platforms, employer-training): Organized agents (organized/mobile) — building exit pathways through credential substitution and sunset mechanisms
 *   - Credential theater bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative machinery (testing, transcripts, accreditation) with declining signal quality; piton classification
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing credential scarcity as inherent to learning rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(educational_access_bottleneck, 0.54).
domain_priors:suppression_score(educational_access_bottleneck, 0.68).
domain_priors:theater_ratio(educational_access_bottleneck, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(educational_access_bottleneck, extractiveness, 0.54).
narrative_ontology:constraint_metric(educational_access_bottleneck, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(educational_access_bottleneck, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(educational_access_bottleneck, tangled_rope).
narrative_ontology:human_readable(educational_access_bottleneck, "Educational Access Bottleneck").
narrative_ontology:topic_domain(educational_access_bottleneck, "education/economic_mobility").

domain_priors:requires_active_enforcement(educational_access_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(educational_access_bottleneck, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(educational_access_bottleneck, incumbent_professionals).
narrative_ontology:constraint_beneficiary(educational_access_bottleneck, elite_institutions).
narrative_ontology:constraint_victim(educational_access_bottleneck, economically_disadvantaged_students).
narrative_ontology:constraint_victim(educational_access_bottleneck, geographic_periphery_populations).
narrative_ontology:constraint_victim(educational_access_bottleneck, intergenerational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME STUDENT (SNARE) — Trapped by tuition costs, collateral requirements, geographic isolation from quality institutions, and information asymmetries about educational pathways. Faces maximum suppression: no alternative credentialing routes, family debt cycles, and credential inflation that makes exit without education impossible. Zero agency within the constraint.
constraint_indexing:constraint_classification(educational_access_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-GENERATION COLLEGE STUDENT (TANGLED ROPE) — Constrained by opportunity costs, family obligations, and social capital deficits, but also benefits from credential acquisition and social network access through higher education. Bears asymmetric extraction (time, debt, social adjustment costs) while gaining genuine coordination benefits (skill development, network access). Moderate agency but significant extraction experienced.
constraint_indexing:constraint_classification(educational_access_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE RESEARCH UNIVERSITY (ROPE) — Benefits from credential scarcity, selective admissions, and prestige extraction. Experiences the constraint as coordination: credential signaling enables employment matching and research collaboration. Net beneficiary with arbitrage options (international recruitment, endowment flexibility, prestige arbitrage across tiers).
constraint_indexing:constraint_classification(educational_access_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMUNITY COLLEGE SYSTEM (TANGLED ROPE) — Organized but constrained by funding caps and credential tier hierarchies. Provides genuine coordination (accessible technical education, remediation, credential pipelines) while bearing extraction: relegated to second-tier status, students face transfer friction, and funding inequity relative to research universities. Active enforcement maintains the tier boundary.
constraint_indexing:constraint_classification(educational_access_bottleneck, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROFESSIONAL LICENSING BODIES (ROPE) — Net beneficiary (arbitrage). Maintain credential scarcity and gate entry to professions. Experience the constraint as legitimate quality control and standardization coordination. Enforcement of licensure requirements is active but perceived as necessary rather than extractive.
constraint_indexing:constraint_classification(educational_access_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALING COALITION (SCAFFOLD) — Organized actors (bootcamps, online platforms, employer-based training) see the traditional bottleneck as a temporary coordination failure with a sunset. Low effective extraction because these actors have agency and exit pathways. Sunset clause: as alternative credentials (AWS certifications, Google Career Certificates, bootcamp attestations) gain employer acceptance, traditional degree monopoly loses force. Estimated sunset: 15-25 years as industry norm-setting accelerates.
constraint_indexing:constraint_classification(educational_access_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CREDENTIAL THEATER BUREAUCRACY (PITON) — Institutional actors (admissions offices, transcript services, accreditation bodies) maintain extensive performative machinery: GPA calculations, standardized test protocols, accreditation rituals. The functional verification (does this person have requisite skills?) has been degraded relative to the theater required (transcripts, test scores, degree titles). Theater ratio high; the system persists through institutional inertia and regulatory lock-in despite declining signal quality.
constraint_indexing:constraint_classification(educational_access_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some credentialing gap is inherent to human knowledge transmission: skills take time to develop, and verification of competence is difficult. This perspective sees the bottleneck as an immutable property of education itself. However, the structural data contradicts this — the engine will compute this as a false summit, revealing that 'inherent to human learning' naturalizes what are actually contingent institutional arrangements (credential monopolies, tier hierarchies, testing theater).
constraint_indexing:constraint_classification(educational_access_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(educational_access_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(educational_access_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(educational_access_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(educational_access_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(educational_access_bottleneck, TR),
    TR >= 0.70.

:- end_tests(educational_access_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): High-moderate. The original research universities and credential gatekeepers capture significant economic and status rents through credential scarcity — credential holders earn 50-100% lifetime wage premiums over non-credential workers. But extractiveness is not at snare levels (0.66+) because education genuinely transfers skills and knowledge; the extraction is overlaid on real coordination value. The 50-year trend shows extractiveness increasing from 0.32 to 0.54 as credential inflation has outpaced skill differentiation — employers now require degrees for jobs that historically required no formal credential, extracting value from pure signaling rather than skill transfer. Suppression (0.68): High. Multiple non-negotiable barriers: tuition and debt requirements create financial suppression; geographic concentration of quality institutions creates access suppression; information asymmetries about alternative pathways create epistemic suppression; licensing requirements for professions create regulatory suppression; tier hierarchies create mobility suppression (community college graduates face persistent transfer friction). The barriers are interconnected and mutually reinforcing. Theater ratio (0.55): Moderate-high and increasing. Standardized testing, GPA metrics, accreditation audits, transcript verification, and degree title hierarchies are extensive performative machinery. The signal quality has degraded as credential inflation has made degrees ubiquitous — the degree no longer meaningfully distinguishes capability, but the testing theater has expanded to compensate, creating credential inflation spirals. Over 50 years, theater has increased from 0.38 to 0.55 as institutions add more signaling machinery without improving actual skill verification.
 *
 * PERSPECTIVAL GAP:
 *   The bottleneck exhibits maximum perspectival divergence. A low-income student sees pure extraction with no escape (snare, maximum d). A community college sees mixed coordination and extraction (tangled_rope, moderate d). An elite university sees legitimate coordination (rope, low d). An alternative credentialing platform sees a temporary problem with clear sunset (scaffold, mobile exit). The credential theater sees its own degraded function (piton, theater justified by inertia). The civilizational observer risks naturalizing artificial scarcity as inherent to learning (false mountain). The perspectival gap reveals that 'the educational access bottleneck' is not a single constraint but a presheaf over multiple structural positions. The same institutional machinery (degree requirements, licensing standards, credential signaling) appears as immutable necessity from one position and removable rent-seeking from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value flows from structural position: beneficiaries (elite universities, licensing bodies, incumbent professionals) have low directionality (d ≈ 0.15-0.25) because they capture extraction benefits and have exit options; victims (trapped low-income students) have high directionality (d ≈ 0.90+) because they bear costs and have no exit. Constrained agents (first-generation students, community colleges) have moderate directionality (d ≈ 0.55-0.65) reflecting mixed costs and benefits. The tangled_rope classification at system level emerges from the combination: genuine coordination (skill certification, quality assurance) exists alongside asymmetric extraction (credential scarcity rents, tier hierarchies, debt structures). Active enforcement maintains both — government accreditation standards enforce quality coordination AND credential monopolies. The piton derives from theater ratio (0.55) indicating performative machinery that persists despite declining signal quality; the bureaucratic machinery is maintained through institutional inertia (admissions offices, testing services, transcript systems) rather than demonstrable function.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH DECOMPOSITION: This constraint exhibits the full six-type range and resolves the mandatrophy by showing that the classification varies legitimately across observer positions. The key insight: the bottleneck is tangled_rope at the system level (coordination function + asymmetric extraction + active enforcement) but appears as snare to powerless agents and rope to beneficiaries. The false mountain at the analytical level (naturalizing credential scarcity as inherent to learning) is detected by the engine as a false summit because the structural data shows the constraint is contingent on institutional arrangements (tier hierarchies, licensing monopolies, degree requirements) that could be dismantled. The theater ratio increase (0.38 to 0.55) reveals Goodhart drift: as credential inflation makes degrees ubiquitous, institutions add testing machinery to maintain signal quality, but the machinery itself becomes the extraction mechanism rather than skill verification. The credentialism identity lock (omega variable) adds a layer: even if material barriers were removed, agents may remain trapped by internalized belief in credential necessity. Alternative pathways represent genuine exits but are suppressed by employer hiring practices and regulatory requirements, not by intrinsic skill verification difficulty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_transfer_observability,
    'How much of the educational access bottleneck reflects genuine difficulty verifying skill transfer versus institutional gatekeeping using verification difficulty as cover?',
    'Comparison of skill outcomes for credentialed vs alternative-path workers; analysis of employer hiring decisions with and without credential signals; correlation between credential content and actual job task requirements',
    'If genuine difficulty dominates: many perspectives shift toward rope/scaffold (coordination problem). If gatekeeping dominates: perspectives shift toward snare/tangled_rope (extraction problem). Classification changes from mountain toward snare at analytical level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transfer_observability, empirical, 'Whether bottleneck reflects genuine skill verification difficulty or institutional gatekeeping').

omega_variable(
    alternative_credential_equivalence,
    'Are alternative credentials (bootcamp, certification, employer-training) genuinely equivalent to traditional degrees in skill transfer and career outcomes, or do they fail at scale?',
    '5-10 year longitudinal tracking of alternative-credential holders vs degree holders; analysis of selection effects and job progression; breakdown of outcome differences by field and employer type',
    'If equivalent at scale: scaffold perspective is structural reality, sunset is real, snare classification fades to tangled_rope over time. If persistent gaps: scaffold is aspirational, snare persists, alternative credentials remain periphery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_equivalence, empirical, 'Whether alternative credentials are equivalent to degrees at scale').

omega_variable(
    debt_cycle_intergenerational_lock,
    'Does educational debt create an intergenerational suppression mechanism (children of indebted graduates face additional barriers) that increases the effective suppression metric beyond the baseline?',
    'Intergenerational analysis: educational attainment and borrowing patterns in families with prior debt; correlation between parent debt levels and child borrowing/access gaps; lifecycle earnings analysis controlling for debt burden',
    'If confirmed: suppression increases from 0.68 to 0.75+; timeline perspective shifts from biographical to generational for victim agents; piton classification becomes untenable (theater cannot persist if it reproduces suppression across generations).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_cycle_intergenerational_lock, empirical, 'Intergenerational suppression mechanism from educational debt cycles').

omega_variable(
    geographic_credential_arbitrage,
    'Does geographic credential arbitrage (rural areas, Global South) represent genuine alternative pathways or exploitation vectors where lower-status credentials become labor market arbitrage tools?',
    'Cross-region analysis of credential acceptance and wage premiums; tracking of geographic credential arbitrage flows and labor market outcomes for arbitraged workers',
    'If arbitrage enables genuine access: geographic scope of snare is local/regional only, not national. If arbitrage exploits: geographic dimension adds extractive layer, pushes piton interpretation, increases theater_ratio (geographic credentialing theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_credential_arbitrage, empirical, 'Whether geographic credential variation enables access or enables exploitation').

omega_variable(
    identity_lock_credentialism,
    'Do individuals internalize credential requirements as markers of personal worth, creating identity-locked suppression beyond material barriers? (Separate from structural debt/access traps.)',
    'Psychological/sociological analysis: self-concept fusion with credential status, belief in credential necessity independent of actual skill requirements, identity crisis post-credential failure',
    'If confirmed: trapped agents should be reclassified as identity_locked in biographical time, changing mountain classification to rope for powerless agents. Suppression is partly internalized, persists after material barriers removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_credentialism, conceptual, 'Identity fusion with credential status as internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(educational_access_bottleneck, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(educ_tr_t0, educational_access_bottleneck, theater_ratio, 0, 0.38).
narrative_ontology:measurement(educ_tr_t25, educational_access_bottleneck, theater_ratio, 25, 0.47).
narrative_ontology:measurement(educ_tr_t50, educational_access_bottleneck, theater_ratio, 50, 0.55).
narrative_ontology:measurement(educ_tr_t75, educational_access_bottleneck, theater_ratio, 75, 0.62).

% Extraction over time
narrative_ontology:measurement(educ_be_t0, educational_access_bottleneck, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(educ_be_t25, educational_access_bottleneck, base_extractiveness, 25, 0.43).
narrative_ontology:measurement(educ_be_t50, educational_access_bottleneck, base_extractiveness, 50, 0.54).
narrative_ontology:measurement(educ_be_t75, educational_access_bottleneck, base_extractiveness, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(educational_access_bottleneck, identity_coordination).
narrative_ontology:boltzmann_floor_override(educational_access_bottleneck, 0.12).
narrative_ontology:affects_constraint(educational_access_bottleneck, professional_licensing_monopoly).
narrative_ontology:affects_constraint(educational_access_bottleneck, intergenerational_wealth_transmission).
narrative_ontology:affects_constraint(educational_access_bottleneck, credential_inflation_spiral).

% DUAL FORMULATION NOTE:
% The educational access bottleneck decomposes into three structurally distinct constraints: (1) credential_skill_transfer (ε=0.25, rope-mountain boundary) — genuine coordination of skill certification; (2) credential_scarcity_rent (ε=0.55, tangled_rope) — extraction through credentialism and tier hierarchies; (3) credential_identity_lock (ε=0.62, snare) — internalized suppression from credentialism. This story covers the system-level tangled_rope. Decomposition enables precise analysis of which interventions address coordination vs extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(educational_access_bottleneck, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
