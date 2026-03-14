% ============================================================================
% CONSTRAINT STORY: professional_pipeline_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_pipeline_bottleneck, []).

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
 *   constraint_id: professional_pipeline_bottleneck
 *   human_readable: Professional Pipeline Bottleneck: Entry-to-Mid-Career Extraction
 *   domain: labor_markets/organizational_dynamics
 *
 * SUMMARY:
 *   The professional pipeline bottleneck describes the structural constraint
 *   that creates asymmetric information and opportunity access between
 *   established professionals and entry/mid-career seekers. This constraint
 *   exhibits simultaneous coordination and extraction: it solves the
 *   legitimate problem of screening for professional capability, but does so
 *   through mechanisms (credential inflation, unpaid internships, geographic
 *   clustering, network gatekeeping) that extract value from aspirants while
 *   concentrating opportunity among credential-holders. The constraint is not
 *   a natural law of labor markets but a contingent institutional arrangement
 *   maintained by beneficiaries with institutional power to enforce
 *   credential requirements, hiring standards, and cultural reproduction
 *   through networks. The theater ratio (0.68) reflects substantial
 *   performativity in gatekeeping rituals — resume screening, behavioral
 *   interviews, credential stacking — that measure signaling ability rather
 *   than job-relevant capability. Alternative pathways (bootcamps,
 *   apprenticeships, portfolio-driven hiring, remote work) are reducing
 *   bottleneck severity at the margins, creating real sunset logic for the
 *   traditional pipeline. The constraint shows its age: as labor market
 *   tightness increases and demographic shifts create talent shortages,
 *   gatekeepers are experimenting with credential relaxation, but these
 *   experiments remain institutional inertia rather than fundamental
 *   restructuring.
 *
 * KEY AGENTS:
 *   - Entry-Level Candidates: Primary victims (powerless/trapped) — face experience paradox, credential inflation, unpaid internship requirements, geographic clustering. Cannot exit without abandoning field aspirations. Maximum suppression.
 *   - Mid-Career Professionals: Secondary victims (moderate/constrained) — benefit from network effects and established position but constrained by sunk costs in credentials, geographic lock-in, and career path dependence. Exit is costly but possible.
 *   - Senior Gatekeepers: Primary beneficiaries (powerful/mobile) — hiring managers, partners, senior leaders who control access to next-level roles. Capture scarcity benefits (higher selection standards, prestige, lower wage pressure) while maintaining discretion over hiring.
 *   - Credential-Issuing Institutions: Beneficiaries (institutional/arbitrage) — universities, professional associations, bootcamps. Benefit from credential inflation, tuition scaling, and bundled services. Have arbitrage options to change credential requirements.
 *   - Credentialing Theater System: Institutional actor (institutional/constrained) — resume formats, interview protocols, credential verification rituals. Maintains low functional value through institutional inertia.
 *   - Alternative Pathway Coalition: Organized agents (organized/constrained) — bootcamp providers, apprenticeship programs, skills-based hiring advocates. Building sunset mechanisms that bypass traditional pipeline.
 *   - Field Knowledge Diversity: Abstract collective victim — brain drain from underrepresented backgrounds, loss of diverse perspectives, reduces field innovation and problem-solving capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_pipeline_bottleneck, 0.52).
domain_priors:suppression_score(professional_pipeline_bottleneck, 0.65).
domain_priors:theater_ratio(professional_pipeline_bottleneck, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_pipeline_bottleneck, extractiveness, 0.52).
narrative_ontology:constraint_metric(professional_pipeline_bottleneck, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(professional_pipeline_bottleneck, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_pipeline_bottleneck, tangled_rope).
narrative_ontology:human_readable(professional_pipeline_bottleneck, "Professional Pipeline Bottleneck: Entry-to-Mid-Career Extraction").
narrative_ontology:topic_domain(professional_pipeline_bottleneck, "labor_markets/organizational_dynamics").

domain_priors:requires_active_enforcement(professional_pipeline_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_pipeline_bottleneck, senior_gatekeepers).
narrative_ontology:constraint_beneficiary(professional_pipeline_bottleneck, established_credential_holders).
narrative_ontology:constraint_victim(professional_pipeline_bottleneck, entry_level_candidates).
narrative_ontology:constraint_victim(professional_pipeline_bottleneck, mid_career_seekers).
narrative_ontology:constraint_victim(professional_pipeline_bottleneck, field_knowledge_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL CANDIDATE (SNARE) — Faces insurmountable barriers: unpaid internship requirements, credential inflation, geographic clustering of opportunities, and experience paradox (need experience to get hired, need job to gain experience). Cannot exit without abandoning field aspirations. Suppression is structural and comprehensive.
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Experiences mixed coordination and extraction. Benefits from network effects and mentorship within the pipeline, but constrained by sunk costs in credentials, geographic lock-in, and penalty for lateral movement. Exit is costly but possible; faces career trajectory damage if switching fields.
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIAL-ISSUING INSTITUTION (ROPE) — Benefits from pipeline bottleneck through tuition scaling, credential inflation, and bundled services (alumni networks, career placement fees). Experiences the constraint as coordination: managing the flow of candidates into labor market. Net beneficiary with arbitrage options; can adapt credential requirements as market pressures shift.
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SENIOR GATEKEEPER (TANGLED ROPE) — Institutional actor (hiring manager, partner, senior partner) who both coordinates talent acquisition AND captures extraction benefits through scarcity. Can exit the constraint by changing hiring practices, but receives sufficient benefit from bottleneck (higher selection standards = prestige, lower wage pressure on early-career tier) that exit is suboptimal. Experiences constraint as beneficial coordination with minor asymmetry.
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING THEATER SYSTEM (PITON) — Rituals and signaling requirements (resume format, interview protocols, credential stacking) persist through institutional inertia despite diminished predictive validity. High theater ratio (0.68) reflects that many gatekeeping functions are performative rather than functional: resume screening, behavioral interviews, and credential verification often measure signaling ability rather than job-relevant capacity. The system maintains these rituals because alternatives haven't fully replaced them and because the rituals serve other institutional purposes (legal defensibility, cultural reproduction).
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE PATHWAY COALITION (SCAFFOLD) — Bootcamps, apprenticeships, skills-based hiring, and portfolio-driven evaluation represent organized attempts to bypass the traditional pipeline. These alternatives have sunset logic: as they demonstrate equivalent or superior outcomes, traditional credential gatekeeping loses legitimacy and force. Suppression declines as pathways mature. Entry barriers for coalition members (career risk, small firm participation) are real but not insurmountable — organized agents have agency and exit options.
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, screening for competence always requires some selection mechanism, and selection mechanisms always create verification bottlenecks — candidates cannot prove capability without prior experience, and employers cannot assess capability without observation periods. This perspective risks naturalizing contingent institutional arrangements (credential inflation, unpaid internships, geographic clustering) as inherent to labor market function. The engine will flag this as a false summit: the specific bottleneck's severity is contingent, not natural.
constraint_indexing:constraint_classification(professional_pipeline_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_pipeline_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_pipeline_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_pipeline_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_pipeline_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_pipeline_bottleneck, TR),
    TR >= 0.70.

:- end_tests(professional_pipeline_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The pipeline bottleneck extracts material value from aspirants (unpaid labor, sunk credential costs, delayed earnings, geographic relocation requirements) and transfers it to gatekeepers and credential institutions. However, extraction is not total — some legitimate screening value exists (employers do need to assess capability), and alternative pathways are emerging that reduce extraction. Suppression (0.65): High. Barriers are substantial: experience paradox (need job to gain experience, need experience to get hired), credential inflation (degree no longer sufficient, now requires certifications + internships + networks), unpaid internship requirements (geographic + financial barriers), and geographic clustering of opportunities. However, suppression is not absolute — alternative pathways exist, and some gatekeepers are experimenting with credential relaxation. Theater ratio (0.68): High. Resume screening, behavioral interviews, and credential verification are substantially performative. Resumes measure formatting and presentation ability, behavioral interviews measure interview-technique practice, and credentials measure perseverance and access to education rather than job-relevant capability. Theater has increased over the interval as credential inflation has outpaced job task complexity, and rituals have become more elaborate to differentiate among credential-equivalent candidates.
 *
 * PERSPECTIVAL GAP:
 *   The pipeline bottleneck demonstrates the gap between how beneficiaries and victims perceive the same constraint. Gatekeepers and credential institutions perceive the pipeline as coordination: solving the problem of matching talent to roles and screening for capability. Victims perceive it as extraction: unnecessary barriers are maintained to reduce competition and preserve insider advantage. Both perceptions contain truth — the constraint is genuinely tangled. The entry-level candidate's snare perspective reveals what the gatekeeper's rope perspective obscures: that the barriers are far more severe than the actual screening problem requires. The alternative pathway perspective reveals that much of the current pipeline's 'necessity' is institutional inertia — bootcamps and portfolio-driven hiring produce comparable or better outcomes at lower extraction cost. The naturalizing (mountain) perspective risks turning contingent institutional arrangements (credential inflation, unpaid internships, geographic clustering) into inevitable features of labor markets.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's position on the extraction gradient is determined by their control over gatekeeping mechanisms and their structural benefit from maintaining barriers. Senior gatekeepers (powerful/mobile) control hiring, training standards, and promotion criteria — they benefit from scarcity (their skills remain valuable, their networks remain valuable) and can opt to maintain or relax barriers based on cost-benefit. Credential institutions (institutional/arbitrage) control credential definitions and can change requirements unilaterally — they benefit from inflation (higher tuition, bundled services) but have arbitrage options. Mid-career professionals (moderate/constrained) benefit from established networks and credentials but are constrained by sunk costs and dependent on barriers staying roughly stable (their credentials don't inflate further). Entry-level candidates (powerless/trapped) bear all costs with no ability to opt out or negotiate: they must acquire credentials, often unpaid, often in expensive locations, often with no guarantee of placement. The directionality gradient is steep and stable.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the professional pipeline is resolved by recognizing that the constraint genuinely coordinates screening (legitimate function) while also extracting value through artificial scarcity mechanisms (illegitimate/unnecessary function). The tangled_rope classification is not a compromise or a 'both/and' fudge — it's a diagnostic recognition that the same institutional structures serve dual purposes. Credential requirements coordinate information about capability, but credential inflation extracts by raising barriers beyond what capability signaling requires. Interview protocols coordinate assessment of interpersonal fit, but behavioral interview theater extracts by privileging interview technique over job performance. Networks coordinate mentorship and knowledge transfer, but network gatekeeping extracts by restricting information access. The mandatrophy is resolved by identifying which components are genuine coordination (they reduce under alternative pathway pressure) and which are extractive (they persist despite proving unnecessary). The measurements show increasing theater ratio and extractiveness over time, indicating that the extraction component is growing relative to the coordination component — the bottleneck is becoming more extractive, not less, as credential inflation accelerates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_inflation_causality,
    'Does credential inflation drive pipeline bottleneck severity, or is it a symptom of deeper labor market mismatch?',
    'Historical analysis of credential requirement drift vs. job task complexity; cross-sector comparison of credential inflation rates vs. bottleneck severity; controlled jurisdictional comparison (sectors with/without credential escalation)',
    'If credential inflation is causal: reducing requirements directly decreases bottleneck (policy target is clear). If symptomatic: credential reduction without addressing underlying mismatch reproduces the bottleneck under new labels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_causality, empirical, 'Whether credential inflation drives or reflects pipeline bottleneck').

omega_variable(
    network_effects_coordination_vs_extraction,
    'Do professional networks coordinate genuinely complementary information-sharing and mentorship, or primarily gate access and extract information asymmetrically from newcomers?',
    'Network analysis of information flow directionality; comparison of outcomes for candidates with vs. without network access, controlling for prior advantage; longitudinal tracking of network composition (does it diversify or concentrate)',
    'If coordination-dominant: networks are pure rope, and bottleneck severity reflects learning/coordination costs. If extraction-dominant: networks are snares, and bottleneck serves gatekeeping rather than efficiency. Classification of mid-career perspective shifts from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_coordination_vs_extraction, empirical, 'Whether professional networks coordinate or extract asymmetrically').

omega_variable(
    unpaid_internship_structural_necessity,
    'Are unpaid internships structurally necessary for entry-level skill development, or are they artificial scarcity mechanisms maintained by employer cost-cutting?',
    'Comparative analysis: paid internship outcomes vs. unpaid internship outcomes for identical role types; longitudinal tracking of intern-to-hire conversion rates and salary trajectories; organizational financial capacity analysis (does employer cost-saving correlate with unpaid internship use)',
    'If structurally necessary: unpaid internships are legitimate screening, and bottleneck reflects knowledge verification costs. If artificial: unpaid internships are pure extraction, and bottleneck serves labor cost arbitrage. Suppression score could be adjusted from 0.65 to 0.80+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unpaid_internship_structural_necessity, empirical, 'Whether unpaid internships are structurally necessary or artificial scarcity mechanisms').

omega_variable(
    geographic_concentration_contingency,
    'Is geographic clustering of professional opportunities (coastal metros, tech hubs, financial centers) inherent to the field''s coordination structure, or is it a contingent path dependency that could be disrupted by remote work and distributed hiring?',
    'Post-pandemic labor market analysis: tracking geographic dispersion of remote-capable roles, salary equalization across regions, candidate flow patterns; comparison with pre-clustering baseline in historical data',
    'If contingent: remote work and distributed hiring are genuine sunset mechanisms for geographic gatekeeping, and bottleneck severity should decline with adoption. If inherent: geographic concentration persists despite nominal flexibility, and remote work adoption does not reduce barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_concentration_contingency, empirical, 'Whether geographic concentration of opportunities is inherent or contingent').

omega_variable(
    identity_lock_in_career_commitment,
    'Do mid-career and early-career professionals experience the pipeline bottleneck primarily as trapped (material barriers) or identity_locked (internalized career framing that makes exit unthinkable)?',
    'Exit pattern analysis: tracking candidates who leave the field vs. those who persist despite suppression signals; post-exit employment quality and satisfaction; narrative analysis of field-leavers'' reasons (barriers vs. identity shifts)',
    'If trapped-dominant: barrier reduction (credentials, internship pay, location flexibility) directly increases exits and increases field diversity. If identity_locked-dominant: barrier reduction is insufficient — agents need identity frame shifting (e.g., ''I can be successful outside this field''). Policy intervention design differs substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_career_commitment, empirical, 'Whether mid-career experience is material entrapment or identity-based lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_pipeline_bottleneck, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ppb_tr_t0, professional_pipeline_bottleneck, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ppb_tr_t10, professional_pipeline_bottleneck, theater_ratio, 10, 0.6).
narrative_ontology:measurement(ppb_tr_t20, professional_pipeline_bottleneck, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ppb_be_t0, professional_pipeline_bottleneck, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ppb_be_t10, professional_pipeline_bottleneck, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ppb_be_t20, professional_pipeline_bottleneck, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_pipeline_bottleneck, information_standard).
narrative_ontology:affects_constraint(professional_pipeline_bottleneck, credential_inflation_spiral).
narrative_ontology:affects_constraint(professional_pipeline_bottleneck, geographic_opportunity_clustering).
narrative_ontology:affects_constraint(professional_pipeline_bottleneck, unpaid_labor_normalization).
narrative_ontology:affects_constraint(professional_pipeline_bottleneck, network_gatekeeping_exclusion).

% DUAL FORMULATION NOTE:
% The professional pipeline bottleneck is upstream of multiple structural constraints in labor markets. Credential inflation, geographic clustering, unpaid internships, and network gatekeeping are each structurally distinct constraints with their own ε values, but all are downstream of and reinforced by the central pipeline bottleneck. Decomposition: pipeline bottleneck (ε=0.52, Tangled Rope) creates demand for credentials, geographic mobility, and network access, which instantiate separate constraints with higher extractiveness. Alternative pathway constraint (ε=0.30, Scaffold) represents the emerging competing pipeline with lower bottleneck severity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
