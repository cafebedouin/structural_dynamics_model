% ============================================================================
% CONSTRAINT STORY: professional_licensing_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_licensing_extraction, []).

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
 *   constraint_id: professional_licensing_extraction
 *   human_readable: Professional Licensing Extraction and Occupational Gatekeeping
 *   domain: economic/occupational/regulatory
 *
 * SUMMARY:
 *   Professional licensing in regulated occupations (medicine, law,
 *   engineering, cosmetology, electrician work) exhibits a structural tension
 *   between legitimate consumer protection and occupational gatekeeping that
 *   extracts economic rents from aspiring practitioners. Licensing boards,
 *   ostensibly created to protect public safety, are systematically captured
 *   by incumbent professionals who use them to restrict labor supply and
 *   maintain wage premiums. The constraint operates as a tangled rope:
 *   genuine coordination exists (consumers do need reasonable assurance of
 *   practitioner competency) alongside systematic extraction (barriers far
 *   exceed what evidence supports as safety-necessary). The theater ratio
 *   reflects that many licensing requirements—particularly time-served
 *   apprenticeship mandates, continuing education without competency
 *   verification, and interstate reciprocity barriers—persist despite no
 *   correlation with practitioner quality or public safety. Over the 30-year
 *   measurement interval, extractiveness has increased as licensing boards
 *   have layered requirements (rising initial extractiveness from 0.42 to
 *   0.58) and theater has increased as requirements have become more
 *   performative (rising from 0.52 to 0.64). This pattern indicates
 *   regulatory capture accumulation: each new requirement sounds
 *   safety-justified but functions as gatekeeping surplus.
 *
 * KEY AGENTS:
 *   - Aspiring Career-Changers: Primary victims (powerless/trapped) — face cumulative barriers (education cost, exam fees, apprenticeship opportunity cost, credential recognition delays). No exit without massive sunk cost.
 *   - Incumbent Licensed Professionals: Primary beneficiaries (institutional/arbitrage) — capture monopoly rents through scarcity maintenance. Control licensing board membership and regulatory agenda.
 *   - Licensing Boards: Institutional actors (institutional/arbitrage) — formally independent but systematically captured by incumbent professionals through board appointment procedures and revolving-door dynamics.
 *   - Credential Examination Vendors: Secondary beneficiaries (organized/arbitrage) — capture exam administration fees; benefit from requirement inflation (more exams = more revenue). Alignment with incumbent interests.
 *   - Regulatory Reform Coalition: Organized agents (organized/constrained) — evidence-based policymakers, occupational licensing reform advocates, interstate commerce interests. Building alternative credentialing pathways with sunset logic.
 *   - Small-Town Practitioners: Mixed position (moderate/constrained) — benefit locally from monopoly but constrained by board regulatory demands and geographic immobility. Victim status tempered by beneficiary dynamics.
 *   - Consumers: Diffuse beneficiaries (powerless/mobile) — theoretically protected by licensing, but protection exceeds safety requirements, effectively subsidizing incumbent rents through higher service costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_licensing_extraction, 0.58).
domain_priors:suppression_score(professional_licensing_extraction, 0.68).
domain_priors:theater_ratio(professional_licensing_extraction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_licensing_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(professional_licensing_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(professional_licensing_extraction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_licensing_extraction, tangled_rope).
narrative_ontology:human_readable(professional_licensing_extraction, "Professional Licensing Extraction and Occupational Gatekeeping").
narrative_ontology:topic_domain(professional_licensing_extraction, "economic/occupational/regulatory").

domain_priors:requires_active_enforcement(professional_licensing_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_licensing_extraction, incumbent_licensed_professionals).
narrative_ontology:constraint_beneficiary(professional_licensing_extraction, licensing_boards).
narrative_ontology:constraint_beneficiary(professional_licensing_extraction, credential_examination_vendors).
narrative_ontology:constraint_victim(professional_licensing_extraction, aspiring_practitioners).
narrative_ontology:constraint_victim(professional_licensing_extraction, career_changers).
narrative_ontology:constraint_victim(professional_licensing_extraction, lower_income_applicants).
narrative_ontology:constraint_victim(professional_licensing_extraction, occupational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASPIRING CAREER-CHANGER (SNARE) — Faces cumulative barriers: expensive education requirements, lengthy apprenticeships, high-stakes licensing exams, and credential recognition gaps. Each individual barrier is surmountable; their combination creates systematic entrapment. Cannot exit without accepting opportunity cost and debt burden. Licensing requirement itself becomes coercive once incumbent group captures the board.
constraint_indexing:constraint_classification(professional_licensing_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL-TOWN PRACTITIONER (TANGLED ROPE) — Constrained by geographic mobility costs and reliance on local licensing authority. Benefits from licensing (consumer protection, monopoly position within region) while bearing costs (continuing education fees, regulatory compliance overhead, vulnerability to regulatory capture). Asymmetric extraction where local licensing board can selectively enforce requirements.
constraint_indexing:constraint_classification(professional_licensing_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT PROFESSIONAL ASSOCIATION (ROPE) — Sees licensing as pure coordination: standardizing qualification signals, protecting public safety, enabling interstate reciprocity. Benefits from monopoly position and regulatory capture without perceiving extraction—the coordination framing naturalizes gatekeeping. Experiences constraint as legitimate quality control mechanism.
constraint_indexing:constraint_classification(professional_licensing_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized actors (occupational licensing reform advocacy, evidence-based policymakers) perceive licensing regime as temporary over-restriction with visible sunset pathways: mutual recognition agreements, competency-based alternatives to time-served apprenticeships, reciprocal interstate licensing, and outcomes-based qualification. Sees extraction declining as regulatory pressure mounts. Has agency to build alternative certification pathways.
constraint_indexing:constraint_classification(professional_licensing_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LICENSING BOARD APPARATUS (PITON) — Original function was consumer protection through qualification verification. Current function largely performative: exam content drifts from practice competency, continuing education requirements lack evidence base, reciprocity barriers persist despite demonstrated safety records. Maintained through institutional inertia and incumbent capture rather than genuine public safety need. Theater ratio reflects that licensing demonstrates qualification but extraction persists beyond what safety requires.
constraint_indexing:constraint_classification(professional_licensing_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — Risks framing licensing as inherent to professionalism: 'some gatekeeping is necessary to prevent charlatans, therefore occupational licensing is a natural law.' This naturalizes what is contingent institutional choice. However, structural data contradicts mountain classification—the suppression is achievable institutional change (sunset via reform coalition perspective), not irreducible barrier. Engine will flag this as false summit, revealing the naturalization.
constraint_indexing:constraint_classification(professional_licensing_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_licensing_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_licensing_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_licensing_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_licensing_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_licensing_extraction, TR),
    TR >= 0.70.

:- end_tests(professional_licensing_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The licensing regime extracts substantial economic surplus from aspiring practitioners (reduced job access, prolonged earnings suppression during requirement completion, credential exclusion) while producing genuine consumer protection. The ratio of extraction to safety benefit is high but not maximal—some protection is real. Over the interval, extractiveness has increased from 0.42 to 0.58 as boards have added requirements without evidence-based safety justification (residency requirements, additional certifications, reciprocity barriers). This drift indicates capture accumulation. Suppression (0.68): High. Multiple independent barriers create cumulative suppression: high exam pass rates exclude cohorts despite competency (psychometric barriers), apprenticeship duration mandates (time barriers), education cost requirements (capital barriers), interstate non-recognition (geographic barriers), and regulatory discretion in board enforcement (discretionary barriers). Suppression is not total—some practitioners navigate the system—but high enough to meaningfully reduce occupational mobility. Theater ratio (0.64): Moderately high. Continuing education requirements lack mandatory competency verification (performative compliance). Exam content drifts from practice application (assessment theater). Board meeting procedures emphasize stakeholder input from incumbents rather than evidence review. Reciprocity barriers are justified as 'varying state standards' despite evidence that safety outcomes are equivalent across high and low-licensing states.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the beneficiary's pure-coordination framing (Rope—licensing protects consumers) and the victim's extraction experience (Snare—impossible occupational access). The incumbent professional association genuinely experiences licensing as coordination: they see themselves solving the legitimate problem of signaling practitioner competency to consumers. This is not entirely false—licensing does provide some signal. But the signal requirement far exceeds what safety data supports, and the surplus accrues entirely to incumbents. The scaffold perspective (reform coalition) identifies that alternative credentialing mechanisms exist—competency portfolios, performance-based testing, interstate mutual recognition—that could provide equivalent consumer protection at lower suppression cost. This creates a sunset pathway: licensing's extraction mechanisms are not inherent to consumer protection, they are contingent institutional choices that can be replaced with evidence-based alternatives over 10-15 years. The piton perspective observes that licensing boards have become performative: they maintain the ritual of professional self-regulation while actual enforcement has become discretionary and capture-driven. The analytical observer risks seeing licensing as inherent professionalism ('professional fields require gatekeeping'—mountain view), but the structural data reveals this as naturalization of contingent political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to extraction flow. Aspiring career-changers are full victims (high d → high f(d) → experienced extraction) with trapped exit options—no escape without abandoning occupational choice. Incumbent professionals are beneficiaries (low d → low f(d)) with arbitrage exit options—they can leverage licensing monopoly into high wages or shift to other professions. Licensing boards are institutional beneficiaries (low d) despite formal independence—their leadership is systematically drawn from incumbents. Examination vendors are secondary beneficiaries (moderate d, captured beneficiary status). Reform coalition members are constrained organized actors (moderate-high d despite beneficiary nominally being 'public safety') because they face coordinated opposition from entrenched interests. Small-town practitioners have mixed position (d ≈ 0.55): they benefit from monopoly locally but bear compliance costs and board regulatory discretion creates risk. Consumers are theoretically protected beneficiaries but diffusely so—individual consumers cannot perceive or avoid the licensing tax.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope correctly models the actual structure. Licensing is neither pure coordination (rope) nor pure extraction (snare)—it is genuine coordination layered with systematic extraction. The beneficiary's rope perspective is their authentic experience (licensing does protect consumers and enables professional standards). The victim's snare perspective is authentic (barriers are cumulative and quasi-inescapable). The scaffold perspective is authentic (alternative pathways exist and are evidence-based superior). The piton perspective is authentic (boards have become performative). The false summit mountain perspective reveals the naturalization risk—'professions require licensing' sounds inevitable until you examine evidence from low-licensing jurisdictions where safety outcomes are equivalent and accessibility is higher. The tangled rope classification prevents both errors: (1) treating licensing as pure public good (rope), which would excuse unlimited suppression, and (2) treating it as pure extraction (snare), which would overlook genuine consumer protection value. The mandatrophy is resolved by showing that the presheaf of perspectives contains authentic disagreement: these are not different framings of one truth, they are different structural experiences of a hybrid mechanism that genuinely contains both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_safety_benefit_magnitude,
    'How much of occupational licensing''s actual enforcement actually protects public safety versus functioning as pure gatekeeping?',
    'Comparative outcomes analysis: complaint rates, disciplinary actions, and consumer harm between high-regulation and low-regulation jurisdictions for same occupations; measurement of exam-competency correlation',
    'If high safety benefit: licensing exhibits genuine coordination function, reducing extracted surplus from snare level toward tangled rope. If minimal safety benefit: extraction mechanisms are substantially vestigial, raising theater ratio and piton classification probability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_benefit_magnitude, empirical, 'Magnitude of public safety benefit from licensing enforcement').

omega_variable(
    interstate_reciprocity_feasibility,
    'Can mutual recognition agreements among licensing boards reduce suppression without compromising safety outcomes?',
    'Pilot reciprocal licensing regimes; outcome tracking across jurisdictions with high and low mutual recognition; safety incident correlation with reciprocity level',
    'If feasible: scaffold sunset is realistic (5-10 year transition path), justifying organized perspective and reform coalition agency. If not feasible: suppression is inherent to multi-jurisdictional complexity, not pure extraction, reducing tangled rope classification strength.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interstate_reciprocity_feasibility, empirical, 'Feasibility of interstate mutual recognition agreements').

omega_variable(
    credential_signaling_sufficiency,
    'Do performance-based certifications (portfolio review, competency demonstration) provide equivalent consumer protection and labor market signaling as time-served licensing?',
    'Comparative outcomes in jurisdictions adopting outcomes-based certification; consumer satisfaction and safety incident rates; employer hiring patterns and wage outcomes for alternative credentials',
    'If sufficient: alternative pathways exist reducing suppression without safety cost, enabling scaffold sunset. If insufficient: time-serving is irreducible to safe practice, and suppression reflects genuine complexity rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signaling_sufficiency, empirical, 'Whether performance-based certifications provide equivalent signaling').

omega_variable(
    incumbent_capture_reversibility,
    'To what extent is licensing board capture by incumbent professionals structurally inevitable versus contingent on specific governance choices?',
    'Comparative analysis of board composition, appointment procedures, and regulatory outcomes across jurisdictions; identification of governance structures that prevent or enable capture',
    'If largely reversible: governance reform can reduce extraction without licensing elimination, shifting classification toward rope with better enforcement. If inevitable: capture is intrinsic to professional self-regulation model, requiring external regulatory structures to prevent snare dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_reversibility, conceptual, 'Reversibility of incumbent capture in licensing boards').

omega_variable(
    apprenticeship_duration_optimality,
    'How much of mandated apprenticeship/experience duration reflects genuine competency requirements versus pure artificial scarcity creation?',
    'Correlation analysis between apprenticeship duration and occupational accident rates, quality metrics, and consumer satisfaction; comparison with evidence-based competency requirements from other high-stakes occupations',
    'If duration exceeds evidence-based requirement: duration requirements are pure suppression mechanism, raising snare classification. If duration aligns with evidence: suppression reflects genuine safety requirements, reducing net extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apprenticeship_duration_optimality, empirical, 'Alignment of apprenticeship duration with competency requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_licensing_extraction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(proflic_tr_t0, professional_licensing_extraction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(proflic_tr_t15, professional_licensing_extraction, theater_ratio, 15, 0.58).
narrative_ontology:measurement(proflic_tr_t30, professional_licensing_extraction, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(proflic_be_t0, professional_licensing_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(proflic_be_t15, professional_licensing_extraction, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(proflic_be_t30, professional_licensing_extraction, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_licensing_extraction, identity_coordination).
narrative_ontology:boltzmann_floor_override(professional_licensing_extraction, 0.12).
narrative_ontology:affects_constraint(professional_licensing_extraction, occupational_wage_inequality).
narrative_ontology:affects_constraint(professional_licensing_extraction, geographic_labor_mobility_restriction).
narrative_ontology:affects_constraint(professional_licensing_extraction, credential_inflation_cycle).

% DUAL FORMULATION NOTE:
% Professional licensing extraction is upstream of three constraint families: (1) wage inequality between licensed and unlicensed occupations, where licensing functions as artificial scarcity enforcement; (2) geographic immobility where interstate non-recognition creates location lock-in; (3) credential inflation where licensing requirements trigger competitive certification arms races. Each downstream constraint has its own ε value derived from licensing's extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(professional_licensing_extraction, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
